;;; sgf-mode.el --- SGF Major Mode  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: SGF, go, game

;;; Code:

(require 'sgf-io)
(require 'sgf-svg)

;; Linked Node Object
(defun sgf-linked-node (prev-node &optional current-node next-nodes)
  "Define node object. It is doubly linked list."
  (vector prev-node current-node next-nodes))

;; Game State
(defun sgf-game-state (linked-node
                       board-2d
                       move-num
                       &optional ko
                       prisoners prisoner-counts
                       undo-stack redo-stack)
  "Define game state object. The move number, board-2d, ko, prisoners are re-computed every time when traversing the moves."
  (vector linked-node                 ; 0 current node
          board-2d                    ; 1
          move-num                    ; 2
          ko                          ; 3 ko position
          prisoners                   ; 4 a list of cons (x . y) after current move
          (if (null prisoner-counts)
              '(0 . 0)
            prisoner-counts) ; 5 accumulated number (b . w)
          undo-stack                  ; 6
          redo-stack))


(defun sgf-create-board-2d (w h)
  "Create a empty 2D board of size WxH."
  (let ((board-2d (make-vector h nil)))
    (dotimes (i h) ;; for each row
      (aset board-2d i (make-vector w 'E)))
    board-2d))


(defun sgf-board-2d-get (xy board-2d)
  (if (consp xy)
      (aref (aref board-2d (cdr xy)) (car xy))))


(defun sgf-board-2d-set (xy v board-2d)
  "Do nothing if xy is nil"
  (if (consp xy)
      (aset (aref board-2d (cdr xy)) (car xy) v)))


(defun sgf-board-hoshi (w h)
  "Return a list of hoshi positions on a board of size WxH."
  (append
   ;; center position
   (if (and (= (logand w 1) 1) (= (logand h 1) 1)) (list (cons (/ (1- w) 2)  (/ (1- h) 2))))
   ;; 4 corners
   (if (and (> w 12) (> h 12))
       (list (cons 3       3)
             (cons (- w 4) 3)
             (cons 3       (- h 4))
             (cons (- w 4) (- h 4))))
   ;; 4 sides
   (if (and (> w 18) (> h 18) (= (logand h 1) 1))
       (list (cons 3       (/ (1- h) 2))
             (cons (- w 4) (/ (1- h) 2))))
   (if (and (> w 18) (> h 18) (= (logand w 1) 1))
       (list (cons (/ (1- w) 2) 3)
             (cons (/ (1- w) 2) (- h 4))))))

(defun sgf-neighbors-xy (xy board-2d)
  "Return a list of neighboring positions of XY on a board of size WxH.

Return nil if xy is nil."
  (when (consp xy)
    (let* ((x (car xy)) (y (cdr xy))
           (w (length (aref board-2d 0)))
           (h (length board-2d))
           (neighbors '(((-1 . 0) . left)
                        ((1 . 0) . right)
                        ((0 . -1) . above)
                        ((0 . 1) . below))))
      (mapcar (lambda (offset)
                (let ((dx (car (car offset)))
                      (dy (cdr (car offset))))
                  (when (and (>= (+ x dx) 0) (< (+ x dx) w)
                             (>= (+ y dy) 0) (< (+ y dy) h))
                    (cons (+ x dx) (+ y dy)))))
              neighbors))))

(defun sgf-neighbors (xy board-2d)
  "Return a list of neighbors of XY on a board of size WxH."
  (mapcar (lambda (pos)
            (sgf-board-2d-get pos board-2d))
          (sgf-neighbors-xy xy board-2d)))

(defun sgf-check-alive (xy board-2d &optional last-color visited)
  "Check if the stone on position XY of BOARD-2D is alive.

It returns a cons cell of the form (LIBERTY-FOUND . VISITED). Its car
indicates if a liberty is found, and its cdr is a list of visited
positions to avoid loops, which also stores all the dead positions if no
liberty is found."
  (let ((curr-color (sgf-board-2d-get xy board-2d)))
    (cond
     ((null xy) (cons t nil))
     ((equal curr-color 'E) (cons t visited))
     ((member xy visited) (cons nil visited))
     ((or (null last-color) (equal last-color curr-color))
      (setq visited (cons xy visited))
      (let ((liberty-found nil))
        (dolist (neighbor (sgf-neighbors-xy xy board-2d) (cons liberty-found visited))
          (when neighbor
            (let ((result (sgf-check-alive neighbor board-2d curr-color visited)))
              (if (car result)
                  (setq liberty-found t)
                (setq visited (cdr result))))))))
     (t (cons nil visited)))))


(defun sgf-capture-stones (xy board-2d)
  "Capture the opponent stone after the play of position XY.
Return the list of positions for the prisoners captured.
If XY is nil, it returns nil."
  (let ((curr-color (sgf-board-2d-get xy board-2d))
        (prisoners nil))
    (dolist (neighbor-xy (sgf-neighbors-xy xy board-2d))
      (when (and neighbor-xy
                 (not (member neighbor-xy prisoners))
                 (not (equal curr-color (sgf-board-2d-get neighbor-xy board-2d))))
        (let* ((results (sgf-check-alive neighbor-xy board-2d))
               (alive-p (car results)))
          (unless alive-p
            (setq prisoners (nconc (cdr results) prisoners))))))
    prisoners))


(defun sgf-valid-move-p (stone xy board-2d)
  "Check if a move of STONE at XY on BOARD-2D is alive.

1. Returns a list of captured positions if it captures opponent's stone;
2. Returns t if it does not capture stones and it has liberty;
3. Returns nil otherwise"
  (sgf-board-2d-set xy stone board-2d)
  (let ((prisoners (sgf-capture-stones xy board-2d)))
    (if (null prisoners)
        (let ((alive (car (sgf-check-alive xy board-2d))))
          (if alive t ; case 2
            (progn (message "Illegal move of %S at position %S" stone xy)
                   (sgf-board-2d-set xy 'E board-2d)
                   nil))) ; case 3
      prisoners))) ; case 1



;; (defun sgf-process-play (node)
;;   "Process a play node.
;; If 'B is present in the node, return (B) or (B . xy) depending on the presence of value.
;; If 'W is present in the node, return (W) or (W . xy) similarly.
;; Returns nil if neither 'B nor 'W is present."
;;   (cond
;;    ((assoc 'B node) (cons 'B (car (alist-get 'B node))))
;;    ((assoc 'W node) (cons 'W (car (alist-get 'W node))))
;;    (t nil)))  ;; Return nil if neither 'B nor 'W is found

(defun sgf-process-play (node)
  "Process a play node.
If 'B or 'W is present in the node, return (B x . y) or (W x . y).
If 'B or 'W exists without coordinates, return (B) or (W).
If neither 'B nor 'W is present, return nil."
  (pcase (or (assoc 'B node) (assoc 'W node))
    (`(,stone (,x . ,y)) (cons stone (cons x y)))    ;; Extract (B/W (x . y)) case
    (`(,stone) (list stone))                  ;; Handle (B) or (W)
    (_ nil)))                                 ;; If nothing found, return nil

(defun sgf-show-comment (node)
  "Show the comment of the node."
  ;; if 'C' does not exist, it shows empty str.
  (message (mapconcat 'identity (alist-get 'C node) " ")))

(defun nested-level-of-first (lst)
  "Return the nesting level of the first element in the list LST.
(nested-level-of-first '(a b (c d))) => 0
(nested-level-of-first '((a b) c d)) => 1"
  (if (listp (car lst))
      (1+ (nested-level-of-first (car lst)))  ;; If the first element is a list, go deeper.
    0))

(defun sgf-linkup-nodes-in-game-tree (game-tree head-lnode)
  "Link up nodes in a branch of the game tree."
  (let* ((prev-lnode head-lnode)
         curr-lnode nested-level)
    (dolist (i game-tree)
      (setq nested-level (nested-level-of-first i))
      (cond ((= nested-level 2) ;; this is a fork
             (sgf-linkup-nodes-in-game-tree i prev-lnode))
            ((= nested-level 1) ;; this is a node
             ;; create new node
             (setq curr-lnode (sgf-linked-node prev-lnode i))
             ;; link prev-node to the new node
             (aset prev-lnode 2 (append (aref prev-lnode 2) (list curr-lnode)))
             ;; this line reversed the order of branches
             ;; (aset prev-lnode 2 (cons curr-lnode (aref prev-lnode 2)))
             (setq prev-lnode curr-lnode))))))



(defun sgf-add-captured-stones (xys stone board-2d svg)
  "Add captured stones to the board. STONE is the stone opponent to the captured stone(s)"
  (dolist (xy xys)
    (let ((x (car xy)) (y (cdr xy)))
      (aset (aref board-2d y) x stone)
      ;; change mvnum color to contrast the stone color
      (dom-set-attribute (car (dom-by-id svg (sgf-svg-mvnum-id x y)))
                         'fill (sgf-svg-set-color stone t))
      (dom-remove-attribute (car (dom-by-id svg (sgf-svg-stone-id x y))) 'visibility))))


(defun sgf-del-captured-stones (xys stone board-2d svg)
  "Delete captured stones from the board."
  (dolist (xy xys)
    (let ((x (car xy)) (y (cdr xy)))
      (aset (aref board-2d y) x 'E)
      ;; change mvnum color to the same with deleted stone
      (dom-set-attribute (car (dom-by-id svg (sgf-svg-mvnum-id x y)))
                         'fill (sgf-svg-set-color stone))
      (dom-set-attribute (car (dom-by-id svg (sgf-svg-stone-id x y))) 'visibility "hidden"))))


(defun sgf-get-overlay ()
  "Return the main overlay used in the current buffer."
  (let ((ovs (overlays-in (point-min) (point-max)))
        sgf-ov )
     (while (and ovs (not sgf-ov))
      (let ((ov (pop ovs)))
        (if (overlay-get ov 'sgf-overlay)
          (setq sgf-ov ov))))
    sgf-ov))


(defun sgf-update-overlay (game-state &optional svg hot-areas)
  "Update the overlay with new SVG and GAME-STATE.
Optionally update HOT-AREAS as well."
  (let ((ov (sgf-get-overlay)))
    (overlay-put ov 'game-state game-state)
    (if svg
        (overlay-put ov 'svg svg)
      (setq svg (overlay-get ov 'svg)))
    (if hot-areas
        (overlay-put ov 'hot-areas hot-areas)
      (setq hot-areas (overlay-get ov 'hot-areas)))
    (overlay-put ov 'display (svg-image svg :map hot-areas))))

;; todo
(defmacro sgf-with-safe-update (game-state svg &rest body)
  "Safely execute BODY with rollback on error for GAME-STATE."
  (declare (debug t) (indent 2))
  `(let ((clone-game-state (copy-sequence ,game-state))
         (clone-svg (copy-sequence ,svg)))  ;; Clone for rollback
     (condition-case err
         (progn ,@body
                (sgf-update-overlay ,game-state ,svg))
       (error
        (message "Error: %s. Rolling back game state." err)
        (sgf-update-overlay clone-game-state clone-svg)))))  ;; Roll back to cloned state


(defun sgf-branch-selection (n &optional branch)
  "Prompt the user to select a branch by reading a character."
  (let ((prompt (format "Select a branch (a-%c): " (+ ?a (1- n)))))
    (if (null branch)
        (setq branch (if (= n 1) 0 (- (read-char prompt) ?a))))
    (if (and (>= branch 0) (< branch n))
        branch
      (error "Invalid branch selection: %c" (+ branch ?a)))))

;; Navigation Functions
(defun sgf-forward-node (&optional branch)
  "Move to the next node in the game tree and update board."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (svg         (overlay-get ov 'svg))
         (interval    (car (overlay-get ov 'svg-params)))
         (game-state  (overlay-get ov 'game-state))
         (curr-lnode  (aref game-state 0))
         (board-2d    (aref game-state 1))
         (mvnum       (aref game-state 2))
         (next-lnodes (aref curr-lnode 2))
         (n           (length next-lnodes))
         (pcounts     (aref game-state 5))
         next-lnode next-node play xy x y color prisoners)
    (if (= n 0)
        (progn (message "No more next play.") nil)
      (progn
        (setq branch (sgf-branch-selection n branch))
        (setq next-lnode (nth branch next-lnodes))
        (setq next-node (aref next-lnode 1))
        (setq play (sgf-process-play next-node))
        (setq stone (car play) xy (cdr play) x (car xy) y (cdr xy))
        (setq mvnum (1+ mvnum))
        (sgf-with-safe-update game-state svg
          (aset game-state 0 next-lnode)
          (aset game-state 2 mvnum) ;; Update move number
          ;; Update board and game state
          (sgf-board-2d-set xy stone board-2d)
          (setq prisoners (sgf-capture-stones xy board-2d))
          (aset game-state 4 prisoners)
          ;; Remove captured stones and update prisoner counts
          (sgf-del-captured-stones prisoners stone board-2d svg)

          (if prisoners
              (if (equal stone 'B)
                  (setcar pcounts (+ (length prisoners) (car pcounts)))
                (setcdr pcounts (+ (length prisoners) (cdr pcounts)))))
          (sgf-show-comment next-node)
          ;; Update move on svg
          (when (consp xy) ; xy could be nil (a pass)
            (sgf-svg-add-stone svg interval x y (symbol-name stone))
            (sgf-svg-add-mvnum svg interval x y mvnum (sgf-svg-set-color stone)))
          (sgf-svg-update-prisoners svg pcounts)
          (sgf-svg-update-turn svg stone)
          (sgf-svg-update-mvnum svg mvnum)
          (sgf-svg-update-next svg interval next-lnode)
          (sgf-svg-update-marks svg interval next-node board-2d)
          ;; (message "Put stone at %d %d" (1+ x) (1+ y))
          )))))


(defun sgf-backward-node ()
  "Move to the previous node in the game tree and update board."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (svg        (overlay-get ov 'svg))
         (game-state (overlay-get ov 'game-state))
         (interval    (car (overlay-get ov 'svg-params)))
         (curr-lnode (aref game-state 0))
         (board-2d   (aref game-state 1))
         (mvnum      (aref game-state 2))
         (prev-lnode (aref curr-lnode 0))
         (pcounts     (aref game-state 5))
         play xy x y curr-prisoners prev-prisoners)
    (if (null prev-lnode)
        (progn (message "No more previous play.") nil)
      (sgf-with-safe-update game-state svg
        (aset game-state 2 (1- mvnum)) ;; Update move number
        (setq play (sgf-process-play (aref curr-lnode 1)))
        (setq stone (car play) xy (cdr play) x (car xy) y (cdr xy))

        (svg-remove svg (sgf-svg-stone-id x y))
        (svg-remove svg (sgf-svg-mvnum-id x y))
        ;; revert changes from current move on board
        (sgf-board-2d-set xy 'E board-2d)
        (setq curr-prisoners (aref game-state 4))
        (sgf-add-captured-stones curr-prisoners stone board-2d svg)
        ;; apply changes from prev move
        (setq play (sgf-process-play (aref prev-lnode 1)))
        (setq xy (cdr play) x (car xy) y (cdr xy))
        ;; run capture
        (setq prev-prisoners (sgf-capture-stones xy board-2d))
        (aset game-state 4 prev-prisoners)
        ;; Remove captured stones and update prisoner counts
        (sgf-del-captured-stones prev-prisoners stone board-2d svg)
        (if (equal stone 'B)
            (setcar pcounts (- (+ (length prev-prisoners) (car pcounts))
                               (length curr-prisoners)))
          (setcdr pcounts (- (+ (length prev-prisoners) (cdr pcounts))
                             (length curr-prisoners))))

        (aset game-state 0 prev-lnode)
        ;; Update move on svg

        (sgf-svg-update-prisoners svg pcounts)
        (sgf-svg-update-turn svg (if (equal stone 'B) 'W 'B))
        (sgf-svg-update-mvnum svg mvnum)
        (sgf-svg-update-next svg interval prev-lnode)
        (sgf-svg-update-marks svg interval (aref prev-lnode 1) board-2d)
        ;; overlay and SVG update
        (sgf-update-overlay game-state svg)))))


(defun sgf-first-node ()
  "Move to the first node in the game tree."
  (interactive)
  (while (sgf-backward-node)))


(defun sgf-last-node ()
  "Move to the last node in the game tree."
  (interactive)
  (while (sgf-forward-node)))


(defun sgf-goto-node (n)
  "Move forward or backward N nodes."
  (interactive "nMove _ plays forward (pos number) or backward (neg number): ")
  (if (> n 0)
      (dotimes (i n) (sgf-forward-node))
    (dotimes (i (- n)) (sgf-backward-node))))


(defun sgf--toggle-layer(layer)
  "Toggle the display of a give layer."
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (svg  (overlay-get ov 'svg))
         group)
    (cond ((equal layer 'mvnum)
           (setq group (sgf-svg-group-mvnums svg)))
          ((equal layer 'marks)
           (setq group (sgf-svg-group-marks svg)))
          ((equal layer 'next)
           (setq group (sgf-svg-group-next svg))))
    (if (dom-attr group 'visibility)
        ;; show the numbers
        (dom-remove-attribute group 'visibility)
      ;; add visibility attribute to hide svg move number group
      (dom-set-attribute group 'visibility "hidden"))
    (sgf-update-overlay game-state svg)))


(defun sgf-toggle-move-number ()
  "Toggle the display of move numbers."
  (interactive)
  (sgf--toggle-layer 'mvnum))

(defun sgf-toggle-next-hint ()
  "Toggle the display of next move hint."
  (interactive)
  (sgf--toggle-layer 'next))

(defun sgf-toggle-marks ()
  "Toggle the display of marks."
  (interactive)
  (sgf--toggle-layer 'marks))


(defun sgf-export-svg (&optional filename)
  "Export the board to an SVG file or display it in a buffer."
  (interactive "FExport file name: ")
  (let* ((ov (sgf-get-overlay))
         (svg (overlay-get ov 'svg)))
    (if (or (not filename) (string-empty-p filename))
        ;; If no filename is given, display the SVG in a buffer
        (with-current-buffer (get-buffer-create "*SVG Image*")
          (erase-buffer)
          (svg-print svg)
          (display-buffer (current-buffer)))
      ;; Otherwise, write the SVG to the specified file
      (when (or (not (file-exists-p filename))
                (y-or-n-p (format "The file '%s' already exists. Overwrite? " filename)))
        (with-temp-file filename
          (svg-print svg))))))


;; Modification Functions
(defun sgf-kill-node ()
  "Delete the current node and all its children."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0)))
    ;; set the prev lnode to link to nil
    (aset (aref curr-lnode 0) 2 nil)
    (sgf-update-buffer-from-game curr-lnode)))


(defun sgf-edit-comment ()
  "Edit the comment of the current node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (curr-node (aref curr-lnode 1))
         ;; C[foo][spam] -> "foo spam"
         (old-comment (mapconcat 'identity (alist-get 'C curr-node) " "))
         (new-comment (read-string "Comment: " old-comment)))
    ;; only update if the comment is changed
    (unless (string= old-comment new-comment)
       ;; Update or remove the 'C' property based on new-comment
      (aset curr-lnode 1
            (if (string-empty-p new-comment)
                ;; delete the comment property if the new comment is empty
                (assq-delete-all 'C curr-node)
              (nconc (assq-delete-all 'C curr-node)
                     (list (list 'C new-comment)))))
      (sgf-update-buffer-from-game curr-lnode)
      (sgf-update-overlay game-state))))


;; igo-editor-move-mode-make-move-root
(defun sgf-make-current-node-root ()
  "Make the current node the root node.
1. move all the nodes in between to the root node.
2. change B, W to AB, AW"
  (interactive))


(defun sgf-edit-game-info ()
  "Edit the game information."
  (interactive))


;; For every move, do:
;; 1. check if it is legal move: not KO, not suicide;
;;    label current stone with red
;; 2. update move number
;; 3. capture opponent stone
;; 4. update board
;; 5. update svg
;; 6. update linked node
;; 7. push change to undo/redo stack
;; 8. stringify the game and update SGF text

(defun sgf-mouse-event-to-board-xy (event)
  "Convert a mouse click to a board position (X . Y)."
  (if (mouse-event-p event)
      (let* ((ov (sgf-get-overlay))
             (interval   (nth 0 (overlay-get ov 'svg-params)))
             (margin     (nth 1 (overlay-get ov 'svg-params)))
             (bar-height (nth 2 (overlay-get ov 'svg-params)))
             (xy (posn-object-x-y (event-start event)))
             (x (/ (- (float (car xy)) margin) interval))
             (y (/ (- (float (cdr xy)) margin bar-height) interval)))
        (cons (round x) (round y)))))

(defun sgf-mouse-click ()
  "Add stone by mouse click on board.
Cases:
1. click on the next move position: the same as `sgf-forward-node';
2. click on other position: put a stone at the clicked position
2.1 if it is at the end of node, create a new node;
2.2 otherwise, create a new branch of game tree.
3. other illegal positions"
  (interactive)
  (let* ((xy (sgf-mouse-event-to-board-xy last-input-event))
         (x (car xy)) (y (cdr xy))
         (ov (sgf-get-overlay))
         (svg  (overlay-get ov 'svg))
         (game-state (overlay-get ov 'game-state))
         (ko (aref game-state 3))
         (curr-lnode  (aref game-state 0))
         (curr-node (aref curr-lnode 1))
         (play (sgf-process-play curr-node))
         (curr-stone (car play))
         (next-stone (if (equal curr-stone 'B) 'W 'B))
         (board-2d   (aref game-state 1))
         (xy-state   (sgf-board-2d-get xy board-2d))
         (next-lnodes (aref curr-lnode 2))
         (next-xys (mapcar (lambda (node) (cdr (sgf-process-play (aref node 1)))) next-lnodes))
         (found (car (seq-positions next-xys xy))))
    (cond (found (sgf-forward-node found)) ;; case 1.
          ((and (equal xy-state 'E) (not (equal xy ko))) ; todo need to check it is not a suicide pos or KO pos
           (let ((n (length next-lnodes))
                 (new-lnode (sgf-linked-node curr-lnode `((,next-stone ,xy)))))
             ;; add the new node as the last branch
             (aset curr-lnode 2 (append next-lnodes (list new-lnode)))
             (sgf-forward-node n) ; if n=0, case 2.1; otherwise, case 2.2
             (let ((stone next-stone)
                   (board-2d (aref game-state 1)))
               (if (null (car (sgf-check-alive xy board-2d)))
                   ;; Move is illegal
                   (progn
                     ;; Remove the new node from the game tree
                     (aset curr-lnode 2 next-lnodes)
                     ;; Revert the game state
                     (sgf-backward-node)
                     (message "Illegal move!"))
                 ;; Move is legal, update the buffer
                 (sgf-update-buffer-from-game curr-lnode)))))

          (t (message "Illegal move!"))))) ; case 3.


(defun sgf-pass ()
  "Pass the current. Add a new node of W[] or B[].

The move number will be incremented."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (next-lnodes (aref curr-lnode 2))
         (new-lnode (sgf-linked-node curr-lnode '((W)))))
    (aset curr-lnode 2 (append next-lnodes (list new-lnode)))
    (sgf-update-overlay game-state svg)))


(defmacro sgf--add-mark (shape add-mark-func)
  "Add a mark to the current node."
  `(let* ((ov (sgf-get-overlay))
          (svg  (overlay-get ov 'svg))
          (game-state (overlay-get ov 'game-state))
          (board-2d (aref game-state 1))
          (interval (nth 0 (overlay-get ov 'svg-params)))
          (curr-lnode (aref game-state 0))
          (curr-node (aref curr-lnode 1))
          (xy (sgf-mouse-event-to-board-xy last-input-event))
          (x (car xy)) (y (cdr xy))
          (xy-state (aref (aref board-2d y) x)))
     (push curr-node (list shape xy))
     (apply add-mark-func svg interval x y xy-state)))


(defun sgf-add-mark-square ()
  "Add a square mark on the board of current game state."
  (interactive)
  (sgf--add-mark 'SQ 'sgf-svg-add-square)
  (message "Add square mark."))

(defun sgf-add-mark-triangle ()
  "Add a triangle mark on the board of current game state."
  (interactive)
  (sgf--add-mark 'TR 'sgf-svg-add-triangle))

(defun sgf-add-mark-circle ()
  "Add a circle mark on the board of current game state."
  (interactive)
  (sgf--add-mark 'CR 'sgf-svg-add-circle))

(defun sgf-add-mark-cross ()
  "Add a cross mark on the board of current game state."
  (interactive)
  (sgf--add-mark 'MA 'sgf-svg-add-cross))

(defun sgf-add-mark-label ()
  "Add a label mark on the board of current game state."
  (interactive))

(defun sgf-del-mark ()
  "Delete a mark from the current node."
  (interactive))


;; (defun sgf-track-dragging ()
;;   "Track dragging on the board. igo-editor-track-dragging"
;;   (interactive)
;;   (let* ((ov (car (overlays-in (point-min) (point-max))))
;;          (svg  (overlay-get ov 'svg))
;;          (hot-areas (overlay-get ov 'hot-areas))
;;          (interval (car (overlay-get ov 'svg-params)))
;;          (game-state (overlay-get ov 'game-state))
;;          (curr-lnode  (aref game-state 0))
;;          (board-2d   (aref game-state 2))
;;          (xy (sgf-mouse-event-to-board-xy last-input-event))
;;          (x (car xy)) (y (cdr xy))
;;          (pos-state (aref (aref board-2d y) x)))
;;     (if (and (eq last-command 'mouse-drag-region)
;;              (not (equal pos-state 'E)))
;;         (progn
;;           (aset (aref board-2d y) x 'E)
;;           (sgf-svg-add-stone svg interval x y 'E)
;;           (overlay-put ov 'display (svg-image svg :map hot-areas))
;;           (overlay-put ov 'svg svg)))))

(defun sgf-toggle-svg-display (&optional choice)
  (interactive)
  (let ((ov (or (sgf-get-overlay) (sgf-setup-overlay))))
    (cond ((equal choice 'hide)
           (sgf--hide-svg ov))
          ((equal choice 'show)
           (sgf--display-svg ov))
          (t (if (null (overlay-get ov 'display))
                 (sgf--display-svg ov)
               (sgf--hide-svg ov))))))


(defun sgf-setup-overlay (&optional interval margin bar-height padding)
  "Setup overlay properties."
  (let* ((ov (or (sgf-get-overlay)
                 ;; set front- and rear-advance parameters to allow
                 ;; the overlay cover the whole buffer even if it is
                 ;; updated from game playing.
                 (make-overlay (point-min) (point-max) nil nil t)))
         (game-tree (sgf-str-to-game-tree (buffer-string)))
         (root (car game-tree))
         (pl-stone (alist-get 'PL root 'B))
         (stone (if (equal pl-stone 'B) 'W 'B))
         (root-lnode (sgf-linked-node nil root nil))
         (w-h (car (alist-get 'SZ root)))
         (w (car w-h)) (h (cdr w-h))
         (board-2d (sgf-create-board-2d w h))
         (interval (or interval sgf-svg-interval))
         (margin (or margin sgf-svg-margin))
         (bar-height (or bar-height sgf-svg-bar-height))
         (padding (or padding sgf-svg-padding))
         (svg-hot-areas (sgf-svg-init w h interval margin bar-height padding))
         (svg (car svg-hot-areas))
         (hot-areas (cdr svg-hot-areas))
         game-state)
    ;; process root node to add stones
    (dolist (prop root)
      (let* ((prop-key (car prop))
             (prop-vals (cdr prop))
             x y stone-color)

        (cond ((or (eq prop-key 'AB) (eq prop-key 'AW))
               (setq stone-color (intern (substring (symbol-name prop-key) 1)))
               (dolist (pos prop-vals)
                 (setq x (car pos) y (cdr pos))
                 (aset (aref board-2d y) x stone-color)
                 (sgf-svg-add-stone svg interval x y stone-color))))))

    (sgf-svg-update-turn svg stone)
    ;; root state
    (sgf-linkup-nodes-in-game-tree (cdr game-tree) root-lnode)
    (setq game-state
          (sgf-game-state root-lnode board-2d 0))

    ;; label this overlay to distinguish from others
    (overlay-put ov 'sgf-overlay t)
    (overlay-put ov 'svg-params (list interval margin bar-height padding))
    (overlay-put ov 'game-state game-state)
    (overlay-put ov 'svg svg)
    (overlay-put ov 'hot-areas hot-areas)
    ov))


(defun sgf--display-svg (ov)
  (let ((svg (overlay-get ov 'svg))
        (hot-areas (overlay-get ov 'hot-areas)))
    (unless (and svg hot-areas)
      (error "Overlay %S does not have 'svg' or 'hot-areas' properties" ov))
    (overlay-put ov 'keymap sgf-mode-graphical-map)
    (overlay-put ov 'display (svg-image svg :map hot-areas))))

(defun sgf--hide-svg (ov)
  (overlay-put ov 'display nil)
  (overlay-put ov 'keymap nil))


;; (defun sgf-redo-overlay ()
;;   "Delete old overlay and create and return a new one."
;;   ;(remove-overlays) ; remove all the overlays in the buffer
;;   (let ((new-ov (make-overlay (point-min) (point-max) (current-buffer) nil t)))
;;     (sgf-setup-overlay new-ov)
;;     new-ov))

;; todo. match only one regexp and highlight different groups
(defvar sgf-mode-font-lock-keywords
  `((,sgf-property-re
     ;; property key
     (1 font-lock-keyword-face))        ; match the 1st group
    (,sgf-property-value-re
     ;; property value
     (0 font-lock-comment-face))        ; match whole regexp
    (,sgf-node-re
     (0 font-lock-builtin-face)))       ; match ;
  "a list of font-lock keywords for SGF mode.")



(defvar sgf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'sgf-toggle-svg-display)
    map)
  "Keymap for SGF major mode.")

(defvar sgf-mode-graphical-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'sgf-forward-node)
    (define-key map [hot-forward mouse-1] 'sgf-forward-node)
    (define-key map "b" 'sgf-backward-node)
    (define-key map [hot-backward mouse-1] 'sgf-backward-node)
    (define-key map "a" 'sgf-first-node)
    (define-key map [hot-first mouse-1] 'sgf-first-node)
    (define-key map "e" 'sgf-last-node)
    (define-key map [hot-last mouse-1] 'sgf-last-node)
    (define-key map "g" 'sgf-goto-node)
    (define-key map "n" 'sgf-toggle-move-number)
    (define-key map "v" 'sgf-view-next)
    (define-key map "k" 'sgf-kill-node)
    (define-key map "s" 'sgf-export-svg)
    (define-key map "c" 'sgf-edit-comment)
    (define-key map "i" 'sgf-edit-game-info)
    (define-key map [hot-grid mouse-1] #'sgf-mouse-click)
    (define-key map [hot-grid mouse-3] #'sgf-menu) ; todo
    (define-key map "p" 'sgf-pass)
    ;; (define-key map (kbd "m s")  'sgf-add-mark-square)
    ;; (define-key map 'sgf-add-mark-triangle)
    ;; (define-key map 'sgf-add-mark-circle)
    ;; (define-key map 'sgf-add-mark-cross)
    ;; (define-key map 'sgf-add-mark-label)
    ;; (define-key map 'sgf-del-mark)
    map)
  "Keymap set for the overlay svg display. It is only activated when the overlay is displayed.")



;; Emacs automatically creates a hook for the mode (e.g.,
;; sgf-mode-hook), and this hook will be run every time the mode is
;; enabled.
;;;###autoload
(define-derived-mode sgf-mode text-mode "SGF"
  "Major mode for editing SGF files.

The following commands are available:

\\{sgf-mode-map}"
  :keymap sgf-mode-map
  (setq font-lock-defaults '(sgf-mode-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sgf\\'" . sgf-mode))


(provide 'sgf-mode)
;;; sgf-mode.el ends here
