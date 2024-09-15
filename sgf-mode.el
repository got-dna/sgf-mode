;;; sgf-mode.el --- SGF Major Mode  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;;; Code:

;(require 'sgf-io)


;; Linked Node Object
(defun sgf-linked-node (prev-node &optional current-node next-nodes)
  "Define node object. It is doubly linked list."
  (vector prev-node current-node next-nodes))

;; Game State
(defun sgf-game-state (linked-node move-num board-2d &optional
                                   black-prisoners black-prisoner-count
                                   white-prisoners white-prisoner-count
                                   ko turn undo-stack)
  "Define game state object. The move number, board-2d, ko, prisoners are re-computed every time when traversing the moves."
  (vector linked-node                 ; 0 current node
          board-2d                    ; 1
          move-num                    ; 2
          ko                          ; 3 ko position
          prisoners                   ; 4 a list of cons (x . y) after current move
          black-white-prisoner-counts ; 5 accumulated number (b . w)
          undo-stack                  ; 6
          redo-stack))


(defun sgf-create-board-2d (w h)
  "Create a empty 2D board of size WxH."
  ;; create a 2d board
  (let ((board-2d (make-vector h nil)))
    (dotimes (i h) ;; for each row
      (aset board-2d i (make-vector w 'E)))
    board-2d))


(defun sgf-board-2d-get (xy board-2d)
  (aref (aref board-2d (cdr xy)) (car xy)))


(defun sgf-board-2d-set (xy v board-2d)
  (aset (aref board-2d (cdr xy)) (car xy) v))


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
  "Return a list of neighboring positions of XY on a board of size WxH."
  (let ((x (car xy)) (y (cdr xy))
        (w (length (aref board-2d 0)))
        (h (length board-2d)))
    (list
     (if (> x 0) (cons (1- x) y)) ;; left
     (if (< x (1- w)) (cons (1+ x) y)) ;; right
     (if (> y 0) (cons x (1- y))) ;; above
     (if (< y (1- h)) (cons x (1+ y)))))) ;; below


(defun sgf-neighbors (xy board-2d)
  (let ((x (car xy)) (y (cdr xy)))
    (list
     (aref (aref board-2d y) (1- x)) ;; left
     (aref (aref board-2d y) (1+ x)) ;; right
     (aref (aref board-2d (1- y)) x) ;; above
     (aref (aref board-2d (1+ y)) x)))) ;; below


(defun sgf-alive-p (xy board-2d &optional last-color visited)
  "Check if the stone on position XY of BOARD-2D is alive.

It returns a cons cell of the form (LIBERTY-FOUND . VISITED). Its car
indicates if a liberty is found, and its cdr is a list of visited
positions to avoid loops, which also stores all the dead positions if no
liberty is found."
  (let ((curr-color (sgf-board-2d-get xy board-2d)))
    (cond
     ;; Base case: the current position is empty, indicating a liberty.
     ((equal curr-color 'E) (cons t visited))
     ;; If we've already visited this position, skip it to avoid loops.
     ((member xy visited) (cons nil visited))
     ;; Recursively check neighbors of the same color.
     ((or (null last-color) (equal last-color curr-color))
      ;; Add the current position to the visited list.
      (setq visited (cons xy visited))
      ;; Recursively check neighbors.
      (let ((liberty-found nil))
        (dolist (neighbor (sgf-neighbors-xy xy board-2d) (cons liberty-found visited))
          (when neighbor
            (let ((result (sgf-alive-p neighbor board-2d curr-color visited)))
              ;; If a liberty is found, return immediately.
              (if (car result)
                  (setq liberty-found t)
                ;; Otherwise, update visited positions.
                (setq visited (cdr result))))))))
     ;; No liberty found.
     (t  (cons nil visited)))))



(defun sgf-capture (xy board-2d)
  "Capture the opponent stone after the play of position XY.
Return the list of positions for the prisoners captured."
  (let ((curr-color (sgf-board-2d-get xy board-2d))
        (prisoners nil))
    (dolist (neighbor-xy (sgf-neighbors-xy xy board-2d))
      (if (and neighbor-xy
               (not (member neighbor-xy prisoners))
               (not (equal curr-color (sgf-board-2d-get neighbor-xy board-2d))))
          (let* ((results (sgf-alive-p neighbor-xy board-2d))
                 (alive-p (car results)))
            (if (not alive-p)
                (setq prisoners (nconc (cdr results) prisoners))))))
    prisoners))


(defun sgf-process-play (node)
  "Process a play node."
  (let* ((color (if (assoc 'B node) 'B 'W))
         (xy (car (alist-get color node)))
         (comment (car (alist-get 'C node))))
    (if comment (message comment))
    (cons color xy)))


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


(defun sgf-update-game-state (game-state &rest body)
  (unwind-protect
      (progn ,@body)
        game-state)
    (message "Error updating game state."))


(defun sgf-add-captured-stones (xys color board-2d svg)
  (dolist (xy xys)
    (let ((x (car xy)) (y (cdr xy)))
      (aset (aref board-2d y) x color)
      (dom-set-attribute svg (board-svg-mvnum-id x y)
                         "color" (if (equal color 'B) "white" "black"))
      (dom-remove-attribute svg (board-svg-stone-id x y) "visibility"))))


(defun sgf-del-captured-stone (xys color board-2d svg)
  (dolist (xy xys)
    (let ((x (car xy)) (y (cdr xy)))
      (aset (aref board-2d y) x 'E)
      (dom-set-attribute svg (board-svg-mvnum-id x y)
                         (if (equal color 'B) "black" "white"))
      (dom-set-attribute svg (board-svg-stone-id x y) "visibility" "hidden"))))


(defun sgf-update-svg-marks (svg interval mark board-2d)
  "Process a mark property and update the marks on the board."
  ;; make sure to remove old marks
  (let ((mark-grp (board-svg-marks-group svg))
        (grid (dom-parent svg mark-grp))
        (type (car mark))
        x y xy-state)
    (svg-remove svg "marks")            ; remove the whole group
    (setq mark-grp (svg-node grid :id "marks")) ; create new group
    (dolist (xy (cdr mark))
      (setq x (car xy) y (cdr xy)
            xy-state (sgf-board-2d-get x y))
      (board-svg-add-mark type mark-grp interval x y xy-state))))


  ;; (vector linked-node                 ; 0 current node
  ;;         board-2d                    ; 1
  ;;         move-num                    ; 2
  ;;         ko                          ; 3 ko position
  ;;         prisoners                   ; 4 a list of cons (x . y) after current move
  ;;         black-white-prisoner-counts ; 5 accumulated number (b . w)
  ;;         undo-stack                  ; 6
  ;;         redo-stack))


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
;; Navigation Functions
(defun sgf-forward-node (&optional branch)
  "Move to the next node in the game tree and update board."
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (svg         (overlay-get ol 'svg))
         (hot-areas   (overlay-get ol 'hot-areas))
         (interval    (car (overlay-get ol 'svg-params)))
         (game-state  (overlay-get ol 'game-state))
         (curr-lnode  (aref game-state 0))
         (board-2d    (aref game-state 1))
         (mvnum       (aref game-state 2))
         (next-lnodes (aref curr-lnode 2))
         (n           (length next-lnodes))
         (pcounts     (aref game-state 5))
         (clone  (copy-sequence game-state))
         next-lnode next-node play xy x y color prisoners)
    (if (= n 0) (progn (message "No more next play.") nil)
      (setq branch
            (or branch
                (if (= n 1) 0
                  (- (read-char (format "Select a branch (a-%c): " (+ ?a n -1))) ?a))))
      (if (or (< branch 0) (>= branch n))
          (error "Invalid branch selection."))
      (setq next-lnode (nth branch next-lnodes))
      (setq next-node (aref next-lnode 1)))
    (setq play (sgf-process-play (aref next-lnode 1)))
    (setq color (car play) xy (cdr play))
    (condition-case-unless-debug err
        ;; protect the game state from partially updated:
        ;; if it failed, roll back to previous game state
        (progn
          (setq mvnum (1+ mvnum))
          (aset game-state 1 mvnum)
          (if (null xy) ; xy is not nil - this is not a pass
              (aset game-state 4 nil)
            ;; update game-state: make sure this aset is in place.
            (sgf-board-2d-set xy color board-2d)
            (setq prisoners (sgf-capture xy board-2d))
            (aset game-state 4 prisoners)
            ;; Remove captured stones and update prisoner counts
            (sgf-del-captured-stones prisoners color board-2d svg)
            (if (equal color 'B)     ;todo
                (setcar pcounts (+ (length prisoners) (car pcounts)))
              (setcdr pcounts (+ (length prisoners) (cdr pcounts))))
            (aset game-state 0 next-lnode)
            (board-svg-add-stone svg interval x y (symbol-name color))
            (board-svg-add-mvnum svg interval x y mvnum color))

          ;; Error handling: Roll back to the previous game state
          (error
           (message "Error: %s. Rolling back to the previous game state." err)
           (setq game-state clone)))

      (overlay-put ol 'display (svg-image svg :map hot-areas))
      (overlay-put ol 'svg svg))))


(defun sgf-backward-node ()
  "Move to the previous node in the game tree and update board."
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (svg        (overlay-get ol 'svg))
         (hot-areas  (overlay-get ol 'hot-areas))
         (game-state (overlay-get ol 'game-state))
         (curr-lnode (aref game-state 0))
         (board-2d   (aref game-state 1))
         (mvnum      (aref game-state 2))
         (prev-lnode (aref curr-lnode 0))
         play xy x y)
    (if prev-lnode
        (progn (aset game-state 1 (1- mvnum))
               (setq play (sgf-process-play (aref curr-lnode 1)))
               (setq xy (cdr play)
                     color (car play)
                     x (car xy)
                     y (cdr xy))
               ;; update game-state: make sure this aset is inplace.
               (aset (aref board-2d y) x 'E)
               (sgf-add-captured-stones (aref game-state 4)
                                        color
                                        board-2d
                                        svg)
               (aset game-state 0 prev-lnode)
               (svg-remove svg (board-svg-stone-id x y))
               (svg-remove svg (board-svg-mvnum-id x y))
               (overlay-put ol 'display (svg-image svg :map hot-areas))
               (overlay-put ol 'svg svg))
      (progn (message "No previous node.") nil))))


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
  (interactive "nMove forward (pos number) or backward (neg number) node number: ")
  (if (> n 0)
      (dotimes (i n) (sgf-forward-node))
    (dotimes (i (- n)) (sgf-backward-node))))


(defun sgf-toggle-move-number()
  "Toggle the display of move numbers."
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (hot-areas (overlay-get ol 'hot-areas))
         (svg  (overlay-get ol 'svg)))
    (if (dom-attr (board-svg-mvnums-group svg) 'visibility)
        ;; show the numbers
        (dom-remove-attribute (board-svg-mvnums-group svg) 'visibility)
      ;; add visibility attribute to hide svg move number group
      (dom-set-attribute (board-svg-mvnums-group svg) 'visibility "hidden"))
    (overlay-put ol 'svg svg)
    (overlay-put ol 'display (svg-image svg :map hot-areas))))


(defun sgf-view-next ()
  "Toggle the display of hint of next move(s)."
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (svg  (overlay-get ol 'svg))
         (grid (car (dom-by-id "grid")))
         (hot-areas (overlay-get ol 'hot-areas))
         (interval (nth 0 (overlay-get ol 'svg-params)))
         (game-state (overlay-get ol 'game-state))
         (curr-lnode (aref game-state 0))
         (next-lnodes (aref curr-lnode 2))
         (char ?A))
    (if next-lnodes
        (dolist (next-lnode next-lnodes)
          (let* ((play (sgf-process-play (aref next-lnode 1)))
                 (xy (cdr play))
                 (x (car xy))
                 (y (cdr xy)))
            (board-svg-add-text grid interval x y (string char) 'E)
            (setq char (1+ char))))))
  (overlay-put ol 'svg svg)
  (overlay-put ol 'display (svg-image svg :map hot-areas)))


(defun sgf-export-svg (&optional filename)
  "Export the board to an SVG file or display it in a buffer."
  (interactive "FExport file name: ")
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (svg (overlay-get ol 'svg)))
    (if (or (not filename) (string-empty-p filename))
        ;; If no filename is given, display the SVG in a buffer
        (let ((output-buffer (get-buffer-create "*SVG Image*")))
          (with-current-buffer output-buffer
            (erase-buffer)
            (insert (svg-print svg)))
          (display-buffer output-buffer))
      ;; Otherwise, write the SVG to the specified file
      (when (or (not (file-exists-p filename))
                (y-or-n-p (format "The file '%s' already exists. Overwrite? " filename)))
        (with-temp-file filename
          (svg-print svg))))))


;; Modification Functions
(defun sgf-kill-node ()
  "Delete the current node and all its children."
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (game-state (overlay-get ol 'game-state))
         (curr-lnode (aref game-state 0)))
    ;; set the prev lnode to link to nil
    (aset  (aref curr-lnode 0) 2 nil)))

(defun sgf-edit-comment ()
  "Edit the comment of the current node."
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (game-state (overlay-get ol 'game-state))
         (curr-lnode (aref game-state 0))
         (curr-node (aref curr-lnode 1))
         (old-comment (or (car (alist-get 'C curr-node)) ""))
         (new-comment (read-string "Comment: " old-comment)))
    (if (not (string= old-comment new-comment))
        ;; only update if the comment is changed
        (if (string-empty-p new-comment)
            ;; delete the comment
            (aset curr-lnode 1 (assq-delete-all 'C curr-node))
          (setf (cdr (assoc 'C curr-node)) new-comment)))))
;; (aset curr-lnode 1 (alist-put 'C new-comment curr-node))))))

;; igo-editor-move-mode-make-move-root
(defun sgf-make-current-node-root ()
  "Make the current node the root node.
1. move all the nodes in between to the root node.
2. change B, W to AB, AW"
  (interactive))


(defun sgf-edit-game-info ()
  "Edit the game information."
  (interactive))


(defun sgf-mouse-click ()
  "Add stone by mouse click on board.

1. click on the next move position: the same as `sgf-forward-node';
2. click on other position: put a stone at the clicked position
2.1 if it is at the end of node, create a new node;
2.2 otherwise, create a new branch of game tree.
3. other illegal positions"
  (interactive)
  (let* ((xy (sgf-mouse-event-to-board-xy last-input-event))
         (x (car xy)) (y (cdr xy))
         (ol (car (overlays-in (point-min) (point-max))))
         (svg  (overlay-get ol 'svg))
         (game-state (overlay-get ol 'game-state))
         (curr-lnode  (aref game-state 0))
         (root-lnode  curr-lnode)
         (board-2d   (aref game-state 2))
         (xy-state   (aref (aref board-2d y) x))
         (next-lnodes (aref curr-lnode 2))
         (next-xys (mapcar (lambda (node) (cdr (sgf-process-play (aref node 1)))) next-lnodes))
         (found (car (seq-positions next-xys xy))))
    (cond (found (sgf-forward-node found)) ;; case 1.
          ((eq xy-state 'E) ; todo need to check it is not a suicide pos or KO pos
           (let ((n (length next-lnodes))
                 (prisoners 0)
                 (new-lnode (sgf-linked-node curr-lnode `((B ,xy)))))
             (aset curr-lnode 2 (append next-lnodes (list new-lnode)))
             ;; todo remove any residual mvnum-id stone-id
             ;; get to the root node from any node
             (while (aref root-lnode 0) (setq root-lnode (aref root-lnode 0)))
             (erase-buffer)
             (insert (sgf-str-from-game-tree root-lnode))
             (if (= n 0)
                 (sgf-forward-node) ; case 2.1
               (sgf-forward-node n)) ; case 2.2
             (message "Put stone at %d %d" (1+ x) (1+ y))))
          (t (message "Illegal move!"))))) ; case 3.


(defun sgf-create-node ()

(defun sgf-pass ()
  "Pass the current. Add a new node of W[] or B[].

The move number will be incremented."
  (interactive)
                                        ;todo
  )

(defmacro sgf--add-mark (shape add-mark-func)
  "Add a mark to the current node."
  `(let* ((ol (car (overlays-in (point-min) (point-max))))
          (svg  (overlay-get ol 'svg))
          (game-state (overlay-get ol 'game-state))
          (board-2d (aref game-state 1))
          (interval (nth 0 (overlay-get ol 'svg-params)))
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
  (sgf--add-mark 'SQ 'board-svg-add-square)
  (message "Add square mark."))

(defun sgf-add-mark-triangle ()
  "Add a triangle mark on the board of current game state."
  (interactive)
  (sgf--add-mark 'TR 'board-svg-add-triangle))

(defun sgf-add-mark-circle ()
  "Add a circle mark on the board of current game state."
  (interactive)
  (sgf--add-mark 'CR 'board-svg-add-circle))

(defun sgf-add-mark-cross ()
  "Add a cross mark on the board of current game state."
  (interactive)
  (sgf--add-mark 'MA 'board-svg-add-cross))

(defun sgf-add-mark-label ()
  "Add a label mark on the board of current game state."
  (interactive))

(defun sgf-del-mark ()
  "Delete a mark from the current node."
  (interactive))


(defun sgf-mouse-event-to-board-xy (event)
  "Convert a mouse click to a board position (X . Y)."
  (if (mouse-event-p event)
      (let* ((ol (car (overlays-in (point-min) (point-max))))
             (interval   (nth 0 (overlay-get ol 'svg-params)))
             (margin     (nth 1 (overlay-get ol 'svg-params)))
             (bar-height (nth 2 (overlay-get ol 'svg-params)))
             (xy (posn-object-x-y (event-start event)))
             (x (/ (- (float (car xy)) margin) interval))
             (y (/ (- (float (cdr xy)) margin bar-height) interval)))
        (cons (round x) (round y)))))


(defvar sgf-mode-map
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
    (define-key map "p" 'sgf-pass)
    ;; (define-key map (kbd "m s")  'sgf-add-mark-square)
    ;; (define-key map 'sgf-add-mark-triangle)
    ;; (define-key map 'sgf-add-mark-circle)
    ;; (define-key map 'sgf-add-mark-cross)
    ;; (define-key map 'sgf-add-mark-label)
    ;; (define-key map 'sgf-del-mark)
    map))


(defun sgf-track-dragging ()
  "Track dragging on the board. igo-editor-track-dragging"
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (svg  (overlay-get ol 'svg))
         (hot-areas (overlay-get ol 'hot-areas))
         (interval (car (overlay-get ol 'svg-params)))
         (game-state (overlay-get ol 'game-state))
         (curr-lnode  (aref game-state 0))
         (board-2d   (aref game-state 2))
         (xy (sgf-mouse-event-to-board-xy last-input-event))
         (x (car xy)) (y (cdr xy))
         (pos-state (aref (aref board-2d y) x)))
    (if (and (eq last-command 'mouse-drag-region)
             (not (equal pos-state 'E)))
        (progn
          (aset (aref board-2d y) x 'E)
          (board-svg-add-stone svg interval x y 'E)
          (overlay-put ol 'display (svg-image svg :map hot-areas))
          (overlay-put ol 'svg svg)))))


(defun sgf-toggle-board-svg (&optional interval margin bar-height padding)
  "Visualize the board in the current buffer using SVG."
  (interactive)
  (let ((ol (overlays-in (point-min) (point-max))))
    (if ol (dolist (i ol) (delete-overlay i))
      (let* ((game-tree (sgf-str-to-game-tree (buffer-string)))
             (root (car game-tree))
             (root-lnode (sgf-linked-node nil root nil))
             (w-h (car (alist-get 'SZ root)))
             (w (car w-h)) (h (cdr w-h))
             (board-2d (sgf-create-board-2d w h))
             (interval (or interval board-svg-interval))
             (margin (or margin board-svg-margin))
             (bar-height (or bar-height board-svg-bar-height))
             (padding (or padding board-svg-padding))
             (svg-hot-areas (board-svg-init w h interval margin bar-height padding))
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
                     (board-svg-add-stone svg interval x y stone-color))))))

        (setq ol (make-overlay (point-min) (point-max)))
        ;; root state
        (sgf-linkup-nodes-in-game-tree (cdr game-tree) root-lnode)
        (setq game-state
              (sgf-game-state root-lnode 0 board-2d nil (cons 0 0) nil))
        (overlay-put ol 'game-state  game-state)
        (overlay-put ol 'svg-params (list interval margin bar-height))
        (overlay-put ol 'svg svg)
        (overlay-put ol 'hot-areas hot-areas)
        (overlay-put ol 'display (svg-image svg :map hot-areas))))))


(defun sgf-mode-font-lock-keywords ()
  "Return a list of font-lock keywords for SGF mode."
  (list
   ;; Node
   (cons sgf-node-re 'font-lock-keyword-face)
   ;; Property
   (cons sgf-property-re 'font-lock-type-face)
   ;; Property value
   (cons sgf-property-value-re 'font-lock-string-face)))

(defvar sgf-mode-hook nil)

(define-derived-mode sgf-mode text-mode "SGF"
  "Major mode for editing SGF files.

The following commands are available:

\\{sgf-mode-map}"

  :keymap sgf-mode-map
  (setq font-lock-defaults '(sgf-mode-font-lock-keywords t))
  (run-mode-hooks 'sgf-mode-hook))

(provide 'sgf-mode)
;;; sgf-mode.el ends here
