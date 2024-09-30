;;; sgf-mode.el --- SGF Major Mode  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: SGF, go, game

;;; Code:

(require 'sgf-io)
(require 'sgf-svg)


(defvar sgf-allow-suicide-move nil
  "Allow suicide or not. Some rule set allow suicide: https://senseis.xmp.net/?Suicide")

(defvar sgf-show-next-hint t
  "Show the hint mark(s) for next move(s).")

(defvar sgf-show-move-number t
  "Show move number on the stones.")

(defvar sgf-show-mark t
  "Show marks on the board.")

;; Linked Node Object
(defun sgf-linked-node (prev-node &optional current-node next-nodes)
  "Define node object. It is doubly linked list."
  (vector prev-node current-node next-nodes))

;; Game State
(defun sgf-game-state (linked-node
                       board-2d
                       &optional
                         ko
                         turn
                         prisoners
                         undos)
  "Define game state object. The move number, board-2d, ko, prisoners are re-computed every time when traversing the moves."
  (vector linked-node                 ; 0 current node
          board-2d                    ; 1
          ko                          ; 3 ko position
          (or turn 'B)                        ; 2 move turn
          ;; 4. accumulated number of prisoners (b . w)
          (or prisoners (cons 0 0))
          ;; 5. stack of changes. each change contains:
          ;; - a list of cons cells: xy positions for black stones
          ;; - a list of cons cells: xy positions for white stones
          ;; - a list of cons cells: xy positions for empty
          ;; - ko position (cons cell)
          ;; - turn
          undos))


(defun sgf-revert-undo (change game-state)
  "Revert the CHANGE for the GAME-STATE"
  (let ((black-xys (aref change 0))
        (white-xys (aref change 1))
        (empty-xys (aref change 2))
        (ko (aref change 3))
        (turn (aref change 4))
        (board-2d (aref game-state 1))
        (pcounts  (aref game-state 4)))
    (dolist (xy black-xys) (sgf-board-2d-set xy 'B board-2d))
    (dolist (xy white-xys) (sgf-board-2d-set xy 'W board-2d))
    (dolist (xy empty-xys) (sgf-board-2d-set xy 'E board-2d))
    (aset game-state 3 turn)
    (aset game-state 2 ko)
    (setcar pcounts (- (car pcounts) (length black-xys)))
    (setcdr pcounts (- (cdr pcounts) (length white-xys)))))


(defun sgf-root-lnode-p (lnode)
  "Check if LNODE is the root node."
  (null (aref lnode 0)))


(defun sgf-valid-stone-p (stone)
  "Check if STONE is a valid color."
  (or (equal stone 'B) (equal stone 'W)))


(defun sgf-xy-on-board-p (xy board-2d)
  "Check if XY is on the board."
  (if xy
      (let ((x (car xy)) (y (cdr xy)))
        (and (>= x 0) (< x (length (aref board-2d 0)))
             (>= y 0) (< y (length board-2d))))))


(defun sgf-xy-is-empty-p (xy board-2d)
  "Check if XY is empty on the board."
  (if xy
      (equal (sgf-board-2d-get xy board-2d) 'E)))


(defun sgf-valid-move-p (xy stone game-state &optional allow-suicide)
  "Check if the move of STONE at XY position on BOARD-2D is valid"
  (let* ((board-2d (aref game-state 1))
         (ko (aref game-state 2)))
    (and
     board-2d
     (sgf-valid-stone-p stone)             ;; valid color
     (sgf-xy-on-board-p xy board-2d) ;; position is on board
     (sgf-xy-is-empty-p xy board-2d) ;; no stone at this position yet
     (not (equal xy ko))             ;; pos is not ko
     (if (not allow-suicide) (not (sgf-suicide-stones xy board-2d)) ;; not suicide move
       ))))


(defun sgf-create-board-2d (w h &optional default)
  "Create a empty 2D board of size WxH with DEFAULT value."
  (let ((board-2d (make-vector h nil)))
    (dotimes (i h) ;; for each row
      (aset board-2d i (make-vector w default)))
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


(defun sgf-enemy-stone (stone)
  "Return the opponent stone of STONE."
  (if (equal stone 'B) 'W 'B))


(defun sgf-neighbors-xy (xy board-2d)
  "Return a list of neighboring positions of XY on a board of size WxH.

Return nil if xy is nil."
  (if (consp xy)
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


(defun sgf-check-liberty (xy board-2d &optional last-color visited)
  "Check if the stone on position XY of BOARD-2D is alive or suiciding.

It returns a cons cell of the form (LIBERTY-FOUND . VISITED). Its car
indicates if a liberty is found, and its cdr is a list of visited
positions to avoid loops, which also stores all the dead positions if no
liberty is found.

If xy is nil (for a move of pass), it returns (t . nil)."
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
            (let ((result (sgf-check-liberty neighbor board-2d curr-color visited)))
              (if (car result)
                  (setq liberty-found t)
                (setq visited (cdr result))))))))
     (t (cons nil visited)))))


(defun sgf-capture-stones (xy board-2d)
  "Compute the captured enemy stones after the play of position XY.
Return the list of positions for the prisoners captured.
If XY is nil (for a move of pass), it returns nil."
  (let ((curr-color (sgf-board-2d-get xy board-2d))
        (prisoners nil))
    (dolist (neighbor-xy (sgf-neighbors-xy xy board-2d))
      (when (and neighbor-xy
                 (not (member neighbor-xy prisoners))
                 (not (equal curr-color (sgf-board-2d-get neighbor-xy board-2d))))
        (let* ((results (sgf-check-liberty neighbor-xy board-2d))
               (alive-p (car results)))
          (unless alive-p
            (setq prisoners (nconc (cdr results) prisoners))))))
    prisoners))


(defun sgf-suicide-stones (xy board-2d)
  "Compute the suiciding stones after the play of position XY.
Return the list of positions for the suiciding stones.
If XY is nil (for a move of pass), it returns nil."
  (let ((results (sgf-check-liberty xy board-2d)))
    (unless (car results)
      (cdr results))))



(defun sgf-get-ko (xy stone board-2d captured-xys)
  "Check if any neighbor of XY is a KO position *after* putting STONE at XY
on BOARD-2D and possibly captured enemy stones at positions
CAPTURED-XYS (which is a list of cons cells indicating captured
positions).

Returns nil or (x . y) for KO position. "
  (if (or (/= 1 (length captured-xys))
          (equal stone 'E))
      nil ; not a KO if more than 1 stones were captured in the XY move
    (let ((empty-or-same-color-count 0)
          empty-or-same-color-position
          state-i)
      (dolist (xy-i (sgf-neighbors-xy xy board-2d))
        (when xy-i
          (setq state-i (sgf-board-2d-get xy-i board-2d))
          (when (or (equal state-i stone) (equal state-i 'E))
            (setq empty-or-same-color-count (1+ empty-or-same-color-count))
            (setq empty-or-same-color-position xy-i))))
      (if (= empty-or-same-color-count 1)
          empty-or-same-color-position
        nil))))


;; Alternative implementation
;; (defun sgf-process-move (node)
;;   "Process a play node.
;; If 'B is present in the node, return (B) or (B . xy) depending on the presence of value.
;; If 'W is present in the node, return (W) or (W . xy) similarly.
;; Returns nil if neither 'B nor 'W is present."
;;   (cond
;;    ((assoc 'B node) (cons 'B (car (alist-get 'B node))))
;;    ((assoc 'W node) (cons 'W (car (alist-get 'W node))))
;;    (t nil)))  ;; Return nil if neither 'B nor 'W is found

(defun sgf-process-move (node)
  "Process a play node.
If 'B or 'W is present in the node, return (B x . y) or (W x . y).
If 'B or 'W exists without coordinates, return (B) or (W).
If neither 'B nor 'W is present, return nil."
  (pcase (or (assoc 'B node) (assoc 'W node))
    (`(,stone (,x . ,y)) (cons stone (cons x y)))    ;; Extract (B/W (x . y)) case
    (`(,stone) (list stone))                  ;; Handle (B) or (W)
    (_ nil)))                                 ;; If nothing found, return nil


(defun sgf-show-comment (node)
  "Show the comment of the move/node."
  ;; if 'C' does not exist, it shows an empty str.
  (message (mapconcat 'identity (alist-get 'C node) " ")))


;; todo: rename
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


(defun sgf-update-display (ov &optional svg hot-areas)
  "Update the SVG display with HOT-AREAS."
  (if svg
      (overlay-put ov 'svg svg)
    (setq svg (overlay-get ov 'svg)))
  (if hot-areas
      (overlay-put ov 'hot-areas hot-areas)
    (setq hot-areas (overlay-get ov 'hot-areas)))
  (overlay-put ov 'display (svg-image svg :map hot-areas)))


(defun sgf-branch-selection (n &optional branch)
  "Prompt the user to select a branch by reading a character."
  (let ((prompt (format "Select a branch (a-%c): " (+ ?a (1- n)))))
    (if (null branch)
        (setq branch (if (= n 1) 0 (- (read-char prompt) ?a))))
    (if (and (>= branch 0) (< branch n))
        branch
      (error "Invalid branch selection: %c" (+ branch ?a)))))


(defun sgf-push-undo (change game-state)
  "Push a game CHANGE to the undo stack in GAME-STATE."
  (let ((undos (aref game-state 5)))
    (aset game-state 5 (cons change undos))))

(defun sgf-pop-undo (game-state)
  "Pop a game CHANGE from the undo stack in GAME.STATE"
  (let ((undos (aref game-state 5)))
    (if undos
        (let ((change (car undos)))
          (aset game-state 5 (cdr undos))
          change)
      nil)))

(defun sgf-forward-move (&optional branch)
  "Move to the next move in the game tree and update board."
  (interactive)
  (let* ((ov         (sgf-get-overlay))
         (svg        (overlay-get ov 'svg))
         (game-state  (overlay-get ov 'game-state))
         (curr-lnode  (aref game-state 0))
         (next-lnodes (aref curr-lnode 2))
         (n           (length next-lnodes))
         next-lnode next-node)
    (if (= n 0)
        (progn (message "No more next play.") nil)
      (progn
        (setq branch (sgf-branch-selection n branch))
        (setq next-lnode (nth branch next-lnodes))
        (setq next-node  (aref next-lnode 1))
        (sgf-apply-node next-node game-state)
        (aset game-state 0 next-lnode)
        (sgf-update-svg game-state svg)
        (sgf-update-display ov)))))

(defun sgf-backward-move ()
  "Move to the previous move in the game tree and update board."
  (interactive)
  (let* ((ov         (sgf-get-overlay))
         (svg        (overlay-get ov 'svg))
         (game-state  (overlay-get ov 'game-state))
         (curr-lnode  (aref game-state 0)))
    (if (sgf-root-lnode-p curr-lnode)
        (progn (message "No more previous play.") nil)
      (progn
        (sgf-revert-undo (sgf-pop-undo game-state) game-state)
        (aset game-state 0 (aref curr-lnode 0))
        (sgf-update-svg game-state svg)
        (sgf-update-display ov)))))


(defun sgf-apply-node (node game-state)
  "Apply the node of move to the game state."
  (let* ((move (sgf-process-move node))
         (stone (car move))
         (xy (cdr move))
         (board-2d (aref game-state 1))
         (xy-state-old (sgf-board-2d-get xy board-2d))
         (turn-old (aref game-state 3))
         (turn-new (sgf-enemy-stone turn-old))
         (ko-old (aref game-state 2))
         (pcounts (aref game-state 4))
         black-xys white-xys empty-xys)
    (sgf-board-2d-set xy stone board-2d)
    (setq prisoners (sgf-capture-stones xy board-2d))
    ;; Remove captured stones
    (dolist (xy prisoners) (sgf-board-2d-set xy 'E board-2d))
    ;; Check for KO: need to put after prisoners are removed.
    (setq ko-new (sgf-get-ko xy stone board-2d prisoners))
    (aset game-state 3 turn-new)
    (aset game-state 2 ko-new)
    (if (equal stone 'B)
        (progn (setcdr pcounts (+ (length prisoners) (cdr pcounts)))
               (setq white-xys prisoners))
      (progn (setcar pcounts (+ (length prisoners) (car pcounts)))
             (setq black-xys prisoners)))
    (if (equal xy-state-old 'E)
        (setq empty-xys (list xy)))
    (sgf-push-undo (vector black-xys white-xys empty-xys ko-old turn-old)
                   game-state)))

(defun sgf-update-svg (game-state svg)
  "Update the svg object of the game state."
  (let* ((board-2d   (aref game-state 1))
         (turn       (aref game-state 3))
         (pcounts    (aref game-state 4))
         (curr-lnode (aref game-state 0))
         (curr-node  (aref curr-lnode 1)))
    ;; Add stones
    (sgf-svg-add-stones svg game-state)
    (sgf-svg-add-mvnums svg game-state)
    (sgf-svg-update-status-prisoners svg pcounts)
    (sgf-svg-update-status-turn svg turn)
    (sgf-svg-update-next svg curr-lnode)
    (sgf-svg-update-marks svg curr-node board-2d)))


(defun sgf-first-move ()
  "Move to the first node in the game tree."
  (interactive)
  (while (sgf-backward-move)))


(defun sgf-last-move ()
  "Move to the last node in the game tree."
  (interactive)
  (while (sgf-forward-move)))


(defun sgf-goto-move (n)
  "Move forward or backward N nodes."
  (interactive "nMove _ plays forward (pos number) or backward (neg number): ")
  (if (> n 0)
      (dotimes (i n) (sgf-forward-move))
    (dotimes (i (- n)) (sgf-backward-move))))


(defun sgf--toggle-layer(layer)
  "Toggle the display of a give layer."
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (svg  (overlay-get ov 'svg))
         group)
    (cond ((equal layer 'mvnum)
           (sgf-game-property-toggle ov :show-move-number)
           (setq group (sgf-svg-group-mvnums svg)))
          ((equal layer 'mark)
           (sgf-game-property-toggle ov :show-mark)
           (setq group (sgf-svg-group-marks svg)))
          ((equal layer 'next)
           (sgf-game-property-toggle ov :show-next-hint)
           (setq group (sgf-svg-group-next svg))))
    (if (dom-attr group 'visibility)
        ;; show the numbers
        (dom-remove-attribute group 'visibility)
      ;; add visibility attribute to hide svg move number group
      (dom-set-attribute group 'visibility "hidden"))
    (sgf-update-display ov)))


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
  (sgf--toggle-layer 'mark))


(defun sgf-export-image (&optional filename)
  "Export the board to an SVG file or display it in a buffer."
  (interactive "FExport file name: ")
  (let* ((ov (sgf-get-overlay))
         (svg (overlay-get ov 'svg)))
    ;; todo clone svg and delete menu bar
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


(defun sgf-edit-move-number ()
  "Edit the move number of the current node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (curr-node  (aref curr-lnode 1))
         (old-mvnum  (car (alist-get 'MN curr-node)))
         new-mvnum valid)
    (while (not valid)
      (setq new-mvnum (read-number "Move number: " old-mvnum))
      (if (not (integerp new-mvnum))
          (message "Invalid move number. Please enter an integer.")
        (setq valid t)))
    (unless (equal old-mvnum new-mvnum)
      (aset curr-lnode 1
            (nconc (assq-delete-all 'MN curr-node)
                   (list (list 'MN new-mvnum))))
      (sgf-update-buffer-from-game curr-lnode (overlay-buffer ov))
      (sgf-update-display ov))))


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
      (sgf-update-buffer-from-game curr-lnode (overlay-buffer ov))
      (sgf-update-display ov))))


;; igo-editor-move-mode-make-move-root
(defun sgf-make-current-node-root ()
  "Make the current node the root node.
1. move all the nodes in between to the root node.
2. change B, W to AB, AW"
  (interactive))


;; Modification Functions
(defun sgf-delete-this-node (lnode)
  "Delete the current node and all its children."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (prev-lnode (aref lnode 0))
         (lnodes (aref prev-lnode 2))
         (lnodes-del (remove lnode lnodes)))
    (aset prev-lnode 2 lnodes-del)
    (sgf-backward-move)
    (sgf-update-buffer-from-game prev-lnode (overlay-buffer ov))))


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

(defun sgf-mouse-event-to-xy (event)
  "Convert a mouse click to a board position (X . Y)."
  (if (mouse-event-p event)
      (let* ((xy (posn-object-x-y (event-start event)))
             (x (/ (- (float (car xy)) sgf-svg-margin) sgf-svg-interval))
             (y (/ (- (float (cdr xy)) sgf-svg-margin sgf-svg-bar) sgf-svg-interval)))
        (cons (round x) (round y)))))

(defun sgf-board-click-left ()
  "Add stone by mouse click on board.
Cases:
1. click on the next move position: the same as `sgf-forward-move';
2. click on other position: put a stone at the clicked position
2.1 if it is at the end of node, create a new node;
2.2 otherwise, create a new branch of game tree.
3. other illegal positions"
  (interactive)
  (let* ((xy (sgf-mouse-event-to-xy last-input-event))
         (ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (ko   (aref game-state 2))
         (turn (aref game-state 3))
         (next-lnodes (aref curr-lnode 2))
         (next-xys (mapcar (lambda (node) (cdr (sgf-process-move (aref node 1)))) next-lnodes))
         (found (car (seq-positions next-xys xy))))
    (cond
      ;; Case 1: Clicked on the next move position
      (found (sgf-forward-move found))
      ;; Case 2: Clicked on an empty position not equal to ko
      ((sgf-valid-move-p xy turn game-state sgf-allow-suicide-move)
       (let ((n (length next-lnodes))
             (new-lnode (sgf-linked-node curr-lnode `((,turn ,xy)))))
         ;; add the new node as the last branch
         (aset curr-lnode 2 (append next-lnodes (list new-lnode)))
         (sgf-forward-move n) ; if n=0, case 2.1; otherwise, case 2.2
         (sgf-update-buffer-from-game curr-lnode (overlay-buffer ov))))
      ;; Case 3.
      (t (message "Illegal move!")))))


(defun sgf-board-click-right ()
  "Right click on board."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (xy (sgf-mouse-event-to-xy last-input-event))
         (clicked-lnode (sgf-find-and-goto-node xy game-state))
         (menu  `(keymap
                  "Move Action"
                  (sgf-goto-this-node
                   menu-item "Back to This Move"
                   sgf-goto-this-node
                   :enable ,(not (equal curr-lnode clicked-lnode)))
                  (sgf-delete-this-node
                   menu-item "Delete This Move and After"
                   sgf-delete-this-node
                   :enable ,(aref clicked-lnode 0)) ; not root node
                  (sgf-root-this-node
                   menu-item "Make This Move the Root"
                   sgf-root-this-node
                   :enable ,(aref clicked-lnode 0))))
         (events (x-popup-menu last-input-event menu)))
  (if (functionp (car events))
      (funcall (car events) clicked-lnode))))


(defun sgf-find-and-goto-node (xy game-state)
  "Find the node at position XY from the current game state, searching backwards.
There could be multiple nodes at the same position during the game; this
function finds the closest one to the current game state.

Returns node found or nil if not."
  (let* ((board-2d  (aref game-state 1))  ;; Extract the current board
         (stone (sgf-board-2d-get xy board-2d))  ;; Get the stone at the XY position
         curr-lnode found-node)
    (while (not found-node)  ;; Loop until node is found or root is reached
      (setq curr-lnode (aref game-state 0))  ;; Get the current linked node
      (let* ((curr-node (aref curr-lnode 1))  ;; Extract the SGF node data
             (play (sgf-process-move curr-node))  ;; Process the current move
             (stone-i (car play))  ;; Stone placed in this node
             (xy-i (cdr play)))  ;; Coordinates of the move
        (if (and (equal stone-i stone) (equal xy-i xy))  ;; Check if it's the node we're looking for
            (setq found-node curr-lnode)  ;; Node found
          (if (null (aref curr-lnode 0))  ;; If we reach the root node, stop the loop
              (error "No move is found at position %S." xy)
            (sgf-backward-move)))))  ;; Move to the previous node
    found-node))  ;; Return the found node, or nil if not found


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
    (sgf-update-display ov)))


(defmacro sgf--add-mark (shape add-mark-func)
  "Add a mark to the current node."
  `(let* ((ov (sgf-get-overlay))
          (svg  (overlay-get ov 'svg))
          (game-state (overlay-get ov 'game-state))
          (board-2d (aref game-state 1))
          (curr-lnode (aref game-state 0))
          (curr-node (aref curr-lnode 1))
          (xy (sgf-mouse-event-to-xy last-input-event))
          (x (car xy)) (y (cdr xy))
          (xy-state (aref (aref board-2d y) x)))
     (push curr-node (list shape xy))
     (apply add-mark-func svg x y xy-state)))


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
;;          (sgf-svg-interval (car (overlay-get ov 'svg-params)))
;;          (game-state (overlay-get ov 'game-state))
;;          (curr-lnode  (aref game-state 0))
;;          (board-2d   (aref game-state 2))
;;          (xy (sgf-mouse-event-to-xy last-input-event))
;;          (x (car xy)) (y (cdr xy))
;;          (pos-state (aref (aref board-2d y) x)))
;;     (if (and (eq last-command 'mouse-drag-region)
;;              (not (equal pos-state 'E)))
;;         (progn
;;           (aset (aref board-2d y) x 'E)
;;           (sgf-svg-add-stone svg sgf-svg-interval x y 'E)
;;           (overlay-put ov 'display (svg-image svg :map hot-areas))
;;           (overlay-put ov 'svg svg)))))

(defun sgf-toggle-svg-display (&optional choice)
  "Toggle graphical. Keep the overlay."
  (interactive)
  (let ((ov (or (sgf-get-overlay) (sgf-setup-overlay))))
    (cond ((equal choice 'hide)
           (sgf--hide-svg ov))
          ((equal choice 'show)
           (sgf--display-svg ov))
          (t (if (null (overlay-get ov 'display))
                 (sgf--display-svg ov)
               (sgf--hide-svg ov))))))


(defun sgf-game-state-from-buffer ()
  "Create game-state (stay at the root) and return."
  (let* ((game-tree (sgf-str-to-game-tree (buffer-string)))
         (root (car game-tree))
         (root-lnode (sgf-linked-node nil root nil))
         (w-h (car (alist-get 'SZ root)))
         (w (car w-h)) (h (cdr w-h))
         (turn (car (alist-get 'PL root)))
         (board-2d (sgf-create-board-2d w h 'E)))
    ;; root state
    (sgf-linkup-nodes-in-game-tree (cdr game-tree) root-lnode)

    ;; process root node to add setup stones
    (dolist (prop root)
      (let* ((prop-key (car prop))
             (prop-vals (cdr prop))
             setup-stone)
        (cond ((member prop-key '(AB AW))
               (setq setup-stone (intern (substring (symbol-name prop-key) 1)))
               (dolist (xy prop-vals)
                 (sgf-board-2d-set xy setup-stone board-2d))))))
    ;; return game-state
    (sgf-game-state root-lnode board-2d nil turn)))


(defun sgf-setup-overlay ()
  "Create overlay and setup overlay properties."
  ;; set front- and rear-advance parameters to allow
  ;; the overlay cover the whole buffer even if it is
  ;; updated from game playing.
  (let* ((ov (make-overlay (point-min) (point-max) nil nil t))
         (game-state (sgf-game-state-from-buffer))
         (board-2d   (aref game-state 1))
         (h (length board-2d))
         (w (length (aref board-2d 0)))
         (svg-hot-areas (sgf-svg-init w h
                                      sgf-show-move-number
                                      sgf-show-next-hint
                                      sgf-show-mark))
         (svg (car svg-hot-areas))
         (hot-areas (cdr svg-hot-areas)))

    (sgf-update-svg game-state svg)

    ;; label this overlay to distinguish from others
    (overlay-put ov 'sgf-overlay t)
    ;; game state
    (overlay-put ov 'game-state game-state)
    ;; game properties
    (overlay-put ov 'game-plist
                 (list :show-next-hint sgf-show-next-hint
                       :show-move-number sgf-show-move-number
                       :show-mark sgf-show-mark
                       :allow-suicide-move sgf-allow-suicide-move
                       :editable t))
    (overlay-put ov 'svg svg)
    (overlay-put ov 'hot-areas hot-areas)
    ov))



(defun sgf-get-overlay-at (&optional pos)
  "Return the SGF overlay at POS position in the current buffer."
  (let* ((pos (or pos (point)))
         (ovs (overlays-in (1- pos) (1+ pos)))
         sgf-ov)
    (while (and ovs (not sgf-ov))
      (let ((ov (pop ovs)))
        (if (overlay-get ov 'sgf-overlay)
            (setq sgf-ov ov))))
    sgf-ov))


(defun sgf-get-overlay ()
  "Return the SGF overlay (even if mouse clicked on non current buffer)."
  (if (or (mouse-event-p last-input-event)
          (memq (event-basic-type last-input-event) '(wheel-up wheel-down)))
      (let* ((mouse-pos (event-start last-input-event))
             (pos    (posn-point mouse-pos))
             (window (posn-window mouse-pos))
             (buffer (window-buffer window)))
        (set-window-point window pos)
        (with-current-buffer buffer
          (sgf-get-overlay-at pos)))
    (sgf-get-overlay-at)))


(defun sgf-game-property-get (ov key)
  "Return game property of KEY"
  (let ((game-plist (overlay-get ov 'game-plist)))
    (plist-get game-plist key)))


(defun sgf-game-property-set (ov key value)
  (let ((game-plist (overlay-get ov 'game-plist)))
    (plist-put game-plist key value)))


(defun sgf-game-property-toggle (ov key)
  "Toggle the game property of KEY."
  (let ((game-plist (overlay-get ov 'game-plist)))
    (sgf-game-property-set ov key (not (plist-get game-plist key)))))


(defun sgf--display-svg (ov)
  "Display SVG in the overlay (as well as setting up keyboard)."
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


(defun sgf-menu ()
  "Show the main menu for the SGF mode."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (menu `(keymap "Main Menu"
                        (sgf-toggle-move-number
                         menu-item "Show Move Number"
                         sgf-toggle-move-number
                         :button
                         (:toggle . (sgf-game-property-get ,ov :show-move-number)))
                        (sgf-toggle-next-hint
                         menu-item "Show Next Hint"
                         sgf-toggle-next-hint
                         :button
                         (:toggle . (sgf-game-property-get ,ov :show-next-hint)))
                        (sgf-toggle-marks
                         menu-item "Show Marks"
                         sgf-toggle-marks
                         :button
                         (:toggle . (sgf-game-property-get ,ov :show-mark)))
                        (seperator-1 menu-item "--")
                        (sgf-toggle-allow-suicide-move
                         menu-item "Allow Illegal Move"
                         sgf-toggle-allow-suicide-move
                         :button
                         (:toggle . (sgf-game-property-get ,ov :allow-suicide-move)))
                        (seperator-2 menu-item "--")
                        (sgf-edit-move-number
                         menu-item "Edit Move Number"
                         sgf-edit-move-number)
                        (sgf-edit-comment
                         menu-item "Edit Comment"
                         sgf-edit-comment)
                        (seperator-3 menu-item "--")
                        (sgf-export-image
                         menu-item "Export Image"
                         sgf-export-image)
                        (sgf-mark-edit-mode
                         menu-item "Mark Edit Mode"
                         sgf-mark-edit-mode
                         :button)))
         (events (x-popup-menu last-input-event menu)))
    (if (functionp (car events))
        (funcall (car events)))))


(defvar sgf-mode-graphical-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'sgf-forward-move)
    (define-key map [hot-forward mouse-1] 'sgf-forward-move)
    (define-key map "b" 'sgf-backward-move)
    (define-key map [hot-backward mouse-1] 'sgf-backward-move)
    (define-key map "a" 'sgf-first-move)
    (define-key map [hot-first mouse-1] 'sgf-first-move)
    (define-key map "e" 'sgf-last-move)
    (define-key map [hot-last mouse-1] 'sgf-last-move)
    (define-key map "g" 'sgf-goto-move)
    (define-key map "n" 'sgf-toggle-move-number)
    (define-key map "m" 'sgf-toggle-marks)
    (define-key map "v" 'sgf-view-next)
    (define-key map "k" 'sgf-kill-node)
    (define-key map "s" 'sgf-export-svg)
    (define-key map "c" 'sgf-edit-comment)
    (define-key map "i" 'sgf-edit-game-info)
    (define-key map [hot-grid mouse-1] #'sgf-board-click-left)
    (define-key map [hot-grid mouse-3] #'sgf-board-click-right)
    (define-key map [hot-menu mouse-1] #'sgf-menu) ; todo
    (define-key map "p" 'sgf-pass)
    ;; (define-key map (kbd "m s")  'sgf-add-mark-square)
    ;; (define-key map 'sgf-add-mark-triangle)
    ;; (define-key map 'sgf-add-mark-circle)
    ;; (define-key map 'sgf-add-mark-cross)
    ;; (define-key map 'sgf-add-mark-label)
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

;;; Local Variables:
;;; lisp-indent-function: common-lisp-indent-function
;;; End:
