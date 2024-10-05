;;; sgf-mode.el --- SGF Major Mode  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: SGF, go, game

;;; Code:

(require 'sgf-game)
(require 'sgf-svg)
(require 'sgf-write)


(defvar sgf-allow-suicide-move nil
  "Allow suicide or not. Some rule set allow suicide: https://senseis.xmp.net/?Suicide")

(defvar sgf-show-next-hint t
  "Show the hint mark(s) for next move(s).")

(defvar sgf-show-move-number t
  "Show move number on the stones.")

(defvar sgf-show-mark t
  "Show marks on the board.")


(defun sgf-root-p (lnode)
  "Check if LNODE is the root node."
  (null (aref lnode 0)))


(defun sgf-valid-stone-p (stone)
  "Check if STONE is a valid color."
  (or (equal stone 'B) (equal stone 'W)))


(defun sgf-xy-on-board-p (xy board-2d)
  "Check if XY is on the board."
  (let ((x (car xy)) (y (cdr xy)))
    (and (>= x 0) (< x (length (aref board-2d 0)))
         (>= y 0) (< y (length board-2d)))))


(defun sgf-xy-is-empty-p (xy board-2d)
  "Check if XY is empty on the board."
  (equal (sgf-game-board-get xy board-2d) 'E))


(defun sgf-valid-move-p (xy stone game-state &optional allow-suicide)
  "Check if the move of STONE at XY position on BOARD-2D is valid"
  (let* ((board-2d (aref game-state 1))
         (ko (aref game-state 2)))
    (and
     xy board-2d
     (sgf-valid-stone-p stone)       ;; valid color
     (sgf-xy-on-board-p xy board-2d) ;; position is on board
     (sgf-xy-is-empty-p xy board-2d) ;; no stone at this position yet
     (not (equal xy ko))             ;; pos is not ko
     (if (not allow-suicide) (not (sgf-suicide-stones xy board-2d)) ;; not suicide move
       ))))


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
            (sgf-game-board-get pos board-2d))
          (sgf-neighbors-xy xy board-2d)))


(defun sgf-check-liberty (xy board-2d &optional last-color visited)
  "Check if the stone on position XY of BOARD-2D is alive or suiciding.

It returns a cons cell of the form (LIBERTY-FOUND . VISITED). Its car
indicates if a liberty is found, and its cdr is a list of visited
positions to avoid loops, which also stores all the dead positions if no
liberty is found.

If xy is nil (for a move of pass), it returns (t . nil)."
  (let ((curr-color (sgf-game-board-get xy board-2d)))
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
  (let ((curr-color (sgf-game-board-get xy board-2d))
        (prisoners nil))
    (dolist (neighbor-xy (sgf-neighbors-xy xy board-2d))
      (when (and neighbor-xy
                 (not (member neighbor-xy prisoners))
                 (not (equal curr-color (sgf-game-board-get neighbor-xy board-2d))))
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
          (setq state-i (sgf-game-board-get xy-i board-2d))
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


(defun sgf-update-display (ov)
  "Update the svg object of the game state."
  (let* ((game-state (overlay-get ov 'game-state))
         (svg (overlay-get ov 'svg))
         (hot-areas (overlay-get ov 'hot-areas))
         (board-2d   (aref game-state 1))
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
    (sgf-svg-update-marks svg curr-node board-2d)
    (overlay-put ov 'display (svg-image svg :map hot-areas))))


(defun sgf-push-undo (change game-state)
  "Push a game CHANGE to the undo stack in GAME-STATE."
  (let ((undos (aref game-state 5)))
    (aset game-state 5 (cons change undos))))

(defun sgf-pop-undo (game-state)
  "Pop a game CHANGE from the undo stack in GAME-STATE"
  (let ((undos (aref game-state 5)))
    (if undos
        (let ((change (car undos)))
          (aset game-state 5 (cdr undos))
          change)
      nil)))


(defun sgf-revert-undo (change game-state)
  "Revert the CHANGE for the GAME-STATE"
  (let ((black-xys (aref change 0))
        (white-xys (aref change 1))
        (empty-xys (aref change 2))
        (ko (aref change 3))
        (turn (aref change 4))
        (board-2d (aref game-state 1))
        (pcounts  (aref game-state 4)))
    (dolist (xy black-xys) (sgf-game-board-set xy 'B board-2d))
    (dolist (xy white-xys) (sgf-game-board-set xy 'W board-2d))
    (dolist (xy empty-xys) (sgf-game-board-set xy 'E board-2d))
    (aset game-state 3 turn)
    (aset game-state 2 ko)
    (setcar pcounts (- (car pcounts) (length black-xys)))
    (setcdr pcounts (- (cdr pcounts) (length white-xys)))))


(defun sgf-branch-selection (n &optional branch)
  "Prompt the user to select a branch by choosing a character."
  (let ((prompt (format "Select a branch (a-%c): " (+ ?a (1- n)))))
    (if (null branch)
        (setq branch (if (= n 1) 0 (- (read-char prompt) ?a))))
    (if (and (>= branch 0) (< branch n))
        branch
      (error "Invalid branch selection: %c" (+ branch ?a)))))

(defun sgf-forward-move (&optional branch interactive-call)
  "Move to the next move in the game tree and update board.

See also `sgf-branch-selection'."
  ;; Since branch is only used when called non-interactively, use
  ;; code`i' to skip assignment for variable branch when called
  ;; interactively (ie, branch value will be nil).

  ;; Use code `p' to check if the function is call interactively. When
  ;; called non-interactively, interactive-call will be nil; otherwise
  ;; it is 1
  (interactive "i\np")
  (let* ((ov         (sgf-get-overlay))
         (svg        (overlay-get ov 'svg))
         (game-state  (overlay-get ov 'game-state))
         (curr-lnode  (aref game-state 0))
         (next-lnodes (aref curr-lnode 2))
         (n           (length next-lnodes))
         next-lnode next-node)
    (if (= n 0)
        (progn (message "No more next move.") nil)
      (setq branch (sgf-branch-selection n branch))
      (setq next-lnode (nth branch next-lnodes))
      (setq next-node  (aref next-lnode 1))
      (sgf-show-comment next-node)
      (sgf-apply-node next-node game-state)
      (aset game-state 0 next-lnode)
      ;; return t if it is a noninteractive call, to indicate a
      ;; successful forward move.
      (if interactive-call (sgf-update-display ov) t))))


(defun sgf-forward-fork (&optional interactive-call)
  "Move to the step before the next fork."
  (interactive "p")
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (continue t))
    (while continue
      (let* ((curr-lnode (aref game-state 0))
             (lnodes (aref curr-lnode 2))
             (n (length lnodes)))
        (if (= n 1)
            (sgf-forward-move)
          (setq continue nil))))
    (if interactive-call (sgf-update-display ov))))


(defun sgf-backward-move (&optional interactive-call)
  "Move to the previous move in the game tree and update board."
  (interactive "p")
  (let* ((ov         (sgf-get-overlay))
         (game-state  (overlay-get ov 'game-state))
         (curr-lnode  (aref game-state 0))
         (prev-lnode  (aref curr-lnode 0)))
    (if (sgf-root-p curr-lnode)
        (progn (message "No more previous play.") nil)
      (sgf-show-comment (aref prev-lnode 1))
      (sgf-revert-undo (sgf-pop-undo game-state) game-state)
      (aset game-state 0 prev-lnode)
      (if interactive-call (sgf-update-display ov) t))))


(defun sgf-backward-fork (&optional interactive-call)
  "Move to the step before the previous fork."
  (interactive "p")
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (continue t))
    (while continue
      (let* ((curr-lnode (aref game-state 0))
             (prev-lnode (aref curr-lnode 0))
             sibling-lnodes)
        (if prev-lnode
            (setq sibling-lnodes (aref prev-lnode 2)))
        (sgf-backward-move)
        (if (/= (length sibling-lnodes) 1)
            (setq continue nil))))
    (if interactive-call (sgf-update-display ov))))


(defun sgf-apply-node (node game-state)
  "Apply the node of move to the game state."
  (let* ((move (sgf-process-move node))
         (stone (car move))
         (xy (cdr move))
         (board-2d (aref game-state 1))
         (xy-state-old (sgf-game-board-get xy board-2d))
         (turn-old (aref game-state 3))
         (turn-new (sgf-enemy-stone turn-old))
         (ko-old (aref game-state 2))
         (pcounts (aref game-state 4))
         black-xys white-xys empty-xys)
    ;; check it is legal move before make any change to game state
    (unless (sgf-valid-move-p xy stone game-state sgf-allow-suicide-move)
      (error "Invalid move of %S at %S" stone xy))
    (sgf-game-board-set xy stone board-2d)
    (setq prisoners (sgf-capture-stones xy board-2d))
    ;; Remove captured stones
    (dolist (xy prisoners) (sgf-game-board-set xy 'E board-2d))
    ;; Check for KO: this code needs to be put after prisoners are removed.
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


(defun sgf-first-move (&optional interactive-call)
  "Move to the first node in the game tree."
  (interactive "p")
  (while (sgf-backward-move))
  (if interactive-call (sgf-update-display (sgf-get-overlay))))


(defun sgf-last-move (&optional branch interactival-call)
  "Move to the last node in the game tree.

See also `sgf-forward-move'."
  (interactive "i\np")
  (while (sgf-forward-move branch))
  (if interactival-call (sgf-update-display (sgf-get-overlay))))


(defun sgf-jump-moves (n &optional branch interactive-call)
  "Move forward or backward N nodes.

It pauses at fork and wait for user input to select a branch.
See also `sgf-forward-move'."
  (interactive "nMove _ plays forward (pos number) or backward (neg number): \ni\np")
  (if (> n 0)
      (dotimes (i n) (sgf-forward-move branch))
    (dotimes (i (- n)) (sgf-backward-move)))
  (if interactive-call (sgf-update-display (sgf-get-overlay))))




(defun sgf--toggle-layer(layer)
  "Toggle the display of a give layer."
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (svg  (overlay-get ov 'svg))
         group)
    (cond ((equal layer 'mvnum)
           (sgf-game-plist-toggle ov :show-move-number)
           (setq group (sgf-svg-group-mvnums svg)))
          ((equal layer 'mark)
           (sgf-game-plist-toggle ov :show-mark)
           (setq group (sgf-svg-group-marks svg)))
          ((equal layer 'next)
           (sgf-game-plist-toggle ov :show-next-hint)
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
  (interactive "FExport svg to file: ")
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


(defun sgf-edit-move-number (&optional lnode)
  "Edit the move number of the given node or current node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (lnode (or lnode (aref game-state 0)))
         (node  (aref lnode 1))
         (old-mvnum  (car (alist-get 'MN node)))
         new-mvnum valid)
    (while (not valid)
      (setq new-mvnum (read-number "Move number: " old-mvnum))
      (if (not (integerp new-mvnum))
          (message "Invalid move number. Please enter an integer.")
        (setq valid t)))
    (unless (equal old-mvnum new-mvnum)
      (aset lnode 1
            (nconc (assq-delete-all 'MN node)
                   (list (list 'MN new-mvnum))))
      (sgf-update-display ov)
      (sgf-write-game-to-buffer lnode (overlay-buffer ov)))))


(defun sgf-edit-comment (&optional lnode)
  "Edit the comment of the given node or current node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (lnode (or lnode (aref game-state 0)))
         (node (aref lnode 1))
         ;; C[foo][spam] -> "foo spam"
         (old-comment (mapconcat 'identity (alist-get 'C node) " "))
         (new-comment (read-string "Edit comment: " old-comment)))
    ;; only update if the comment is changed
    (unless (string= old-comment new-comment)
      ;; Update or remove the 'C' property based on new-comment
      (aset lnode 1
            (if (string-empty-p new-comment)
                ;; delete the comment property if the new comment is empty
                (assq-delete-all 'C node)
              (nconc (assq-delete-all 'C node)
                     (list (list 'C new-comment)))))
      (sgf-write-game-to-buffer lnode (overlay-buffer ov)))))


;; igo-editor-move-mode-make-move-root
;; todo
(defun sgf-root-node ()
  "Make the current node the root node.
1. move all the nodes in between to the root node of setup.
2. change B, W to AB, AW"
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0)))))


(defun sgf-prune ()
  "Delete all its children of the current node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0)))
    (aset curr-lnode 2 nil)
    (sgf-write-game-to-buffer curr-lnode (overlay-buffer ov))))


(defun sgf-prune-inclusive ()
  "Delete the current node and all its children."
  (interactive)
  (sgf-backward-move)
  (sgf-prune))

;; todo
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
  ;; mouse-1 event for the left click
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
             (new-lnode (sgf-game-linked-node curr-lnode `((,turn ,xy)))))
         ;; add the new node as the last branch
         (aset curr-lnode 2 (append next-lnodes (list new-lnode)))
         (sgf-forward-move n) ; if n=0, case 2.1; otherwise, case 2.2
         (sgf-write-game-to-buffer curr-lnode (overlay-buffer ov))))
      ;; Case 3.
      (t (message "Illegal move!")))))


(defun sgf-board-click-right ()
  "Right click on board."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (xy (sgf-mouse-event-to-xy last-input-event))
         (clicked-lnode (sgf-find-node xy game-state))
         (menu `("ACTION ON THIS MOVE"
                 ["Edit Comment"
                  ,(lambda () (interactive) (sgf-edit-comment clicked-lnode))]
                 ["Edit Move Number"
                  ,(lambda () (interactive) (sgf-edit-move-number clicked-lnode))
                  :enable ,(not (sgf-root-p clicked-lnode))]
                 ["Back to This Move"
                  ,(lambda () (interactive) (sgf-goto-node clicked-lnode))
                  :help "Move game state to this move"
                  :enable ,(not (equal curr-lnode clicked-lnode))]
                 ["Prune to This Move"
                  ,(lambda () (interactive) (sgf-goto-node clicked-lnode) (sgf-prune))
                  :enable ,(not (sgf-root-p clicked-lnode))] ; not root node
                 ["Put as Setup"
                  ,(lambda () (interactive) (sgf-root-node))
                  :enable ,(not (sgf-root-p clicked-lnode))])))
    (popup-menu menu)))


(defun sgf-goto-node (lnode)
  "Move game state to the given LNODE and update svg board."
  (let* ((ov  (sgf-get-overlay))
         (game-state  (overlay-get ov 'game-state))
         found)
    (while (not found)
      (let* ((curr-lnode  (aref game-state 0))
             (prev-lnode  (aref curr-lnode 0)))
        (if (equal curr-lnode lnode)
            (setq found t)
          (progn
            (sgf-revert-undo (sgf-pop-undo game-state) game-state)
            (aset game-state 0 prev-lnode)))))
    (sgf-update-display ov)))

(defun sgf-find-node (xy game-state)
  "Search backward from the current game state to find the node that put
the corresponding stone at XY on board. There could be multiple nodes at
the same position during the whole game; this function finds the closest
one to the current game state.

Returns linked node found or nil if not. The game-state remains unchanged."
  (let* ((board-2d  (aref game-state 1))  ;; Extract the current board
         (stone (sgf-game-board-get xy board-2d))  ;; Get the stone at the XY position
         (curr-lnode (aref game-state 0))
         found-lnode)
    (while (not found-lnode)  ;; Loop until node is found or root is reached
      (let* ((curr-node (aref curr-lnode 1))  ;; Extract the SGF node data
             (play (sgf-process-move curr-node))  ;; Process the current move
             (stone-i (car play))  ;; Stone placed in this node
             (xy-i (cdr play)))  ;; Coordinates of the move
        (if (and (equal stone-i stone) (equal xy-i xy))  ;; Check if it's the node we're looking for
            (setq found-lnode curr-lnode)  ;; Node found
          (if (null (aref curr-lnode 0))  ;; If we reach the root node, stop the loop
              (error "No move is found at position %S." xy)
            (setq curr-lnode (aref curr-lnode 0))))))  ;; Move to the previous node
    found-lnode))  ;; Return the found node, or nil if not found

;; todo: to finish
(defun sgf-pass ()
  "Pass the current. Add a new node of W[] or B[].

The move number will be incremented."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (next-lnodes (aref curr-lnode 2))
         (new-lnode (sgf-game-linked-node curr-lnode '((W)))))
    (aset curr-lnode 2 (append next-lnodes (list new-lnode)))
    (sgf-update-display ov)))


(defun sgf--handle-mouse-input (action-fn message-text)
  "Generalized handler for mouse input, calling ACTION-FN for specific actions."
  ;; Define a transient keymap that captures mouse clicks on the hot grid
  (let ((map (make-sparse-keymap)))
    ;; Handle mouse clicks
    (define-key map [hot-grid mouse-1] action-fn)
    ;; Use transient map and exit on any key press or mouse event outside [hot-grid mouse-1]
    (set-transient-map
     map
     ;; type C-g to exit map
     (lambda () (not (equal last-input-event ?\C-g)))
     ;; show exit message
     (lambda () (message "Exited edit mode."))
     (concat message-text "Type C-g to exit."))))


(defun sgf--action-setup-stone (event stone)
  "Add or delete setup stones."
  (let* ((xy (sgf-mouse-event-to-xy event))
         (ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (board-2d (aref game-state 1))
         (curr-lnode (aref game-state 0))
         (curr-node (aref curr-lnode 1))
         (prop-key (if (eq stone 'B) 'AB 'AW))   ;; 'AB for black stones, 'AW for white stones
         (prop (assoc prop-key curr-node))
         (xys (cdr prop)))
    (if (sgf-root-p curr-lnode)
        (let ((xys (if (member xy xys)
                       (progn (sgf-game-board-set xy 'E board-2d)
                              (delete xy xys))  ;; Remove stone from the list
                     (progn (sgf-game-board-set xy stone board-2d)
                            (nconc xys (list xy))))))
          (if xys
              (if prop
                  (setcdr prop xys)  ; Update existing entry
                (nconc curr-node (list (list prop-key xys)))) ; Add new property entry
            ;; If the xy list is empty, remove the property entirely
            (setq curr-node (assq-delete-all prop-key curr-node)))

          (sgf-update-display ov)
          (format "Edited %s stone at %s" stone xy))
      (error "Cannot edit setup stones on a non-root node. Move to the beginning of the game with `sgf-first-move'"))))


(defun sgf-edit-setup-black-stone ()
  (interactive)
  (sgf--handle-mouse-input
   ;; Create a closure that captures the value of `stone`
   (lambda (event) (interactive "e") (sgf--action-setup-stone event 'B))
   "Click on the board to edit black stones. "))


(defun sgf-edit-setup-white-stone ()
  (interactive)
  (sgf--handle-mouse-input
   ;; Create a closure that captures the value of `stone`
   (lambda (event) (interactive "e") (sgf--action-setup-stone event 'W))
   "Click on the board to edit white stones. "))


(defun sgf--action-mark (event shape)
  (let* ((xy (sgf-mouse-event-to-xy event))
         (ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (curr-node (aref curr-lnode 1))
         (curr-mark (assoc shape curr-node)))
    (if curr-mark
        ;; If the shape exists, check if the coordinate already exists in the mark list
        (if (member xy (cdr curr-mark))
            ;; If the coordinate exists, delete it
            (progn
              (setcdr curr-mark (delete xy (cdr curr-mark)))
              (message "Deleted mark at %s" xy)
              ;; If no coordinates are left for this mark, remove the mark entirely
              (unless (cdr curr-mark)
                (setq curr-node (assq-delete-all shape curr-node))))
          ;; Otherwise, add the coordinate
          (message "Added mark at %s" xy)
          (setcdr curr-mark (nconc (cdr curr-mark) (list xy))))
      ;; If the mark doesn't exist, add it
      (message "Added mark at %s" xy)
      (nconc curr-node (list (cons shape (list xy)))))
    ;; Update the display
    (sgf-update-display ov)))


(defun sgf-edit-mark-square ()
  "Add/delete a square mark on the board of current game state."
  (interactive)
  (sgf--handle-mouse-input
   ;; Create a closure that captures the value of `stone`
   (lambda (event) (interactive "e") (sgf--action-mark event 'SQ))
   "Click on the board to edit square mark. "))

(defun sgf-edit-mark-triangle ()
  "Add/delete a triangle mark on the board of current game state."
  (interactive)
  (sgf--handle-mouse-input
   ;; Create a closure that captures the value of `stone`
   (lambda (event) (interactive "e") (sgf--action-mark event 'TR))
   "Click on the board to edit triangle mark. "))

(defun sgf-edit-mark-circle ()
  "Add/delete a circle mark on the board of current game state."
  (interactive)
  (sgf--handle-mouse-input
   ;; Create a closure that captures the value of `stone`
   (lambda (event) (interactive "e") (sgf--action-mark event 'CR))
   "Click on the board to edit circle mark. "))

(defun sgf-edit-mark-cross ()
  "Add/delete a cross mark on the board of current game state."
  (interactive)
  (sgf--handle-mouse-input
   ;; Create a closure that captures the value of `stone`
   (lambda (event) (interactive "e") (sgf--action-mark event 'MA))
   "Click on the board to edit cross mark. "))


(defun sgf--action-mark-label (event)
  "Add, edit, or delete a label mark on the board of current game state."
  (interactive "e")
  (let* ((xy (sgf-mouse-event-to-xy event))
         (ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (curr-node (aref curr-lnode 1))
         (curr-mark (assoc 'LB curr-node))   ;; Check if 'LB (label) mark already exists
         (xy-label (assoc xy (cdr curr-mark))) ;; Check for existing label at xy
         (old-txt (if xy-label (cdr xy-label) "")) ;; Old label if exists
         (new-txt (read-string "Label: " old-txt))) ;; Prompt user for input
    ;; Only update if the label is changed
    (unless (string= old-txt new-txt)
      (if (string-empty-p new-txt)
          ;; If the new label is empty, delete the label at xy
          (progn
            (when xy-label
              (setcdr curr-mark (delete xy-label (cdr curr-mark)))
              (message "Deleted label at %s" xy)
              ;; If no labels remain, remove the 'LB' mark entirely
              (when (null (cdr curr-mark))
                (setq curr-node (assq-delete-all 'LB curr-node)))))
        ;; Otherwise, add or update the label
        (if xy-label
            ;; Update the existing label
            (setcdr xy-label new-txt)
          ;; Add a new label
          (if curr-mark
              (setcdr curr-mark (nconc (cdr curr-mark) (list (cons xy new-txt))))
            (nconc curr-node (list (cons 'LB (list (cons xy new-txt)))))))
        (message "Updated label at %s with text '%s'" xy new-txt)))
    ;; Update the display
    (sgf-update-display ov)))


(defun sgf-edit-mark-label ()
  "edit a label mark on the board of current game state."
  (interactive)
  (sgf--handle-mouse-input
   'sgf--action-mark-label
   "Click on the board to edit label. "))


(defun sgf--action-delete-mark (event)
  "Delete a mark from the current node."
  (interactive "e")
  (let* ((xy (sgf-mouse-event-to-xy event))
         (ov  (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (curr-node  (aref curr-lnode 1)))
    (dolist (shape '(SQ TR CR MA))
      (let ((curr-mark (assoc shape curr-node)))
        (when (and curr-mark (member xy curr-mark))
          (delete xy curr-mark)
          (message "Deleted mark at %s" xy)
          (sgf-update-display ov))))))

(defun sgf-delete-mark ()
  "Delete a mark of any shape from the current node."
  (interactive)
  (sgf--handle-mouse-input
   'sgf--action-delete-mark
   "Click on the board to delete marks. "))


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

(defun sgf-toggle-svg-display (keep &optional beg end)
  "Toggle graphical display.

If BEG and END are nil, parse the whole buffer as SGF content. If KEEP
is non-nil, just hide display and do not remove the old overlay;
otherwise, delete and create new overlay."
  (interactive "P")
  (let* ((ov (sgf-get-overlay)))
    (if (and keep ov)
        (if (overlay-get ov 'display)
            (sgf--hide-svg ov)
          (sgf--display-svg ov))
      (sgf-setup-overlay (or beg (point-min))
                         (or end (point-max))))))


(defun sgf-setup-overlay (beg end)
  "Create overlay and setup overlay properties."
  ;; set front- and rear-advance parameters to allow
  ;; the overlay cover the whole buffer even if it is
  ;; updated from game playing.
  (remove-overlays beg end)
  (let* ((ov (make-overlay beg end nil nil t))
         (game-state (sgf-game-from-buffer beg end))
         (board-2d   (aref game-state 1))
         (h (length board-2d))
         (w (length (aref board-2d 0)))
         (svg-hot-areas (sgf-svg-init w h
                                      sgf-show-move-number
                                      sgf-show-next-hint
                                      sgf-show-mark))
         (svg (car svg-hot-areas))
         (hot-areas (cdr svg-hot-areas)))

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
    (overlay-put ov 'keymap sgf-mode-graphical-map)
    (sgf-update-display ov)
    ov))



(defun sgf-get-overlay-at (&optional pos)
  "Return the SGF overlay at POS position in the current buffer."
  (let* ((pos (or pos (point)))
         (ovs (overlays-in (1- pos) (1+ pos)))
         sgf-ov)
    (while (and ovs (not sgf-ov))
      (let ((ov (pop ovs)))
        ;; make sure get the right overlay
        (if (overlay-get ov 'game-state)
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


(defun sgf--hide-svg (ov)
  (overlay-put ov 'display nil)
  (overlay-put ov 'keymap nil))


(defun sgf--display-svg (ov)
  "Display SVG in the overlay (as well as setting up keyboard)."
  (let ((svg (overlay-get ov 'svg))
        (hot-areas (overlay-get ov 'hot-areas)))
    (unless (and svg hot-areas)
      (error "Overlay %S does not have 'svg' or 'hot-areas' properties" ov))
    (overlay-put ov 'keymap sgf-mode-graphical-map)
    (overlay-put ov 'display (svg-image svg :map hot-areas))))


;; (defun sgf-redo-overlay ()
;;   "Delete old overlay and create and return a new one."
;;   ;(remove-overlays) ; remove all the overlays in the buffer
;;   (let ((new-ov (make-overlay (point-min) (point-max) (current-buffer) nil t)))
;;     (sgf-setup-overlay new-ov)
;;     new-ov))


(defun sgf-game-plist-get (ov key)
  "Return game property of KEY"
  (let ((game-plist (overlay-get ov 'game-plist)))
    (plist-get game-plist key)))


(defun sgf-game-plist-set (ov key value)
  (let ((game-plist (overlay-get ov 'game-plist)))
    (plist-put game-plist key value)))


(defun sgf-game-plist-toggle (ov key)
  "Toggle the game property of KEY."
  (let ((game-plist (overlay-get ov 'game-plist)))
    (sgf-game-plist-set ov key (not (plist-get game-plist key)))))


(defun sgf-menu ()
  "Show the main menu for the SGF mode."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (menu-keymap
          ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Menu-Example.html
          `(keymap "Main Menu"
                   (sgf-toggle-svg-display menu-item "Diable SVG Display" sgf-toggle-svg-display)
                   (sgf-toggle-allow-suicide-move
                    menu-item "Allow Suicide Move"
                    sgf-toggle-allow-suicide-move
                    :button (:toggle . (sgf-game-plist-get ,ov :allow-suicide-move)))
                   (sgf-toggle-move-number ; key symbol
                    menu-item "Show Move Number"
                    sgf-toggle-move-number
                    :button (:toggle . (sgf-game-plist-get ,ov :show-move-number)))
                   (sgf-toggle-next-hint
                    menu-item "Show Next Hint"
                    sgf-toggle-next-hint
                    :button (:toggle . (sgf-game-plist-get ,ov :show-next-hint)))
                   (sgf-toggle-marks
                    menu-item "Show Marks"
                    sgf-toggle-marks
                    :button (:toggle . (sgf-game-plist-get ,ov :show-mark)))
                   (seperator-1 menu-item "--")
                   (seperator-2 menu-item "--")
                   (sgf-edit-move-number menu-item "Edit Move Number" sgf-edit-move-number)
                   (sgf-edit-comment menu-item "Edit Comment" sgf-edit-comment)
                   (seperator-3 menu-item "--")
                   (sgf-edit-setup-black-stone menu-item "Edit Black Setup Stone" sgf-edit-setup-black-stone)
                   (sgf-edit-setup-white-stone menu-item "Edit White Setup Stone" sgf-edit-setup-white-stone)
                   (sgf-edit-mark-cross menu-item "Edit Cross Mark" sgf-edit-mark-cross)
                   (sgf-edit-mark-square menu-item "Edit Square Mark" sgf-edit-mark-square)
                   (sgf-edit-mark-triangle menu-item "Edit Triangle Mark" sgf-edit-mark-triangle)
                   (sgf-edit-mark-circle menu-item "Edit Circle Mark" sgf-edit-mark-circle)
                   (sgf-edit-mark-label menu-item "Edit Label" sgf-edit-mark-label)
                   (sgf-delete-mark menu-item "Delete Mark" sgf-delete-mark)
                   (seperator-4 menu-item "--")
                   (sgf-export-image menu-item "Export Image" sgf-export-image))))
    (popup-menu menu-keymap)))


(defvar sgf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'sgf-toggle-svg-display)
    map)
  "Keymap for SGF major mode.")


(defvar sgf-mode-graphical-map
  (let ((map (make-sparse-keymap)))
    ;; navigation functions
    (define-key map "f" 'sgf-forward-move)
    (define-key map [hot-forward mouse-1] 'sgf-forward-move)
    (define-key map (kbd "M-f") 'sgf-forward-fork)
    (define-key map "b" 'sgf-backward-move)
    (define-key map [hot-backward mouse-1] 'sgf-backward-move)
    (define-key map (kbd "M-b") 'sgf-backward-fork)
    (define-key map "a" 'sgf-first-move)
    (define-key map [hot-first mouse-1] 'sgf-first-move)
    (define-key map "e" 'sgf-last-move)
    (define-key map [hot-last mouse-1] 'sgf-last-move)
    (define-key map "j" 'sgf-jump-moves)
    ;; display/show functions
    (define-key map (kbd "s n") 'sgf-toggle-move-number)
    (define-key map (kbd "s m") 'sgf-toggle-marks)
    (define-key map (kbd "s h") 'sgf-toggle-next-hint)
    ;; modify functions
    (define-key map (kbd "m k") 'sgf-prune-inclusive) ; kill node
    (define-key map (kbd "m K") 'sgf-prune)
    (define-key map (kbd "m c") 'sgf-edit-comment)
    (define-key map (kbd "m n") 'sgf-edit-move-number)
    (define-key map (kbd "m a") 'sgf-edit-mark-triangle)
    (define-key map (kbd "m d") 'sgf-edit-mark-square)
    (define-key map (kbd "m o") 'sgf-edit-mark-circle)
    (define-key map (kbd "m x") 'sgf-edit-mark-cross)
    (define-key map (kbd "m l") 'sgf-edit-mark-label)
    (define-key map (kbd "m -") 'sgf-delete-mark)
    (define-key map (kbd "m b") 'sgf-edit-setup-black-stone)
    (define-key map (kbd "m w") 'sgf-edit-setup-white-stone)
    (define-key map (kbd "m i") 'sgf-edit-game-info)
    (define-key map "z" 'sgf-export-image)
    (define-key map [hot-grid mouse-1] #'sgf-board-click-left)
    (define-key map [hot-grid mouse-3] #'sgf-board-click-right)
    (define-key map [hot-menu mouse-1] #'sgf-menu)
    (define-key map "p" 'sgf-pass)
    map)
  "Keymap set for the overlay svg display. It is only activated when the overlay is displayed.")



;; Emacs automatically creates a hook for the mode (e.g.,
;; sgf-mode-hook), and this hook will be run every time the mode is
;; enabled.
;;;###autoload
(define-derived-mode sgf-mode
    text-mode "SGF"
    "Major mode for editing SGF files. The following commands are available:
\\{sgf-mode-map}"
    :keymap sgf-mode-map)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sgf\\'" . sgf-mode))


(provide 'sgf-mode)
;;; sgf-mode.el ends here

;;; Local Variables:
;;; lisp-indent-function: common-lisp-indent-function
;;; End:
