;;; sgf-mode.el --- SGF Major Mode  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: SGF, go, game


;;; Commentary:
;;

;;; Code:

(require 'sgf-util)
(require 'sgf-svg)
(require 'sgf-io)


(defvar sgf-show-next-hint t
  "Show the hint mark(s) for next move(s).")

(defvar sgf-show-move-number t
  "Show move number on the stones.")

(defvar sgf-show-mark t
  "Show marks on the board.")

(defvar sgf-allow-suicide-move nil
  "Allow suicide or not. Some rule set allow suicide: https://senseis.xmp.net/?Suicide")

(defvar sgf-editable t
  "Allow edit of the SGF buffer from the game.")

(defvar sgf-traverse-path nil
  "Default path to traverse thru when initiate game and display.

Examples:

1. `nil' or `0'. do nothing and stay at the beginning of the game.

2. `t'. move to the end of the game. choose the first branch when come across a fork.

3. `-1'. similar to Example 2 except stay at the move next to the last.

4. `3'. similar to Example 2 except stay at the 3rd move from the beginning.

5. `(34 ?a ?b ?a)'. move forward 34 steps in total by selecting branch a, b and then a respectively

6. `(B (1 . 2))'. move to the first move put B stone at the position x=1 and y=2.

See also `sgf-traverse'. It uses `read-from-string' or `read' to convert
the text value of this variable to elisp object.")


;; TODO check there is only one entity for every type of prop
(defun sgf-show-comment (node)
  "Show the comment of the move/node."
  ;; if 'C' does not exist, it shows an empty str.
  (message (mapconcat 'identity (alist-get 'C node) " ")))


(defun sgf-update-display (&optional ov)
  "Update the svg object and display according to the current game state."
  (interactive)
  (if (null ov) (setq ov (sgf-get-overlay)))
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
    (if (sgf-game-plist-get :show-move-number ov)
        (sgf-svg-add-mvnums svg game-state)
      (sgf-svg-mark-last-move svg curr-node))
    (sgf-svg-update-status-prisoners svg pcounts)
    (sgf-svg-update-status-turn svg turn)
    (if (sgf-game-plist-get :show-next-hint ov)
        (sgf-svg-update-next svg curr-lnode))
    (if (sgf-game-plist-get :show-mark ov)
        (sgf-svg-update-marks svg curr-node board-2d))
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
    (dolist (xy black-xys) (sgf-board-set xy 'B board-2d))
    (dolist (xy white-xys) (sgf-board-set xy 'W board-2d))
    (dolist (xy empty-xys) (sgf-board-set xy 'E board-2d))
    (aset game-state 3 turn)
    (aset game-state 2 ko)
    (setcar pcounts (- (car pcounts) (length black-xys)))
    (setcdr pcounts (- (cdr pcounts) (length white-xys)))))


(defun sgf-branch-selection (n &optional branch)
  "Prompt the user to select a branch by choosing a character.

N is total number of branches. BRANCH is zero-based integer smaller than
N, indicating your branch choice. If it is nil, it will prompt."
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
  (let* ((ov          (sgf-get-overlay))
         (svg         (overlay-get ov 'svg))
         (game-state  (overlay-get ov 'game-state))
         (curr-lnode  (aref game-state 0))
         (next-lnodes (aref curr-lnode 2))
         (n           (length next-lnodes))
         next-lnode next-node)
    (if (= n 0)
        ;; make sure to return nil if there is no next move.
        (progn (message "No more next move.") nil)
      (setq branch (sgf-branch-selection n branch))
      (setq next-lnode (nth branch next-lnodes))
      (setq next-node  (aref next-lnode 1))
      (if interactive-call (sgf-show-comment next-node))
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
  "Move to the previous move in the game tree and update board.

See also `sgf-forward-move'."
  (interactive "p")
  (let* ((ov         (sgf-get-overlay))
         (game-state  (overlay-get ov 'game-state))
         (curr-lnode  (aref game-state 0))
         (prev-lnode  (aref curr-lnode 0)))
    (if (sgf-root-p curr-lnode)
        ;; make sure to return nil if it is the root node.
        (progn (message "No more previous move.") nil)
      (if interactive-call (sgf-show-comment (aref prev-lnode 1)))
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
             siblings)
        (if prev-lnode
            (setq siblings (aref prev-lnode 2)))
        (sgf-backward-move)
        (if (/= (length siblings) 1)
            (setq continue nil))))
    (if interactive-call (sgf-update-display ov))))


(defun sgf-apply-node (node game-state)
  "Apply the node of move to the game state."
  (let* ((move (sgf-process-move node))
         (stone (car move))
         (xy (cdr move))
         (board-2d (aref game-state 1))
         (xy-state-old (sgf-board-get xy board-2d))
         (turn-old (aref game-state 3))
         (turn-new (sgf-enemy-stone turn-old))
         (ko-old (aref game-state 2))
         (pcounts (aref game-state 4))
         ko-new prisoners
         black-xys white-xys empty-xys)
    ;; check it is legal move before make any change to game state
    (unless (sgf-valid-move-p xy stone game-state (sgf-game-plist-get :allow-suicide-move))
      (error "Invalid move of %S at %S" stone xy))
    (when xy
      ;; node is not a pass
      (sgf-board-set xy stone board-2d)
      (setq prisoners (sgf-capture-stones xy board-2d))
      ;; Remove captured stones
      (dolist (xy prisoners) (sgf-board-set xy 'E board-2d))
      ;; Check for KO: this code needs to be put after prisoners are removed.
      (setq ko-new (sgf-get-ko xy stone board-2d prisoners))
      (aset game-state 2 ko-new)
      (when (eq stone 'B)
        (setcdr pcounts (+ (length prisoners) (cdr pcounts)))
        (setq white-xys prisoners))
      (when (eq stone 'W)
        (setcar pcounts (+ (length prisoners) (car pcounts)))
        (setq black-xys prisoners))
      (if (eq xy-state-old 'E)
          (setq empty-xys (list xy))))
    (aset game-state 3 turn-new)
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


(defun sgf-traverse (path &optional interactive-call)
  "Traverse the game tree from current node according to the PATH.

For example, (sgf-traverse '(9 ?b ?a)) will move forward 9 steps and
pick branch b and a in the 1st and 2nd forks (if come across forks),
 respectively."
  (interactive "xTraverse path: \np")
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state)))
    (cond ((null path) nil) ; do nothing
          ((eq path t) (sgf-last-move 0)) ; pick the first branch at all forks
          ((integerp path)
           (cond ((> path 0) (sgf-jump-moves path 0))
                 ;; if it is negative, jump to end and move back PATH steps.
                 ((< path 0) (sgf-last-move 0) (sgf-jump-moves path))
                 ((= path 0) nil)))
          ((listp path) ; eg (9 ?b ?a)
           (let ((steps (car path))
                 (branches (cdr path))
                 (diff 0))
             (dolist (branch branches)
               (sgf-forward-fork)
               (sgf-forward-move (- branch ?a)))
             (setq diff (- steps (sgf-lnode-depth (aref game-state 0))))
             ;; if come across additional forks, pick the 1st branch
             (sgf-jump-moves diff 0))))
    (if interactive-call (sgf-update-display ov))))


(defun sgf-lnode-path (&optional lnode)
  "Return the path in the form of '(steps branch-1 branch-2 ...) to reach
LNODE from the root.

The return value can be passed to `sgf-traverse'."
  (unless lnode
    (let* ((ov (sgf-get-overlay))
           (game-state (overlay-get ov 'game-state)))
      (setq lnode (aref game-state 0))))

  (let ((depth 0) (path '()))
    (while (not (sgf-root-p lnode))
      (let* ((prev-lnode (aref lnode 0))
             (siblings (aref prev-lnode 2)))
        (if (> (length siblings) 1)
            ;; Find the index of the current lnode in sibling nodes and
            ;; append to the end of path
            (push (+ ?a (seq-position siblings lnode)) path))
        (setq lnode prev-lnode
              depth (1+ depth))))
    (cons depth (nreverse path))))


(defun sgf-lnode-depth (lnode)
  "Return the depth of the LNODE from the root node."
  (let ((depth 0))
    (while (not (sgf-root-p lnode))
      (setq lnode (aref lnode 0)
            depth (1+ depth)))
    depth))


(defun sgf-lnode-move-number (lnode)
  "Return the move number for the LNODE.

It computes the depth of LNODE from the root node or previous MN
property, not include setup node.

See also `sgf-lnode-depth'."
  (let ((num 0) mn-prop)
    (while (and (not (sgf-root-p lnode))
                (not (setq mn-prop (car (alist-get 'MN (aref lnode 1)))))
                (setq num (1+ num)))
      (setq lnode (aref lnode 0)))
    (+ (or mn-prop 0) num)))


(defun sgf--toggle-layer(layer)
  "Toggle the display of a give layer."
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (svg  (overlay-get ov 'svg))
         group)
    (cond ((equal layer 'mvnum)
           (sgf-game-plist-toggle :show-move-number ov)
           (setq group (sgf-svg-group-mvnums svg)))
          ((equal layer 'mark)
           (sgf-game-plist-toggle :show-mark ov)
           (setq group (sgf-svg-group-marks svg)))
          ((equal layer 'next)
           (sgf-game-plist-toggle :show-next-hint ov)
           (setq group (sgf-svg-group-next svg))))
    (if (dom-attr group 'visibility)
        ;; show the numbers
        (dom-remove-attribute group 'visibility)
      ;; add visibility attribute to hide svg move number group
      (dom-set-attribute group 'visibility "hidden"))
    (sgf--display-svg ov)))


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
          (svg-print svg))
        (message "SVG exported to %s" filename)))))


(defun sgf-edit-move-number (&optional lnode)
  "Edit the move number of the given node or current node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (lnode (or lnode (aref game-state 0)))
         (node  (aref lnode 1))
         (old-mvnum  (car (alist-get 'MN node)))
         new-mvnum)
    (setq new-mvnum (read-number "Move number: " old-mvnum))
    (if (integerp new-mvnum)
        (unless (equal old-mvnum new-mvnum)
          (aset lnode 1
                ;; delete the old MN *whether* it exists or not
                (nconc (assq-delete-all 'MN node)
                       (list (list 'MN new-mvnum))))
          (if (sgf-game-plist-get :show-number)
              (sgf-update-display ov t nil t)
            (message "Move number was not displayed. Enable its display.")
            (sgf-toggle-numbers))

          (sgf-serialize-game-to-buffer ov))
      (message "Invalid move number %S. Please enter an integer." new-mvnum))))


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
      ;; delete the old comment property
      (setq node (assq-delete-all 'C node))
      (aset lnode 1
            (if (string-empty-p new-comment)
                node
              (nconc node (list (list 'C new-comment))))))
    (sgf-serialize-game-to-buffer ov)))


;; igo-editor-move-mode-make-move-root
;; todo
(defun sgf-root-node ()
  "Make the current node the root node.
1. move all the nodes in between to the root node of setup.
2. change B, W to AB, AW"
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0)))
    (sgf-serialize-game-to-buffer curr-lnode (overlay-buffer ov))))


(defun sgf-prune ()
  "Delete all its children of the current node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0)))
    (aset curr-lnode 2 nil)
    (sgf-serialize-game-to-buffer curr-lnode (overlay-buffer ov))))


(defun sgf-prune-inclusive ()
  "Delete the current node and all its children."
  (interactive)
  ;; update display
  (sgf-backward-move t)
  (sgf-prune))

;; todo
(defun sgf-edit-game-info ()
  "Edit the game information."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0)))
    (sgf-serialize-game-to-buffer curr-lnode (overlay-buffer ov))))


(defun sgf-mouse-event-to-xy (event)
  "Convert a mouse click to a board position (X . Y)."
  (if (mouse-event-p event)
      (let* ((xy (posn-object-x-y (event-start event)))
             (x (/ (- (float (car xy)) sgf-svg-margin) sgf-svg-interval))
             (y (/ (- (float (cdr xy)) sgf-svg-margin sgf-svg-bar) sgf-svg-interval)))
        (cons (round x) (round y)))))

(defun sgf-board-click-left (event)
  "Add stone by mouse click on board.
Cases:
1. click on the next move position: the same as `sgf-forward-move';
2. click on other position: put a stone at the clicked position
2.1 if it is at the end of node, create a new node;
2.2 otherwise, create a new branch of game tree.
3. other illegal positions"
  (interactive "@e")
  ;; mouse-1 event for the left click
  (let* ((xy (sgf-mouse-event-to-xy event))
         (ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (ko   (aref game-state 2))
         (turn (aref game-state 3))
         (next-lnodes (aref curr-lnode 2))
         (next-xys (mapcar (lambda (node) (cdr (sgf-process-move (aref node 1)))) next-lnodes))
         (found (car (seq-positions next-xys xy))))
    (cond
     ;; Case 1: Clicked on one of the next move position
     (found (sgf-forward-move found))
     ;; Case 2: Clicked on an empty position not equal to ko
     ((sgf-valid-move-p xy turn game-state (sgf-game-plist-get :allow-suicide-move ov))
      (let ((n (length next-lnodes))
            (new-lnode (sgf-linked-node curr-lnode `((,turn ,xy)))))
        ;; add the new node as the last branch
        (aset curr-lnode 2 (nconc next-lnodes (list new-lnode)))
        (sgf-forward-move n) ; if n=0, case 2.1; otherwise, case 2.2
        (sgf-update-display ov)
        (sgf-serialize-game-to-buffer curr-lnode (overlay-buffer ov))))
     ;; Case 3.
     (t (message "Illegal move!")))))


(defun sgf-board-click-right (event)
  "Right click on board pop up a menu."
  (interactive "@e")
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (xy (sgf-mouse-event-to-xy event))
         (clicked-lnode (sgf-find-back-lnode xy game-state))
         (menu `("ACTION ON THIS MOVE"
                 ["Edit Comment"
                  ,(lambda () (interactive) (sgf-edit-comment clicked-lnode))]
                 ["Edit Move Number"
                  ,(lambda () (interactive) (sgf-edit-move-number clicked-lnode))
                  :enable ,(not (sgf-root-p clicked-lnode))]
                 ["Back to This Move"
                  ,(lambda () (interactive) (sgf-goto-back-lnode clicked-lnode))
                  :help "Move game state to this move"
                  :enable ,(not (equal curr-lnode clicked-lnode))]
                 ["Prune to This Move"
                  ,(lambda () (interactive) (sgf-goto-back-lnode clicked-lnode) (sgf-prune))
                  :enable ,(not (sgf-root-p clicked-lnode))] ; not root node
                 ["Put as Setup"
                  ,(lambda () (interactive) (sgf-root-node))
                  :enable ,(not (sgf-root-p clicked-lnode))])))
    (popup-menu menu)))


(defun sgf-goto-back-lnode (lnode)
  "Move game state backwards to the given LNODE and update svg board."
  (let* ((ov  (sgf-get-overlay))
         (game-state  (overlay-get ov 'game-state))
         found)
    (while (not found)
      (let* ((curr-lnode  (aref game-state 0))
             (prev-lnode  (aref curr-lnode 0)))
        (if (equal curr-lnode lnode)
            (setq found t)
          (sgf-revert-undo (sgf-pop-undo game-state) game-state)
          (aset game-state 0 prev-lnode))))
    (sgf-update-display ov)))

(defun sgf-find-back-lnode (xy game-state)
  "Search backward from the current game state to find the node that put
the corresponding stone at XY on board. There could be multiple nodes at
the same position during the whole game; this function finds the closest
one to the current game state.

Returns linked node found or nil if not. The game-state remains unchanged."
  (let* ((board-2d  (aref game-state 1))  ;; Extract the current board
         (stone (sgf-board-get xy board-2d))  ;; Get the stone at the XY position
         (curr-lnode (aref game-state 0))
         found-lnode)
    (while (not found-lnode)  ;; Loop until node is found or root is reached
      (let* ((curr-node (aref curr-lnode 1))  ;; Extract the SGF node data
             (play (sgf-process-move curr-node))  ;; Process the current move
             (stone-i (car play))  ;; Stone placed in this node
             (xy-i (cdr play)))  ;; Coordinates of the move
        (if (and (eq stone-i stone) (equal xy-i xy))  ;; Check if it's the node we're looking for
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
         (new-lnode (sgf-linked-node curr-lnode '((W)))))
    (aset curr-lnode 2 (append next-lnodes (list new-lnode)))
    (sgf-serialize-game-to-buffer curr-lnode (overlay-buffer ov))
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
     ;;(lambda () (not (equal (key-description (this-command-keys)) "C-c C-g")))
     (lambda () (not (equal last-input-event ?\C-g)))
     ;; show exit message
     (lambda () (message "Exited edit mode."))
     (concat message-text "Type C-g to exit.")
     ;; automatic exit after 30 seconds
     30)))


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
                       (progn (sgf-board-set xy 'E board-2d)
                              (delete xy xys))  ;; Remove stone from the list
                     (progn (sgf-board-set xy stone board-2d)
                            (nconc xys (list xy))))))
          (if xys
              (if prop
                  (setcdr prop xys)  ; Update existing entry
                (nconc curr-node (list (list prop-key xys)))) ; Add new property entry
            ;; If the xy list is empty, remove the property entirely
            (setq curr-node (assq-delete-all prop-key curr-node)))
          (sgf-serialize-game-to-buffer curr-lnode (overlay-buffer ov))
          (sgf-update-display ov)
          (message "Edited %s stone at %s" stone xy))
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
    (sgf-serialize-game-to-buffer curr-lnode (overlay-buffer ov))
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
    ;; Serialize the game state to the buffer
    (sgf-serialize-game-to-buffer curr-lnode (overlay-buffer ov))
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
          (sgf-serialize-game-to-buffer curr-lnode (overlay-buffer ov))
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

(defun sgf-toggle-svg-display (&optional beg end)
  "Toggle graphical display. Overlay keeps unchanged.

If BEG and END are nil, parse the whole buffer as SGF content."
  (interactive)
  (let* ((ov (sgf-get-overlay)))
    (if ov
        (if (overlay-get ov 'display)
            (sgf--hide-svg ov)
          (sgf--display-svg ov))
      (sgf-create-overlay (or beg (point-min))
                          (or end (point-max))))))


(defun sgf-create-overlay (&optional beg end)
  "Create a fresh overlay and setup overlay properties.

It removes old overlays if there is any."

  (interactive)
  (setq beg (or beg (point-min))
        end (or end (point-max)))
  ;; remove old ones; otherwise, it accumulates repetitive overlays
  ;; over calls
  (remove-overlays beg end)
  ;; set front- and rear-advance parameters to allow
  ;; the overlay cover the whole buffer even if it is
  ;; updated from game playing.
  (let* ((ov (make-overlay beg end nil nil t))
         (game-state (sgf-parse-buffer-to-game beg end))
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
                       :traverse-path sgf-traverse-path
                       :editable sgf-editable))
    (overlay-put ov 'svg svg)
    (overlay-put ov 'hot-areas hot-areas)
    (overlay-put ov 'keymap sgf-mode-graphical-map)
    ;; traverse to the specified game state
    (sgf-traverse sgf-traverse-path)
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
;;     (sgf-create-overlay new-ov)
;;     new-ov))


(defun sgf-game-plist-get (key &optional ov)
  "Return game property of KEY"
  (let* ((ov (or (sgf-get-overlay)))
         (game-plist (overlay-get ov 'game-plist)))
    (plist-get game-plist key)))


(defun sgf-game-plist-set (key value &optional ov)
  (let* ((ov (or (sgf-get-overlay)))
         (game-plist (overlay-get ov 'game-plist)))
    (plist-put game-plist key value)))


(defun sgf-game-plist-toggle (key &optional ov)
  "Toggle the game property of KEY."
  (let* ((ov (or (sgf-get-overlay)))
         (game-plist (overlay-get ov 'game-plist)))
    (sgf-game-plist-set ov key (not (plist-get game-plist key)))))


(defun sgf-menu ()
  "Show the main menu for the SGF mode."
  (interactive "@")
  (let* ((ov (sgf-get-overlay))
         (menu-keymap
          ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Menu-Example.html
          `(keymap "Main Menu"
                   (sgf-toggle-svg-display menu-item "Diable SVG Display" sgf-toggle-svg-display)
                   (sgf-toggle-allow-suicide-move
                    menu-item "Allow Suicide Move"
                    sgf-toggle-allow-suicide-move
                    :button (:toggle . (sgf-game-plist-get :allow-suicide-move ,ov)))
                   (sgf-toggle-move-number ; key symbol
                    menu-item "Show Move Number"
                    sgf-toggle-move-number
                    :button (:toggle . (sgf-game-plist-get :show-move-number ,ov)))
                   (sgf-toggle-next-hint
                    menu-item "Show Next Hint"
                    sgf-toggle-next-hint
                    :button (:toggle . (sgf-game-plist-get :show-next-hint ,ov)))
                   (sgf-toggle-marks
                    menu-item "Show Marks"
                    sgf-toggle-marks
                    :button (:toggle . (sgf-game-plist-get :show-mark ,ov)))
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
    (define-key map (kbd "C-c C-c") 'sgf-create-overlay)
    (define-key map (kbd "C-c C-t") 'sgf-toggle-svg-display)
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
    (define-key map "t" 'sgf-traverse)
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
    (define-key map [hot-del mouse-1] #'sgf-prune-inclusive)
    (define-key map [hot-menu mouse-1] #'sgf-menu)
    (define-key map "p" 'sgf-pass)
    map)
  "Keymap set for the overlay svg display. It is set as overlay property
and only activated when the overlay is displayed.")



;; Emacs automatically creates a hook for the mode (e.g.,
;; sgf-mode-hook), and this hook will be run every time the mode is
;; enabled.
;;;###autoload
(define-derived-mode sgf-mode
  text-mode "SGF"
  "Major mode for editing SGF files. The following commands are available:
\\{sgf-mode-map}"
  :keymap sgf-mode-map)
;; TODO  font-lock, use imenu, use xref, use completion-at-point

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sgf\\'" . sgf-mode))


(provide 'sgf-mode)
;;; sgf-mode.el ends here
