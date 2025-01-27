;;; sgf-mode.el --- SGF Major Mode  -*- lexical-binding: t; -*-


;; Author: Zech Xu
;; Version: 1.0
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://github.com/RNAer/sgf-mode
;; Keywords: SGF, go, game

;;; Commentary:
;;

;;; Code:

(require 'sgf-util)
(require 'sgf-svg)
(require 'sgf-io)
(require 'sgf-graph)
(require 'katago)

(defun sgf-push-undo (game-state change)
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

(defun sgf-revert-undo (game-state &optional change)
  "Revert the CHANGE for the GAME-STATE"
  (unless change
    (setq change (sgf-pop-undo game-state)))
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
    (aset game-state 6 (1- (aref game-state 6)))
    (setcar pcounts (- (car pcounts) (length black-xys)))
    (setcdr pcounts (- (cdr pcounts) (length white-xys)))))


(defun sgf-branch-selection (n &optional branch)
  "Prompt the user to select a branch by choosing a character.

N is total number of branches. BRANCH is zero-based integer smaller than
N, indicating your branch choice. If it is nil, it will prompt."
  (let* ((last (+ ?a (1- n)))
         (prompt (format "Select a branch (a-%c): " last)))
    (if (null branch)
        (setq branch
              (if (= n 1)
                  0
                (- (read-char-choice prompt (number-sequence ?a last))
                   ?a))))
    (if (and (>= branch 0) (< branch n))
        branch
      (error "Invalid branch selection: %c" (+ branch ?a)))))


(defun sgf-forward-move (&optional branch ov interactive-call)
  "Move to the next move in the game tree and update board.

See also `sgf-branch-selection'."
  ;; Since branch is only used when called non-interactively, use
  ;; code`i' to skip assignment for variable branch when called
  ;; interactively (ie, branch value will be nil).

  ;; Use code `p' to check if the function is call interactively. When
  ;; called non-interactively, interactive-call will be nil; otherwise
  ;; it is 1
  (interactive "i\ni\np")
  (let* ((ov   (or ov (sgf-get-overlay)))
         (game-state (overlay-get ov 'game-state))
         (lnode    (aref game-state 0))
         (children (aref lnode 2))
         (n        (length children))
         child)
    (if (= n 0)
        ;; make sure to return nil if there is no next move.
        (when interactive-call (message "No more next move.") nil)
      (setq branch (sgf-branch-selection n branch))
      (setq child (nth branch children))
      (sgf-apply-lnode child game-state (sgf-game-plist-get :suicide-move ov))
      (when (get-buffer-window sgf-graph-buffer-name)
        (with-current-buffer sgf-graph-buffer-name
          (sgf-graph-forward-node branch)))
      ;; return t if it is a noninteractive call, to indicate a
      ;; successful forward move.
      (when interactive-call
        (sgf-show-comment child)
        (sgf-update-display ov))
      t)))


(defun sgf-forward-fork (&optional ov interactive-call)
  "Move to the step just before the next fork."
  (interactive "i\np")
  (let* ((ov (or ov (sgf-get-overlay)))
         (game-state (overlay-get ov 'game-state)))
    ;; Continue advancing as long as there is exactly one child.
    (while (= (length (aref (aref game-state 0) 2)) 1)
      (sgf-forward-move 0 ov))
    ;; Update display if called interactively.
    (when interactive-call
      (sgf-update-display ov))))


(defun sgf-backward-move (&optional ov interactive-call)
  "Move to the previous move in the game tree and update board.

See also `sgf-forward-move'."
  (interactive "i\np")
  (let* ((ov  (or ov (sgf-get-overlay)))
         (game-state  (overlay-get ov 'game-state))
         (lnode  (aref game-state 0))
         (parent (aref lnode 0)))
    (if (sgf-root-p lnode)
        ;; make sure to return nil if it is the root node.
        (when interactive-call (message "No more previous move.") nil)
      (sgf-revert-undo game-state)
      (aset game-state 0 parent)
      (when (get-buffer-window sgf-graph-buffer-name)
        (with-current-buffer sgf-graph-buffer-name
          (sgf-graph-backward-node)))
      (when interactive-call
        (sgf-show-comment parent)
        (sgf-update-display ov))
      t)))


(defun sgf-backward-fork (&optional interactive-call)
  "Move to the step before the previous fork."
  (interactive "p")
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state)))
    ;; Continue moving backward until there is more than one sibling.
    (while (let* ((lnode (aref game-state 0))
                  (parent (aref lnode 0))
                  (siblings (and parent (aref parent 2))))
             (sgf-backward-move ov)
             (and siblings (= (length siblings) 1))))
    ;; Update display if called interactively.
    (when interactive-call
      (sgf-update-display ov))))


(defun sgf-apply-lnode (lnode game-state allow-suicide)
  "Apply the node of move to the game state."
  (let* ((node (aref lnode 1))
         (move (sgf-process-move node))
         (stone (car move))
         (xy (cdr move))
         (board-2d (aref game-state 1))
         (turn-old (aref game-state 3))
         (turn-new (sgf-enemy-stone turn-old))
         (ko-old (aref game-state 2))
         (pcounts (aref game-state 4))
         ;; for the non-root node that has AW or AB, get setup stones
         ;; and add them to the board
         (setup-stones (sgf-add-setup-stones node board-2d))
         (empty-xys (nconc (car setup-stones) (cdr setup-stones)))
         black-xys white-xys ko-new prisoners)
    (aset game-state 0 lnode)
    (aset game-state 6 (1+ (aref game-state 6)))
    (when xy   ; node is not a pass
      ;; Validate the move before make any change to game state.
      (unless (sgf-valid-move-p xy stone board-2d ko-old)
        (error "Invalid move of %S at %S!" stone xy))
      (if (eq (sgf-board-get xy board-2d) 'E) (push xy empty-xys))
      (sgf-board-set xy stone board-2d)
      ;; Handle captured stones
      (setq prisoners (sgf-capture-stones xy board-2d))
      (dolist (xy prisoners) (sgf-board-set xy 'E board-2d))
      ;; Check for suicide after removing captured stones
      (when (and (not allow-suicide)
                 (sgf-suicide-stones xy board-2d))
        ;; undo the changes on the board
        (dolist (xy prisoners) (sgf-board-set xy (sgf-enemy-stone stone) board-2d))
        (sgf-board-set xy 'E board-2d)
        (error "Suicide move at %S is not allowed!" xy))
      ;; Determine KO status after captures: this code needs to be put after prisoners are removed.
      (setq ko-new (sgf-get-ko xy stone board-2d prisoners))
      (aset game-state 2 ko-new)
      ;; Update prisoner counts and store captured positions.
      (cond ((eq stone 'B)
             (setcdr pcounts (+ (length prisoners) (cdr pcounts)))
             (setq white-xys prisoners))
            ((eq stone 'W)
             (setcar pcounts (+ (length prisoners) (car pcounts)))
             (setq black-xys prisoners))))
    ;; Update the game state with the next turn.
    (aset game-state 3 turn-new)
    ;; Save undo state.
    (sgf-push-undo game-state (vector black-xys white-xys empty-xys ko-old turn-old))))


(defun sgf-first-move (&optional ov interactive-call)
  "Move to the first node in the game tree."
  (interactive "i\np")
  (while (sgf-backward-move ov))
  (when interactive-call
    (sgf-update-display ov)))


(defun sgf-last-move (&optional ov interactival-call)
  "Move to the last node in the game tree.

Always pick the 1st branch upon fork. See also `sgf-forward-move'."
  (interactive "i\np")
  (while (sgf-forward-move 0 ov))
  (when interactival-call
    (sgf-update-display ov)))


(defun sgf-jump-moves (n &optional ov interactive-call)
  "Move forward or backward N nodes.

It pauses at fork and wait for user input to select a branch.
See also `sgf-forward-move'."
  (interactive "nMove _ plays forward (pos number) or backward (neg number): \ni\np")
  (if (> n 0)
      (dotimes (_ n) (sgf-forward-move 0 ov))
    (dotimes (_ (- n)) (sgf-backward-move ov)))
  (when interactive-call
    (sgf-update-display ov)))


(defun sgf-back-to-game ()
  "Return to the main game, assuming you are off the main variation.

The main variation is the 1st branch for every forks."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (lnode (sgf-get-lnode ov))
         (path (sgf-lnode-path lnode))
         (depth (pop path))
         branch)
    (while (branch (pop path))
      (if (eq branch ?a)
          (sgf-forward-fork ov)))
    (sgf-update-display ov)))


(defun sgf-show-comment (&optional lnode)
  "Show the comment of the move/node."
  ;; if 'C' does not exist, it shows an empty str.
  (interactive)
  (let* ((lnode (or lnode (sgf-get-lnode)))
         (node (aref lnode 1)))
    (message (replace-regexp-in-string
              "%" "%%" ; Escape '%' in the comment str
              (mapconcat 'identity (alist-get 'C node) " ")))))


(defun sgf-show-path ()
  "Show the path in the form of `(steps branch-1 branch-2 ...)' to reach
the current game state from the root."
  (interactive)
  (let* ((lnode (sgf-get-lnode))
         (path (sgf-lnode-path lnode)))
    (message "Path: %s" (sgf-path-to-str path))))


(defun sgf-traverse (path &optional ov interactive-call)
  "Traverse the game tree from current node according to the PATH.

For example, (sgf-traverse \\='(9 ?b ?a)) will move forward 9 steps and
pick branch b and a in the 1st and 2nd forks (if come across forks),
 respectively. See also `sgf-traverse-path'."
  (interactive "xTraverse path: \ni\np")
  (let* ((ov (or ov (sgf-get-overlay))))
    (cond ((null path) nil) ; do nothing
          ;; path is `t': go to the last move and pick the first branch at all forks.
          ((eq path t) (sgf-last-move ov))
          ;; path is an integer: jump forward or backward by PATH steps.
          ((integerp path)
           (cond ((> path 0) (sgf-jump-moves path ov))
                 ;; if it is negative, jump to end and move back PATH steps.
                 ((< path 0) (sgf-last-move ov) (sgf-jump-moves path ov))))
          ;; path is a list. eg (9 ?b ?a)
          ((listp path)
           (dolist (branch (cdr path))
             (sgf-forward-fork ov)
             (sgf-forward-move (- branch ?a) ov))
           (let ((depth 0) (steps (car path)) (lnode (sgf-get-lnode ov)))
             (while (setq lnode (aref lnode 0)) (setq depth (1+ depth)))
             ;; if come across additional forks, pick the 1st branch
             (sgf-jump-moves (- steps depth) ov))))
    (when interactive-call
      (sgf-update-display ov))))


(defun sgf-merge-nodes (node-1 node-2)
  "Merge NODE-2 into NODE-1 in place.

The NODE-1 will be updated with the merged result. The NODE-2 may
remain unmodified."
  (dolist (item1 node-1)
    (let* ((key (car item1))
           (value1 (cdr item1))
           (item2 (assoc key node-2))
           (value2 (cdr item2)))
      (cond
       ((or (eq key 'B) (eq key 'W))) ; do nothing
       ;; for comments, concatenate them.
       ((and (eq key 'C) item2)
        (setcdr item1 (list (concat (car value1) " " (car value2)))))
       ;; If key exists in both lists and assume values are both
       ;; lists, append them and uniquefy them.
       (item2
        (setcdr item1 (seq-uniq (append value1 value2)))))))
  ;; Add items from NODE-2 that were not in NODE-1.
  (dolist (item2 node-2)
    (let* ((key (car item2)))
      (unless (or (eq key 'B) (eq key 'W) (assoc key node-1))
        (push item2 node-1))))
  node-1)


(defun sgf--merge-branches (lnode)
  "Merge the branches with the same moves."
  (let ((moves (make-hash-table :test 'equal))
        (children (aref lnode 2))
        (new-children '()))
    (dolist (child children)
      (let* ((node (aref child 1))
             (move (sgf-process-move node))
             (exist-lnode (gethash move moves)))
        (if exist-lnode
            (progn (aset exist-lnode 2 (nconc (aref exist-lnode 2)
                                              (aref child 2)))
                   (sgf-merge-nodes (aref exist-lnode 1) node))
          (puthash move child moves))))
    (maphash (lambda (k v)
               (aset v 0 lnode)
               (push v new-children)) moves)
    (aset lnode 2 new-children)))


(defun sgf-merge-branches ()
  "Merge the branches with the same next moves of current node.

This function calls `sgf--merge-branches' to do the heavy-lifting work.
It is useful to merge multiple game variations with same head moves into
one game."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (lnode (sgf-get-lnode ov)))
    (sgf--merge-branches lnode)
    (sgf-graph-tree ov)
    (sgf-update-display ov t t nil)
    (sgf-serialize-game-to-buffer ov)))


(defun sgf-swap-branches (b front)
  "Swap the child branch B to front or back.

Swap the chosen branch to back by default; if prefix argument is non
nil, swap to front. If there is only one branch, it will not swap."
  (interactive "i\nP")

  (let* ((ov (sgf-get-overlay))
         (lnode (sgf-get-lnode ov))
         (children (aref lnode 2))
         (n-1 (1- (length children))))
    (if (> n-1 0)
        (let* ((last (+ ?a n-1))
               (b (or b
                      (read-char-choice
                       (format "Swap the branch _ to %s (choose a-%c): "
                               (if front "front" "back") last)
                       (number-sequence ?a last))))
               (i (- b ?a))
               (j (if front
                      (if (> i 0)
                          (1- i)
                        (error "The branch %c is already in the front." b))
                    (if (< i n-1)
                        (1+ i)
                      (error "The branch %c is already in the back." b))))
               (child-i (nth i children))
               (child-j (nth j children)))
          ;; swap in place
          (setf (nth i children) child-j)
          (setf (nth j children) child-i)
          (sgf-graph-tree ov)
          (sgf-update-display ov t t nil)
          (sgf-serialize-game-to-buffer ov))
      (message "There is only one branch - no swap."))))


(defun sgf-remove-variations ()
  "Remove all the variations before the current game state in the game tree."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (lnode (sgf-get-lnode ov))
         (parent (aref lnode 0)))
    (while parent
      (if (> (length (aref parent 2)) 1)
          (aset parent 2 (list lnode)))
      (setq lnode parent)
      (setq parent (aref parent 0)))
    (sgf-graph-tree ov)
    (sgf-serialize-game-to-buffer ov)))


(defun sgf--svg-group-from-game-prop (svg game-prop)
  (let* ((s (symbol-name game-prop))
         (group-id (substring s 6)))
    (sgf-svg-group svg group-id)))

(defun sgf--toggle-layer (game-prop)
  "Toggle the display of a give layer."
  (let* ((ov (sgf-get-overlay))
         (svg  (overlay-get ov 'svg))
         (group (sgf--svg-group-from-game-prop svg game-prop)))
    (sgf-game-plist-toggle game-prop ov)
    (sgf-svg-toggle-visibility group)
    (sgf-update-display ov)))

;; todo
(defun sgf-toggle-katago-info ()
  "Toggle the display of katago winrate or score."
  (interactive)
  (sgf--toggle-layer :show-kifu))

(defun sgf-toggle-katago ()
  "Toggle the display of Katago's evaluation."
  (interactive)
  (sgf--toggle-layer :show-katago))

(defun sgf-toggle-numbers ()
  "Toggle the display of move numbers."
  (interactive)
  (sgf--toggle-layer :show-numbers))

(defun sgf-toggle-hints ()
  "Toggle the display of next move hint."
  (interactive)
  (sgf--toggle-layer :show-hints))

(defun sgf-toggle-marks ()
  "Toggle the display of marks."
  (interactive)
  (sgf--toggle-layer :show-marks))

(defun sgf-toggle-ko ()
  "Toggle the display of KO mark."
  (interactive)
  (sgf--toggle-layer :show-ko))

(defun sgf-toggle-new-move ()
  "Toggle whether allow new moves.

See also `sgf-new-move'."
  (interactive)
  (let* ((ov (sgf-get-overlay)))
    (sgf-game-plist-toggle :new-move ov)
    (message "Toggled allowing new move.")
    (if (and (not (sgf-game-plist-get :new-move ov))
             (sgf-game-plist-get :show-hints ov))
        ;; if not allowing new move, it may be in self exam, disable
        ;; showing hint of next move.
        (sgf-toggle-hints))))

(defun sgf-toggle-suicide-move ()
  "Toggle whether allow suicide moves.

See also `sgf-suicide-move'."
  (interactive)
  (let* ((ov (sgf-get-overlay)))
    (sgf-game-plist-toggle :suicide-move ov)))


(defun sgf-export-image (&optional filename)
  "Export the board to an SVG file or display it in a buffer.

It clones svg to a new object and removes the menu bar and move the
status bar to the top instead."
  (interactive "FExport svg to file: ")
  (let* ((ov (sgf-get-overlay))
         (svg (overlay-get ov 'svg))
         (svg-new (copy-tree svg))
         (status-bar (car (dom-by-id svg-new "status-bar"))))
    (svg-remove svg-new "menu-bar")
    (dom-set-attribute status-bar 'transform "translate(0, 0)")
    (if (or (not filename) (string-empty-p filename))
        ;; If no filename is given, display the SVG in a buffer
        (with-current-buffer (get-buffer-create "*SVG Image*")
          (erase-buffer)
          (svg-print svg-new)
          (display-buffer (current-buffer)))
      ;; Otherwise, write the SVG to the specified file
      (when (or (not (file-exists-p filename))
                (y-or-n-p (format "The file '%s' already exists. Overwrite? " filename)))
        (with-temp-file filename
          (svg-print svg-new))
        (message "SVG exported to %s" filename)))))


(defun sgf-edit-move-number (&optional lnode)
  "Edit the move number of the given node or current node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (lnode (or lnode (sgf-get-lnode ov)))
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
          (if (sgf-game-plist-get :show-numbers)
              (sgf-update-display ov t nil t)
            (message "Move number was not displayed. Enable its display.")
            (sgf-toggle-numbers))
          (sgf-serialize-game-to-buffer ov))
      (message "Invalid move number %S. Please enter an integer." new-mvnum))))


(defun sgf-edit-comment (&optional lnode)
  "Edit the comment of the given node or current node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (lnode (or lnode (sgf-get-lnode ov)))
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
              (nconc node (list (list 'C new-comment)))))
      (sgf-graph-tree ov)
      (sgf-serialize-game-to-buffer ov))))


(defun sgf-edit-annotation (&optional lnode)
  "Add or delete move annotation."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (lnode (or lnode (sgf-get-lnode ov)))
         (node (aref lnode 1))
         ;; C[foo][spam] -> "foo spam"
         (old-annt (seq-find (lambda (i) (alist-get i node))
                             '(BM DO IT TE)))
         (annt-choice (read-multiple-choice "Edit move annotation: "
                                            '((?b "BM" "bad move")
                                              (?d "DO" "doubtful move")
                                              (?i "IT" "interesting move")
                                              (?t "TE" "tesuji move")
                                              (?n "nil" "delete annotation"))))
         (new-annt (intern (cadr annt-choice))))
    ;; only update if the annotation is changed
    (unless (eq old-annt new-annt)
      ;; delete the old comment property
      (setq node (assq-delete-all old-annt node))
      (aset lnode 1
            (if (null new-annt)
                node
              (nconc node (list (list new-annt "1")))))
      (sgf-graph-tree ov)
      (sgf-update-display ov nil t t))
    (sgf-serialize-game-to-buffer ov)))


(defun sgf-make-root (&optional lnode)
  "Make all the nodes before and including the current node to the root node.

It add the stones as setup stones. Note that it discards all the
marks, labels, and comments of the moves except the last one."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (undos (aref game-state 5))
         (curr-lnode (aref game-state 0))
         (lnode (or lnode curr-lnode))
         (node (aref lnode 1))
         (same (eq curr-lnode lnode))
         (children (aref lnode 2))
         (depth 0)
         (aw '()) (ab '()))
    (while (not (sgf-root-p lnode))
      (let* ((move (sgf-process-move (aref lnode 1)))
             (stone (car move))
             (xy (cdr move)))
        (if (eq stone 'B) (push xy ab) (push xy aw))
        (setq depth (1+ depth))
        (setq lnode (aref lnode 0))))
    (if (= depth 0) ; skip if it was at root node.
        (message "It is at root node. skip...")
      ;; now lnode is the root lnode
      ;; link root to the next node(s) and vice vesa
      (aset lnode 2 children)
      (dolist (child children) (aset child 0 lnode))
      (let ((root (aref lnode 1)))
        ;; append new AB and AW to the root node
        (if ab (setf (alist-get 'AB root) (nconc (alist-get 'AB root '()) ab)))
        (if aw (setf (alist-get 'AW root) (nconc (alist-get 'AW root '()) aw)))
        (aset lnode 1 (sgf-merge-nodes root node)))
      (aset game-state 0 (if same lnode curr-lnode))
      (aset game-state 2 nil)             ; clear KO
      (aset game-state 5 (nthcdr depth undos))  ; clear undos before lnode
      (sgf-graph-tree ov)
      (sgf-update-display ov)
      (sgf-serialize-game-to-buffer ov))))


(defun sgf-prune (&optional interactive-call)
  "Delete all its children of the current node."
  (interactive "p")
  (let* ((ov (sgf-get-overlay))
         (lnode (sgf-get-lnode ov)))
    (aset lnode 2 nil)
    ;; update hint display if the hint is set to shown
    (if (and interactive-call (sgf-game-plist-get :show-hints))
        (sgf-update-display ov t t))
    (sgf-graph-tree ov)
    (sgf-serialize-game-to-buffer ov)))


(defun sgf-prune-inclusive ()
  "Delete the current node and all its children."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (lnode (sgf-get-lnode ov))
         (parent (aref lnode 0))
         (siblings (aref parent 2)))
    (sgf-backward-move ov)
    (aset parent 2 (delq lnode siblings))
    (sgf-graph-tree ov)
    (sgf-update-display ov)
    (sgf-serialize-game-to-buffer ov)))


;; Game Info Properties
(defconst sgf-game-info-props
  '((CP "Copyright")
    (US "Enterer Name")
    (AN "Annotator Name")
    (SO "Source")
    (EV "Event Name")
    (GN "Game Name")
    (RO "Round Number" :type integer)
    (DT "Date")
    (PC "Place")
    (BT "Black Team")
    (PB "Black Player")
    (BR "Black Rank")
    (WT "White Team")
    (PW "White Player")
    (WR "White Rank")
    (RU "Rule")
    (OT "Overtime Method")
    (TM "Time Limit" :type number)
    (HA "Handicap Stones" :type integer)
    (KM "Komi" :type number)
    (RE "Result")
    (ON "Opening Moves")
    (GC "Comment" :type text)))

(defun sgf-game-info-prop-type (prop)
  (or (plist-get (cddr prop) :type) 'text))

(defun sgf-edit-game-info (&optional ov)
  "Edit the game information."
  (interactive)
  (require 'widget)
  (let* ((ov (or ov (sgf-get-overlay)))
         (game-state (overlay-get ov 'game-state))
         (lnode (aref game-state 0))
         (buffer-name "*Go Game Information*")
         (max-width (apply #'max
                           (mapcar
                            (lambda (prop) (string-width (nth 1 prop)))
                            sgf-game-info-props)))
         (km (make-sparse-keymap))
         widgets)
    ;; move to root node
    (while (aref lnode 0) (setq lnode (aref lnode 0)))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (widget-insert "Game Information\n\n")
    (widget-insert " C-c C-c: Save ")
    (widget-insert " C-c C-r: Reset ")
    (widget-insert " C-x k: Cancel\n\n")
    (set-keymap-parent km widget-field-keymap)
    (define-key km (kbd "C-c C-c")
                (lambda () (interactive) (sgf-edit-game-info--save ov lnode widgets)))
    (define-key km (kbd "C-c C-r")
                (lambda () (interactive) (sgf-edit-game-info ov)))

    (dolist (prop sgf-game-info-props)
      (let* ((prop-id (car prop))
             (prop-description (nth 1 prop))
             (prop-type (sgf-game-info-prop-type prop))
             (prop-value (car (alist-get prop-id (aref lnode 1))))
             (field (format "%%%ds: %%%%v" max-width))
             (widget (widget-create
                      (or prop-type 'editable-field)
                      :size 15
                      :format (format field prop-description)
                      :keymap km
                      prop-value)))
        (push (cons prop-id widget) widgets)
        (widget-insert "\n")))
    (widget-insert "\n ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (sgf-edit-game-info--save ov lnode widgets))
                   "Save")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (sgf-edit-game-info ov))
                   "Reset")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (kill-buffer buffer-name))
                   "Cancel")
    (widget-insert "\n")

    (use-local-map widget-keymap)
    (widget-setup)
    ;; move to first field-
    (widget-forward 1)))

(defun sgf-edit-game-info--save (ov lnode widgets)
  "Validate the input value and save it."
  (interactive)
  (let ((node (aref lnode 1)))
    (dolist (prop sgf-game-info-props)
      (let* ((prop-id (car prop))
             (prop-type (sgf-game-info-prop-type prop))
             (prop-description (nth 1 prop))
             (widget (cdr (assoc prop-id widgets)))
             (value (widget-value widget)))
        ;; (message "%S" prop-id)
        ;; convert value to string or nil
        (if (eq prop-type 'text)
            (setq value (sgf-io--escape-text value)))
        (if (and (stringp value) (string= value ""))
            (setq value nil))
        ;; set or delete property if changed
        (when (not (equal value (car (alist-get prop-id node))))
          ;; first, validate if the input is correct value type
          (if (widget-apply widget :validate)
              (error "%s: %s" prop-description (widget-get widget :error)))

          (setq node (assoc-delete-all prop-id node))
          (if value (nconc node (list (list prop-id value))))
          (aset lnode 1 node))))
    (sgf-serialize-game-to-buffer ov)
    (message "Game information saved.")
    (kill-buffer)))

(defun sgf--move-to-existing-or-new-next-node (ov xy)
  "It check if the move of TURN color at position XY (note that XY could be
nil for a pass) exist in the next moves. If yes, move forward to it;
otherwise, create a new linked node and move the game state to it."
  (let* ((game-state (overlay-get ov 'game-state))
         (turn (aref game-state 3))
         (curr-lnode (aref game-state 0))
         (children (aref curr-lnode 2))
         (next-xys (mapcar (lambda (lnode) (cdr (sgf-process-move (aref lnode 1)))) children))
         (found (car (seq-positions next-xys xy))))
    (if found
        ;; Case 1: Clicked on one of the next move position
        (sgf-forward-move found ov t)
      ;; Case 2: Clicked on an empty position not equal to ko
      (if (sgf-game-plist-get :new-move ov)
          ;; only allow new move if it is set to be allowed
          (let* ((allow-suicide (sgf-game-plist-get :suicide-move ov))
                 (new-node (if xy `((,turn ,xy)) `((,turn))))
                 (new-lnode (sgf-linked-node curr-lnode new-node)))
            (sgf-apply-lnode new-lnode game-state allow-suicide)
            ;; add the new node as the last branch
            (aset curr-lnode 2 (nconc children (list new-lnode)))
            (sgf-update-display ov)
            (sgf-graph-tree ov)
            (sgf-serialize-game-to-buffer ov))
        (message "New move is set to be prohibited.")))))


(defun sgf-mouse-event-to-xy (event)
  "Convert a mouse click to a board position (X . Y)."
  (if (mouse-event-p event)
      (let* ((pos (event-start event))
             (image (posn-image pos))
             (scale (let ((s (image-property image :scale)))
                      ;; scale could be the symbol 'default
                      (if (eq s 'default) 1 s)))
             (size (float (* scale sgf-svg-size)))
             (xy (posn-object-x-y pos))
             (x (/ (car xy) size))
             (y (/ (cdr xy) size)))
        (message "%s: %.2f %.2f" xy x y)
        (cons (1- (round x)) (- (round y) 2)))))


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
         (ov (sgf-get-overlay)))
    (sgf--move-to-existing-or-new-next-node ov xy)))


(defun sgf-katago-get-next-move (xy)
  "Return the KataGo evaluation for the next move at the XY position."
  (let* ((moves (sgf-katago-get-next-moves))
         (move-xy (assoc xy moves))
         (info (cdr move-xy)))
    info))

(defun sgf-katago-get-next-moves ()
  "Return the KataGo evaluation for the next move of possible positions."
  (let* ((ov  (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (turn-number (aref game-state 6))
         (result (overlay-get ov 'katago-moves))
         (moves (and result (gethash turn-number result))))
    moves))


(defun sgf-board-click-right (event)
  "Right click on board pop up a menu."
  (interactive "@e")
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (board-2d (aref game-state 1))
         (h (length board-2d))
         (curr-lnode (aref game-state 0))
         (xy (sgf-mouse-event-to-xy event))
         ;; (xy-gtp (cons (car xy) (- h (cdr xy))))
         (katago-info (sgf-katago-get-next-move xy))
         (clicked-lnode (sgf-find-back-lnode xy game-state))
         (menu (cond (clicked-lnode
                      `("ACTION ON THIS MOVE"
                        ["Edit Comment"
                         ,(lambda () (interactive) (sgf-edit-comment clicked-lnode))]
                        ["Edit Move Number"
                         ,(lambda () (interactive) (sgf-edit-move-number clicked-lnode))
                         :enable ,(not (sgf-root-p clicked-lnode))]
                        ["Edit Move Annotation"
                         ,(lambda () (interactive) (sgf-edit-annotation clicked-lnode))
                         :enable ,(not (sgf-root-p clicked-lnode))]
                        ["Back to This Move"
                         ,(lambda () (interactive)
                            (sgf-goto-back-lnode clicked-lnode)
                            (sgf-update-display ov))
                         :help "Move game state to this move"
                         :enable ,(not (equal curr-lnode clicked-lnode))]
                        ["Prune to This Move"
                         ,(lambda () (interactive)
                            (sgf-goto-back-lnode clicked-lnode)
                            (sgf-prune t))
                         :enable ,(not (sgf-root-p clicked-lnode))] ; not root node
                        ["Put the Stone and Before as Setup"
                         ,(lambda () (interactive) (sgf-make-root clicked-lnode))
                         :enable ,(not (sgf-root-p clicked-lnode))]))
                     (katago-info `(,(format "KATAGO EVAL @ %s" xy)
                                    ,(format "win rate: %.2f%%\nscore: %.2f\nvisits: %d\npv: %s"
                                            (plist-get katago-info :winrate)
                                            (plist-get katago-info :score)
                                            (plist-get katago-info :visits)
                                            (plist-get katago-info :pv))))
                     (t `("No action or info to show.")))))
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
          (sgf-backward-move))))))


(defun sgf-find-back-lnode (xy game-state)
  "Search backward from the current game state to find the node that put
the corresponding stone at XY on board. There could be multiple nodes at
the same position during the whole game; this function finds the closest
one to the current game state.

Returns linked node found or nil if not. The game-state remains unchanged."
  (let* ((board-2d (aref game-state 1))
         (stone (sgf-board-get xy board-2d))
         (curr-lnode (aref game-state 0))
         found-lnode)
    (while (and (not found-lnode) (not (sgf-root-p curr-lnode)))
      (let* ((curr-node (aref curr-lnode 1))
             (play (sgf-process-move curr-node))
             (stone-i (car play))
             (xy-i (cdr play)))
        (if (and (eq stone-i stone) (equal xy-i xy))
            (setq found-lnode curr-lnode)
          (setq curr-lnode (aref curr-lnode 0)))))
    (or found-lnode
        (progn (message "No move is found at position %S." xy)
               nil))))

(defun sgf-pass ()
  "Pass the current. Add a new node of W[] or B[].

The move number will be incremented."
  (interactive)
  (let* ((ov (sgf-get-overlay)))
    (sgf--move-to-existing-or-new-next-node ov nil)))


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
         (prop-key (if (eq stone 'B) 'AB 'AW))  ; 'AB for black stones, 'AW for white stones
         (prop (assoc prop-key curr-node))
         (xys (cdr prop)))
    (if (sgf-root-p curr-lnode)
        (let ((xys (if (member xy xys)
                       (progn (sgf-board-set xy 'E board-2d)
                              (delete xy xys))  ;; Remove stone from the list
                     (progn (sgf-board-set xy stone board-2d)
                            (push xy xys)))))
          (if xys
              (if prop
                  (setcdr prop xys)  ; Update existing entry
                (nconc curr-node (list (cons prop-key xys)))) ; Add new property entry
            ;; If the xy list is empty, remove the property entirely
            (setq curr-node (assq-delete-all prop-key curr-node)))
          (sgf-serialize-game-to-buffer ov)
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


(defun sgf-edit-mark-function-maker (mark-type mark-name)
  "Create a function to add/delete a MARK-NAME mark on the board."
  (let ((fn-name (intern (format "sgf-edit-mark-%s" mark-name)))
        (action-fn (pcase mark-type
                     ((or 'SQ 'TR 'CR 'MA) 'sgf--action-mark-shape)
                     ('LB 'sgf--action-mark-label))))
    (fset fn-name
          (lambda ()
            "Add/delete a mark on the board of the current game state."
            (interactive)
            (unless (sgf-game-plist-get :show-marks)
              (message "Marks were not displayed. Enable it.")
              (sgf-toggle-marks))
            (sgf--handle-mouse-input
             (lambda (event) (interactive "e") (funcall action-fn event mark-type))
             (format "Click on the board to edit %s mark." mark-name))))
    fn-name))

;; Create the marking functions
(sgf-edit-mark-function-maker 'SQ "square")
(sgf-edit-mark-function-maker 'TR "triangle")
(sgf-edit-mark-function-maker 'CR "circle")
(sgf-edit-mark-function-maker 'MA "cross")
(sgf-edit-mark-function-maker 'LB "label")


(defun sgf--action-mark-shape (event shape)
  (let* ((xy (sgf-mouse-event-to-xy event))
         (ov (sgf-get-overlay))
         (curr-lnode (sgf-get-lnode ov))
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
    (sgf-serialize-game-to-buffer ov)
    ;; Update the display
    (sgf-update-display ov)))


(defun sgf--action-mark-label (event _)
  "Add, edit, or delete a label mark on the board of current game state."
  (interactive "e")
  (let* ((xy (sgf-mouse-event-to-xy event))
         (ov (sgf-get-overlay))
         (curr-lnode (sgf-get-lnode ov))
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
    (sgf-serialize-game-to-buffer ov)
    ;; Update the display
    (sgf-update-display ov)))


(defun sgf--action-delete-mark (event)
  "Delete a mark from the current node."
  (interactive "e")
  (let* ((xy (sgf-mouse-event-to-xy event))
         (ov  (sgf-get-overlay))
         (curr-lnode (sgf-get-lnode ov))
         (curr-node  (aref curr-lnode 1)))
    (dolist (shape '(SQ TR CR MA))
      (let ((curr-mark (assoc shape curr-node)))
        (when (and curr-mark (member xy curr-mark))
          (delete xy curr-mark)
          (message "Deleted mark at %s" xy)
          (sgf-serialize-game-to-buffer ov)
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
;;   (let* ((ov   (sgf-get-overlay))
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


(defun sgf-update-display (&optional ov no-move no-number no-hint no-katago)
  "Update the svg object and display according to the current game state."
  (interactive)
  (let* ((ov (or ov (sgf-get-overlay)))
         (svg (overlay-get ov 'svg))
         (game-state (overlay-get ov 'game-state))
         (board-2d   (aref game-state 1))
         (ko         (aref game-state 2))
         (turn       (aref game-state 3))
         (pcounts    (aref game-state 4))
         (curr-lnode (aref game-state 0))
         (curr-node  (aref curr-lnode 1)))
    (sgf-svg-update-ko svg ko)
    (unless no-move
      (sgf-svg-update-stones svg game-state)
      (sgf-svg-add-annotations svg game-state)
      (sgf-svg-update-status-prisoners svg pcounts)
      (sgf-svg-update-status-turn svg turn))
    (unless no-number
      (sgf-svg-update-numbers svg game-state))
    (unless no-hint
      (sgf-svg-update-hints svg curr-lnode)
      (sgf-svg-update-marks svg curr-node board-2d))
    (unless no-katago
      (let ((katago (sgf-katago-get-next-moves)))
        ;; if katago is nil (not analyzed for the next move), clear the
        ;; obsolete katago info on the board from the previous move.
        (sgf-svg-update-katago svg katago t)))
    (overlay-put ov 'svg svg)
    (sgf--display-svg ov)))


(defun sgf-buffer-update-hook (ov after beg end &optional _length)
  ;; (message "Before --- beg: %d end: %d" beg end)
  (when (and after                             ; after the text change
             ;; only update the overlay if it is current buffer that are modified.
             (eq (current-buffer) (window-buffer)))
    (let* ((inhibit-modification-hooks nil)
           (game-state (overlay-get ov 'game-state))
           (lnode (aref game-state 0))
           (path (sgf-lnode-path lnode))
           (new-game-state (sgf-parse-buffer-to-game-state beg end)))
      ;; (message "--- new game state\n: %S" new-game-state)
      ;; (message "--- path: %S" path)
      ;; (message "--- current buffer: %s" (buffer-substring-no-properties beg end))
      (overlay-put ov 'game-state new-game-state)
      ;; move to the move just before
      (setcar path (1- (car path)))
      ;; traverse and display
      (sgf-traverse path ov t))))


(defun sgf-remove-game-display ()
  "Remove the overlay and turn off the game display.

It also removes tree graph buffer if it exists."
  (interactive)
  (let* ((ov (sgf-get-overlay-at))
         (b (overlay-get ov 'graph-buffer)))
    (if b (kill-buffer b))
    (delete-overlay ov)))


(defun sgf--setup-overlay (ov game-state svg-hot-areas game-plist)
  "Setup overlay properties for the game."
  (let ((svg (car svg-hot-areas))
        (hot-areas (cdr svg-hot-areas)))
    (overlay-put ov 'game-plist game-plist)
    (overlay-put ov 'game-state game-state)
    (overlay-put ov 'svg svg)
    (overlay-put ov 'hot-areas hot-areas)
    (overlay-put ov 'keymap sgf-mode-display-map)
    (sgf--scroll-map-areas hot-areas sgf-mode-display-map)
    (overlay-put ov 'insert-behind-hooks '(sgf-buffer-update-hook))
    ;; Traverse to the specified game state and update display
    (sgf-traverse (plist-get game-plist :traverse-path) ov)
    ;; these svg group are visible when svg was created and needs to
    ;; be synced with game-plist when the overlay is initialized.
    (dolist (p '(:show-numbers :show-hints :show-marks :show-ko))
      (unless (plist-get game-plist p)
        (sgf-svg-toggle-visibility (sgf--svg-group-from-game-prop svg p))))
    (sgf-update-display ov)
    ov))


(defun sgf--display-svg (ov)
  "Display SVG in the overlay (as well as setting up keyboard)."
  (let ((svg (overlay-get ov 'svg))
        (hot-areas (overlay-get ov 'hot-areas))
        (scale (or (overlay-get ov 'image-scale)
                   (image-property (overlay-get ov 'display) :scale))))
    (unless (and svg hot-areas)
      (error "Overlay %S does not have 'svg' or 'hot-areas' properties" ov))
    (overlay-put ov 'keymap sgf-mode-display-map)
    (overlay-put ov 'display (svg-image svg :map hot-areas :scale scale))))


(defun sgf--hide-svg (ov)
  (let* ((img (overlay-get ov 'display))
         (scale (image-property img :scale)))
    (overlay-put ov 'image-scale scale)
    (overlay-put ov 'display nil)
    (overlay-put ov 'keymap nil)))


(defun sgf-toggle-game-display (&optional beg end game-plist)
  "Toggle graphical display of the game.

If the overlay is not created, it will create the SGF overlay with the
region between BEG and END (when nil, the whole buffer is parsed as SGF
content.

If the overlay exists, it keeps unchanged."

  (interactive)
  (if (use-region-p)
      (setq beg (region-beginning) end (region-end))
    (setq beg (or beg (point-min))
          end (or end (point-max))))

  (let* ((ov (ignore-errors (sgf-get-overlay))))
    (if ov
        (if (overlay-get ov 'display) (sgf--hide-svg ov) (sgf--display-svg ov))
      ;; set front- and rear-advance parameters to allow
      ;; the overlay cover the whole buffer even if it is
      ;; updated from game playing.
      (let* ((ov (make-overlay beg end nil nil t))
             (game-state (sgf-parse-buffer-to-game-state beg end))
             (board-2d   (aref game-state 1))
             (h (length board-2d))
             (w (length (aref board-2d 0)))
             (svg-hot-areas (sgf-svg-init w h)))
        (unless game-plist (setq game-plist (sgf-default-game-plist)))
        (sgf--setup-overlay ov game-state svg-hot-areas game-plist)))))


(defun sgf-init-new-game (&optional beg end game-plist)
  "Initialize a new game and overlay with empty SGF content.

The existing SGF content in the buffer will be erased."

  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))

  (let* ((w (read-number "board width: " 19))
         (h (read-number "board height: " 19))
         (b-w (read-char-choice "Player to start (b)lack or (w)hite: " '(?b ?w)))
         (pl (if (eq b-w ?b) 'B 'W))
         (root-node `((FF 4)
                      (GM 1)
                      (DT ,(format-time-string "%Y-%m-%d"))
                      (SZ (,w . ,h))
                      (PL ,pl)))
         (root-lnode (sgf-linked-node nil root-node nil))
         (game-state (sgf-root-lnode-to-game-state root-lnode))
         (svg-hot-areas (sgf-svg-init w h))
         (ov (make-overlay beg end nil nil t)))
    (unless game-plist (setq game-plist (sgf-default-game-plist)))
    ;; update buffer content; otherwise, the *empty* overlay (empty
    ;; overlays are overlays cover no text) won't display.
    (sgf--setup-overlay ov game-state svg-hot-areas game-plist)
    (sgf-serialize-game-to-buffer ov)))

(defun sgf-katago-analyze (&optional whole-game-p)
  "Analyze the whole game or the next move only with KataGo.

If WHOLE-GAME-P (argument prefix) is non-nil, analyze the whole game,
otherwise analyze next move."
  (interactive "P")
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (depth (aref game-state 5))
         (lnode (aref game-state 0))
         (json (sgf-serialize-lnode-to-json lnode whole-game-p))
         (result (or (overlay-get ov 'katago-moves) (make-hash-table)))
         (callback (lambda (t m)
                     (puthash t m result)
                     ;; these have to be put inside callback;
                     ;; otherwise, they will be executed before the
                     ;; asynchronous process is done.
                     (overlay-put ov 'katago-moves result)
                     (sgf-update-display ov t t t))))
    (unless katago-analysis-process (katago-analysis-init))
    (message "%s" json)
    (katago-analysis-query (json-encode json) callback)))

;; (defun sgf-katago-compute-scores ()
;;   "Compute the score of the current game state with KataGo."
;;   (interactive)
;;   (let* ((ov (sgf-get-overlay))
;;          (game-state (overlay-get ov 'game-state))
;;          (lnode (aref game-state 0))
;;          (root (sgf-get-root))
;;          children)
;;     (while (setq children (sgf-get-children root))
;;       (setq root (car children)))

;;     (unless katago-analysis-process (katago-analysis-init))
;;     (katago-analysis-query (json-encode json) callback)))


(defvar-keymap sgf-mode-map
  :doc "Keymap for SGF major mode."
  "C-c C-c" #'sgf-toggle-game-display ; use the org babel evaluate binding
  "C-c s i" #'sgf-init-new-game
  "C-c s r" #'sgf-remove-game-display)


(defvar-keymap sgf-mode-display-map
  :doc   "Keymap for the overlay svg display.
It is set as overlay property and only activated when the overlay is displayed."
  :suppress t
  "+"   #'image-increase-size
  "-"   #'image-decrease-size
  "g"   #'sgf-graph-hv
  "z"   #'sgf-export-image
  "c"   #'sgf-show-comment
  "p"   #'sgf-show-path
  "f"   #'sgf-forward-move "<right>" #'sgf-forward-move
  "b"   #'sgf-backward-move "<left>" #'sgf-backward-move
  "M-f" #'sgf-forward-fork
  "M-b" #'sgf-backward-fork
  "a"   #'sgf-first-move
  "e"   #'sgf-last-move
  "j"   #'sgf-jump-moves
  "t"   #'sgf-traverse
  "r"   #'sgf-back-to-game
  "k a" #'sgf-katago-analyze
  "s n" #'sgf-toggle-numbers
  "s m" #'sgf-toggle-marks
  "s h" #'sgf-toggle-hints
  "s k" #'sgf-toggle-ko
  "s s" #'sgf-toggle-new-move
  "s a" #'sgf-toggle-katago
  "s i" #'sgf-toggle-katago-metrics
  "m p" #'sgf-pass
  "m r" #'sgf-make-root
  "m k" #'sgf-prune-inclusive ; kill node
  "m K" #'sgf-prune
  "m c" #'sgf-edit-comment
  "m n" #'sgf-edit-move-number
  "m a" #'sgf-edit-mark-triangle
  "m d" #'sgf-edit-mark-square
  "m o" #'sgf-edit-mark-circle
  "m x" #'sgf-edit-mark-cross
  "m l" #'sgf-edit-mark-label
  "m -" #'sgf-delete-mark
  "m b" #'sgf-edit-setup-black-stone
  "m w" #'sgf-edit-setup-white-stone
  "m i" #'sgf-edit-game-info
  "m h" #'sgf-edit-annotation ; highlight
  "m m" #'sgf-merge-branches
  "m s" #'sgf-swap-branches
  "m v" #'sgf-remove-variations
  "<hot-grid> <mouse-1>" #'sgf-board-click-left
  "<hot-grid> <mouse-3>" #'sgf-board-click-right)


(defun sgf--scroll-map-areas (hot-areas keymap)
  "Allow scrolling for all the map areas on the board."
  (require 'pixel-scroll)
  (dolist (area hot-areas)
    ;; area example: ((rect (30 . 55) . (80 . 80)) hot-grid (pointer hand))
    (let ((area-id (cadr area)))
      (define-key keymap (vector area-id 'wheel-up) #'pixel-scroll-precision)
      (define-key keymap (vector area-id 'wheel-down) #'pixel-scroll-precision))))


(defun sgf-setup (&optional vertical)
  (whitespace-cleanup)
  (let ((beg (point-min)) (end (point-max)))
    (if (eq beg end)
        (sgf-init-new-game beg end)
      (sgf-toggle-game-display beg end))
    (sgf-graph-hv vertical)))


(defcustom sgf-mode-hook nil
  "Hook run when entering SGF mode."
  :type 'hook
  :group 'sgf)


;;;###autoload
(define-derived-mode sgf-mode nil "SGF"
  "Major mode for editing SGF files. The following commands are available:
\\{sgf-mode-map}"
  :keymap sgf-mode-map)


;;;###autoload
(add-hook 'sgf-mode-hook 'sgf-setup)


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sgf\\'" . sgf-mode))


(provide 'sgf-mode)
;;; sgf-mode.el ends here
