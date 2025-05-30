;;; sgf-util.el --- util functions -*- lexical-binding: t -*-


;; Author: Zech Xu
;; Version: 1.0
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://github.com/RNAer/sgf-mode
;; Keywords: SGF, go, game

;;; Commentary:

;;; Code:

(defvar sgf-bm-color "red" "Color for the node of the bad move")
(defvar sgf-do-color "blue" "Color for the node of the doubtful move")
(defvar sgf-it-color "yellow" "Color for the node of the interesting move")
(defvar sgf-te-color "green" "Color for the node of the tesuji move")

(defcustom sgf-show-hints t
  "Show the hint mark(s) for next move(s)."
  :type '(boolean)
  :group 'sgf)

(defcustom sgf-show-numbers t
  "Show move number on the stones."
  :type '(boolean)
  :group 'sgf)

(defcustom sgf-show-marks t
  "Show marks on the board."
  :type '(boolean)
  :group 'sgf)

(defcustom sgf-show-ko t
  "Show KO position on the board."
  :type '(boolean)
  :group 'sgf)

(defcustom sgf-show-katago t
  "Show katago analysis on the board."
  :type '(boolean)
  :group 'sgf)


(defcustom sgf-new-move nil
  "Do not allow new move on the game. It is useful for exam to check if you clicked and played right next move (because it will not show up if the move is not in the game). However, it allow other changes (eg comment and mark modification)."
  :type '(boolean)
  :group 'sgf)

(defcustom sgf-suicide-move nil
  "Allow suicide or not. Some rule set allow suicide: https://senseis.xmp.net/?Suicide"
  :type '(boolean)
  :group 'sgf)


(defcustom sgf-traverse-path nil
  "Default path to traverse thru when initiate game and display.

Examples:

1. `nil' or `0'. do nothing and stay at the beginning of the game.

2. `t'. move to the end of the game. choose the first branch when come across a fork.

3. `-1'. similar to Example 2 except stay at the move next to the last.

4. `3'. similar to Example 2 except stay at the 3rd move from the beginning.

5. `(34 ?a ?b ?a)'. move forward 34 steps in total by selecting branch a, b and then a respectively for the first 3 forks.

See also `sgf-traverse'."
  :type '(radio (integer :tag "Number of moves from start (positive number) or from end (negative number)")
                (boolean :tag "Start or end of the game")
                (cons :tag "Total step number and branches"
                      (natnum :tag "number of steps" :value 9)
                      (repeat :tag "branch character(s)"
                              (character :tag "char (a-zA-Z)" :value ?a))))
  :group 'sgf)


(defun sgf-default-game-plist ()
  "Return the global default game option list."
  `(:new-move ,sgf-new-move
              :show-hints ,sgf-show-hints
              :show-numbers ,sgf-show-numbers
              :show-marks ,sgf-show-marks
              :show-ko ,sgf-show-ko
              :show-katago ,sgf-show-katago
              :suicide-move ,sgf-suicide-move
              :traverse-path ,sgf-traverse-path))


(defun sgf-update-game-plist (input-plist &rest plist)
  "Create a property list for the game.

Update the global default variable value in the plist from PLIST.

 Examples:
(sgf-update-game-plist (sgf-default-game-plist) :foo 1 :editable nil)
(sgf-update-game-plist (sgf-default-game-plist) :show-marks t :traverse-path \\='(9 ?b ?a))"
  (while plist
    (let* ((key (pop plist))
           (value (pop plist)))
      (if (plist-member input-plist key)
          ;; update only existing customizable property
          (plist-put input-plist key value))))
  input-plist)


(defun sgf-game-plist-get (key &optional ov)
  "Return game property of KEY for the overlay."
  (let* ((ov (or ov (sgf-get-overlay)))
         (game-plist (overlay-get ov 'game-plist)))
    (plist-get game-plist key)))


(defun sgf-game-plist-set (key value &optional ov)
  "Set game property of KEY for the overlay."
  (let* ((ov (or ov (sgf-get-overlay)))
         (game-plist (overlay-get ov 'game-plist)))
    (plist-put game-plist key value)))


(defun sgf-game-plist-toggle (key &optional ov)
  "Toggle the game property of KEY."
  (let* ((ov (or ov (sgf-get-overlay)))
         (game-plist (overlay-get ov 'game-plist)))
    (sgf-game-plist-set key (not (plist-get game-plist key)) ov)))


(defun sgf-toggle (flag &optional true-or-false)
  "Toggle the FLAG boolean option.

Examples: (sgf-toggle t) => nil
          (sgf-toggle t 'true) => t
          (sgf-toggle nil) => t
          (sgf-toggle nil 'false) => nil"
  (cond ((null true-or-false) (not flag)) ; toggle
        ((eq true-or-false 'true) t)
        ((eq true-or-false 'false) nil)
        (t flag)))


(defun sgf-merge-alist (alist)
  "Merge items with the same key in ALIST."
  (let ((result '()))
    (dolist (item alist)
      (let* ((key (car item))
             (value (cdr item))
             (existing (assq key result))
             (new (nconc (cdr existing) value)))
        ;; If key doesn't exist, add new entry with value in a list
        (setf (alist-get key result) (delete-dups new))))
    (nreverse result)))


(defun sgf-get-overlay-at (&optional pos)
  "Return the SGF overlay at POS position in the current buffer.

See also `sgf-get-overlay'. Use this function if no mouse event is involved."
  (let* ((pos (or pos (point)))
         (ovs (overlays-in (1- pos) (1+ pos)))
         sgf-ov)
    (while (and ovs (not sgf-ov))
      (let ((ov (pop ovs)))
        ;; make sure get the right overlay
        (if (overlay-get ov 'game-state)
            (setq sgf-ov ov))))
    (or sgf-ov
        (error "No SGF overlay found at position %d in buffer %s. Try moving point to an overlay region." pos (buffer-name)))))


(defun sgf-get-overlay ()
  "Return the SGF overlay (even if mouse clicked on non current buffer).

See also `sgf-get-overlay-at'. Use this function if mouse event is involved."
  (if (or (mouse-event-p last-input-event)
          (memq (event-basic-type last-input-event) '(wheel-up wheel-down)))
      (let* ((mouse-pos (event-start last-input-event))
             (pos    (posn-point mouse-pos))
             (window (posn-window mouse-pos))
             (buffer (window-buffer window)))
        (set-window-point window pos)
        (with-current-buffer buffer
          (sgf-get-overlay-at pos)))
    (sgf-get-overlay-at (point))))


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

If \\='B or \\='W is present in the node, return (B . (x . y) or (W . (x . y).
If \\='B or \\='W exists without coordinates, return (B) or (W).
If neither \\='B nor \\='W is present, return nil."
  (pcase (or (assoc 'B node) (assoc 'W node))
    (`(,stone (,x . ,y)) (cons stone (cons x y))) ;; Extract (B/W (x . y)) case
    (`(,stone) (list stone))                  ;; Handle (B) or (W)
    (_ nil)))                                 ;; If nothing found, return nil


(defun sgf-add-setup-stones (node board-2d)
  "Process NODE to add setup stones to BOARD-2D.

Return a cons cell of the form (B-XYS . W-XYS) where B-XYS and W-XYS are
lists of black and white stones respectively. Return nil if no setup stones."
  (let ((b-xys (alist-get 'AB node))
        (w-xys (alist-get 'AW node)));; return nil if 'AB does not exist
    (dolist (xy b-xys)
      (sgf-board-set xy 'B board-2d))
    (dolist (xy w-xys)
      (sgf-board-set xy 'W board-2d))
    (cons b-xys w-xys)))


(define-inline sgf-get-lnode (&optional ov)
  "Return the lnode of the overlay OV."
  (inline-quote (let* ((ov (or ,ov (sgf-get-overlay)))
                       (game-state (overlay-get ov 'game-state)))
                  (aref game-state 0))))

(define-inline sgf-get-parent (lnode)
  "Return the parent node of LNODE."
  (inline-quote (aref ,lnode 0)))

(define-inline sgf-get-children (lnode)
  "Return the children nodes of LNODE."
  (inline-quote (aref ,lnode 2)))

(define-inline sgf-get-siblings (lnode)
  "Return the sibling nodes of LNODE.

Return nil if LNODE is the root node."
  (inline-quote (let ((parent (sgf-get-parent ,lnode)))
                  (aref parent 2))))

(define-inline sgf-get-root (lnode)
  "Return the root of LNODE."
  (inline-quote (while (not (sgf-root-p lnode))
                    (setq lnode (sgf-get-parent lnode)))))

(define-inline sgf-node-data (lnode)
  "Return the node of LNODE."
  (inline-quote (aref ,lnode 1)))

(define-inline sgf-root-p (lnode)
  "Check if LNODE is the root node."
  (inline-quote (null (sgf-get-parent ,lnode))))

(define-inline sgf-path-to-str (path)
  "Return the str of the path in the form of `(steps branch-1 branch-2 ...)'.

(sgf-path-to-str '(3 ?a ?b ?a)) => \"(3 a b a)\""
  (inline-quote (format "(%d %s)"
                        (car ,path)
                        (mapconcat #'char-to-string (nthcdr 1 ,path) " "))))


(defun sgf-lnode-move-number (lnode)
  "Return the move number for the LNODE.

It computes the depth of LNODE from the root node or previous MN
property, not include setup node."
  (let ((num 0) mn-prop)
    (while (and (not (sgf-root-p lnode))
                (not (setq mn-prop (car (alist-get 'MN (aref lnode 1)))))
                (setq num (1+ num)))
      (setq lnode (aref lnode 0)))
    (+ (or mn-prop 0) num)))


(defun sgf-lnode-path (lnode)
  "Return the path in the form of `(steps branch-1 branch-2 ...)' to reach
LNODE from the root.

The return value can be passed to `sgf-traverse'."
  (let ((depth 0) (branch-choices '()))
    (while (not (sgf-root-p lnode))
      (let* ((parent (aref lnode 0))
             (siblings (aref parent 2)))
        (if (> (length siblings) 1)
            ;; Find the index of the current lnode in sibling nodes and
            ;; append to the end of branch choices
            (let ((pos (seq-position siblings lnode)))
              (if pos
                  (push (+ ?a pos) branch-choices)
                (error "Current lnode not found in siblings: %S" (aref lnode 1)))))
        (setq lnode parent
              depth (1+ depth))))
    ;; (setq branch-choices (nreverse branch-choices))
    (cons depth branch-choices)))


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


(defun sgf-board-create (w h &optional default)
  "Create a empty 2D board of size WxH with DEFAULT value."
  (let ((board-2d (make-vector h nil)))
    (dotimes (i h) ;; for each row
      (aset board-2d i (make-vector w default)))
    board-2d))


(defun sgf-board-clear (board-2d)
  "Clear 2D board"
  (dotimes (i (length board-2d))       ;; Loop over rows
    (dotimes (j (length (aref board-2d i)))  ;; Loop over columns in each row
      (sgf-board-set (cons i j) 'E board-2d))))  ;; Set each cell to 'E'


(defun sgf-board-get (xy board-2d)
  (aref (aref board-2d (cdr xy)) (car xy)))


(defun sgf-board-set (xy v board-2d)
  (aset (aref board-2d (cdr xy)) (car xy) v))


(defun sgf-enemy-stone (stone)
  "Return the opponent stone of STONE."
  (if (equal stone 'B) 'W 'B))


(defun sgf-valid-stone-p (stone)
  "Check if STONE is a valid color."
  (or (eq stone 'B) (eq stone 'W)))


(defun sgf-xy-on-board-p (xy board-2d)
  "Check if XY is on the board."
  (let ((x (car xy)) (y (cdr xy)))
    (and (>= x 0) (< x (length (aref board-2d 0)))
         (>= y 0) (< y (length board-2d)))))


(defun sgf-xy-is-empty-p (xy board-2d)
  "Check if XY is empty on the board."
  (eq (sgf-board-get xy board-2d) 'E))


(defun sgf-valid-move-p (xy stone board-2d ko)
  "Check if the move of STONE at XY position on BOARD-2D is valid"
  (and
   board-2d
   (sgf-valid-stone-p stone)       ;; valid color
   (or (null xy)                       ;; pass move
       (and
        (sgf-xy-on-board-p xy board-2d) ;; position is on board
        (sgf-xy-is-empty-p xy board-2d) ;; no stone at this position yet
        (not (equal xy ko))))))           ;; pos is not ko


(defun sgf-neighbors-xy (xy board-2d)
  "Return a list of neighboring positions of XY on a BOARD-2D of size W x H."
  (let* ((x (car xy)) (y (cdr xy))
         (w (length (aref board-2d 0)))
         (h (length board-2d)))
    (delq nil
          (mapcar (lambda (offset)
                    (let ((nx (+ x (car offset)))
                          (ny (+ y (cdr offset))))
                      (if (and (>= nx 0) (< nx w) (>= ny 0) (< ny h))
                          (cons nx ny))))
                  '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))))))


(defun sgf-check-liberty (xy board-2d &optional prev-stone visited)
  "Check if the stone on position XY of BOARD-2D is alive or suiciding.

It returns a cons cell of the form (LIBERTY-FOUND . VISITED). Its car
indicates if a liberty is found, and its cdr is a list of visited
positions to avoid loops, which also stores all the dead positions if no
liberty is found.

If xy is nil (for a move of pass), it returns (t . nil)."
  (let ((xy-state (sgf-board-get xy board-2d)))
    (cond
     ((eq xy-state 'E) (cons t visited))      ; Empty space = liberty
     ((member xy visited) (cons nil visited)) ; Already checked
     ((or (null prev-stone) (eq prev-stone xy-state))
      (let ((liberty-found nil)
            (visited (cons xy visited))
            (neighbors (sgf-neighbors-xy xy board-2d)))
        (while (and neighbors (not liberty-found))
          (let* ((neighbor (pop neighbors))
                 (result (sgf-check-liberty neighbor board-2d xy-state visited)))
            ;; don't put these setqs in the let block
            (setq liberty-found (car result))
            (setq visited (cdr result))))
        (cons liberty-found visited)))
     (t (cons nil visited)))))


(defun sgf-capture-stones (xy board-2d)
  "Compute if the stone at position XY captures any enemy stones.

Return the list of positions for the prisoners captured."
  (let ((xy-state (sgf-board-get xy board-2d))
        prisoners)
    (dolist (neighbor (sgf-neighbors-xy xy board-2d))
      (when (and neighbor
                 ;; the neighbor is empty or an enemy stone
                 (not (eq xy-state (sgf-board-get neighbor board-2d)))
                 ;; the neighbor is already captured
                 (not (member neighbor prisoners)))
        (let ((results (sgf-check-liberty neighbor board-2d)))
          (unless (car results)         ; no liberty found
            (setq prisoners (nconc (cdr results) prisoners))))))
    prisoners))


(defun sgf-suicide-stones (xy board-2d)
  "Compute if the stone at position XY is suiciding.

Return the list of positions for the suiciding stones.
If XY is nil (for a move of pass), it returns nil."
  (let ((results (sgf-check-liberty xy board-2d)))
    (unless (car results)
      (cdr results))))


(defun sgf-get-ko (xy stone board-2d captured-xys)
  "Check if any neighbor of XY is a KO position.

The previous move has put STONE at XY on BOARD-2D and possibly captured
enemy stones at positions CAPTURED-XYS (which is a list of cons cells
indicating captured positions).

Returns nil or KO position in the form of (x . y). "

  (when (and (memq stone '(B W)) (= 1 (length captured-xys)))
    ;; not a KO if more than 1 stones were captured after the move at XY.
    (let* ((neighbors (sgf-neighbors-xy xy board-2d))
           (empty-or-same-color-count 0)
           empty-or-same-color-position)
      (dolist (neighbor neighbors)
        (let ((state (sgf-board-get neighbor board-2d)))
          (when (or (eq state stone) (eq state 'E))
            (setq empty-or-same-color-count (1+ empty-or-same-color-count))
            (setq empty-or-same-color-position neighbor))))
      (if (= empty-or-same-color-count 1)
          empty-or-same-color-position))))


(provide 'sgf-util)
;;; sgf-util.el ends here
