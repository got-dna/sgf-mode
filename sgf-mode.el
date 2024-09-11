;;; sgf-mode.el --- SGF Editing Mode  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;;; Code:


(defvar sgf-space-re "[ \t\n\r]*")

(defvar sgf-property-value-re
  (rx (seq "["                         ; Empty or non-empty value: starts with "["
           (group
            (zero-or-more
             (or (seq "\\" (any "[]"))   ; Match escaped "[" or "]"...
                 (not (any "\\]")))))    ; Or any character except closing bracket or unescaped "\"
           "]")                        ; Ends with "[
      ))

;; use `regexp' to dynamic include other regexp patterns
(defvar sgf-property-re
  (rx (group (one-or-more alpha))        ; Property name: one or more alphabetic characters
      (regexp sgf-space-re)              ; Optional spaces between name and values
      (one-or-more                       ; Property values: one or more occurrences
        (regexp sgf-property-value-re)   ; Match a single value
        (regexp sgf-space-re)            ; Optional spaces between values
        ))
  "Regular expression to match a single SGF property and its value(s), allowing whitespace(s) in between.")


(defvar sgf-node-re
  (rx ";"
      (regexp sgf-space-re)
      (one-or-more (regexp sgf-property-re)))
  "Regular expression to match a single node. A node starts with a semicolon and contains one or more properties.")


(defun string-to-number-or-nil (str)
  "Return the number represented by STR, or nil if STR is not a valid number."
  (if (string-match-p (rx
                       string-start
                       (optional (any "-+"))          ; Optional sign
                       (or
                        (seq (one-or-more digit)       ; Digits before the decimal point
                             (optional (seq "." (zero-or-more digit)))) ; Optional decimal point and digits
                        (seq "." (one-or-more digit))) ; Decimal point with digits
                       string-end)
                      str)

      (string-to-number str)))

;;; Specific property converters
(defun sgf-convert-DT (val)
  "for property DT"
  (parse-time-string val))


;; define a helper function to convert a position char to int
(defun char-to-num (char)
  "?a-?z ?A-?Z to 0-25 26-51.

(char-to-num ?a) => 0
(char-to-num ?z) => 25"
  (cond
   ((and (>= char ?a) (<= char ?z)) (- char ?a))
   ((and (>= char ?A) (<= char ?Z)) (+ (- char ?A) 26))
   (t (error "Invalid point letter '%c'" char))))

(defun sgf-convert-pos (pos-str)
  "(sgf-convert-pos \"aa\") => \"(0 . 0)\""
  ;; aa:ac like str is odd in length
  (if (string-empty-p pos-str)
      "" ; this is a pass. position is empty
    (if (= (logand (seq-length pos-str) 1) 1)
        (mapconcat (lambda (i) (format "%S" i)) (sgf-convert-multi-positions pos-str) " ")
      (format "%S" (sgf-convert-single-position pos-str)))))

(defun sgf-convert-single-position (pos-str)
  "for property AB, AW, AE"
   (cons (char-to-num (aref pos-str 0))
         (char-to-num (aref pos-str 1))))

(defun sgf-convert-multi-positions (pos-str)
  "Convert a multi-position string in 'aa:ac' format into a list of numeric pairs."
  (let* ((positions (mapcar #'sgf-convert-single-position (split-string pos-str ":")))
         (first (car positions)) (last (cadr positions))
         (first-x (car first)) (first-y (cdr first))
         (last-x (car last)) (last-y (cdr last)))
    (if (= first-x last-x)
        ;; either x is the same
        (mapcar (lambda (i) (cons first-x i)) (number-sequence first-y last-y))
      ;; or y the same
      (mapcar (lambda (i) (cons i first-y)) (number-sequence first-x last-x)))))

(ert-deftest sgf-convert-multi-positions-test ()
  (let ((cases '(("aa:ac" . ((0 . 0) (0 . 1) (0 . 2)))
                 ("aa:ca" . ((0 . 0) (1 . 0) (2 . 0))))))
    (dolist (i cases)
      (should (equal (sgf-convert-multi-positions (car i)) (cdr i))))))


(defun sgf-convert-LB (label-str)
  "for property LB: label a position or stone. e.g. LB[ee:label]"
  (let ((re (rx (group (repeat 2 (any "a-zA-Z"))) ":" (group (zero-or-more anything)))))
    (if (string-match re label-str)
        (format "%S" (cons (sgf-convert-single-position (match-string 1 label-str))
                           (match-string 2 label-str)))
      (error "Malformed SGF label %S" label-str))))

(defun sgf-convert-SZ (str)
  "Process the SZ property from an SGF file and return a cons cell of board dimensions."
  (let* ((nums (split-string str ":"))
         (width (string-to-number (car nums)))
         (height (string-to-number (or (cadr nums) (car nums)))))
    (format "%S" (cons width height))))

;; todo: use plist instead of alists?
(defun sgf-buffer-to-game-tree ()
  "Convert and replace the content in SGF-BUFFER to an eLisp list.

If SGF-BUFFER is nil, use the current buffer. The buffer content should be SGF strings."
  (let ((property-processors '(("DT" . sgf-convert-DT)
                               ("SZ" . sgf-convert-SZ)
                               ("B"  . sgf-convert-pos)
                               ("W"  . sgf-convert-pos)
                               ("AB" . sgf-convert-pos)
                               ("AW" . sgf-convert-pos)
                               ("AE" . sgf-convert-pos)
                               ("SQ" . sgf-convert-pos)
                               ("TR" . sgf-convert-pos)
                               ("LB" . sgf-convert-LB)))
        processor
        beg-prop beg-prop-val node prop prop-key prop-val)
    (with-current-buffer (current-buffer)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward sgf-node-re nil t)
          (setq node (match-string 0))
          (replace-match "(") ; delete the node SGF string and insert "(".
          (while (string-match sgf-property-re node beg-prop)
            (setq beg-prop (match-end 0))
            (setq prop (match-string 0 node))
            (setq prop-key (match-string 1 node))
            (insert "(")
            (insert (upcase prop-key))
            (setq processor (cdr (assoc prop-key property-processors)))
            (while (string-match sgf-property-value-re prop beg-prop-val)
              (setq beg-prop-val (match-end 0))
              (setq prop-val (match-string 1 prop))
              (insert " ")
              (if processor
                  (setq prop-val (funcall processor prop-val))
                ;; try convert to a number
                (setq prop-val (or (string-to-number-or-nil prop-val)
                                   (format "%S" prop-val))))
              (insert (format "%s" prop-val)))
            (insert ")")
            (setq beg-prop-val 0))
          (insert ")")
          (setq beg-prop 0))))))


(defun sgf-buffer-to-game-tree-new (&optional new-buffer)
  "Convert the content of SGF-BUFFER to emacs-lisp in a new buffer."
  (interactive "sNew buffer name: ")
  (let* ((buffer (generate-new-buffer new-buffer))
         (sgf-str (buffer-string)))
    (with-current-buffer buffer
      (insert sgf-str)
      (goto-char (point-min))
      (sgf-buffer-to-game-tree)
      (emacs-lisp-mode))
    (pop-to-buffer buffer)))


(defun sgf-str-to-game-tree (str)
  "Convert a string of sgf into the equivalent Emacs Lisp."
  (interactive)
  (with-temp-buffer
    (insert str)
    (sgf-buffer-to-game-tree)
    (goto-char (point-min))
    (read (current-buffer))))


(defun sgf-file-to-game-tree (file)
  "Convert the sgf contents of FILE to emacs lisp."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (sgf-buffer-to-game-tree)
    (goto-char (point-min))
    (read (current-buffer))))


(defun sgf-str-from-prop (prop)
  "Convert a property to an SGF string.

(sgf-str-from-prop '(B (0 . 0) (1 . 0))) => B[aa][ba]
(sgf-str-from-prop '(FF 4)) => FF[4]
(sgf-str-from-prop '(AB (1 . 1) (1 . 2))) => AB[bb][bc]
(sgf-str-from-prop '(SZ (15 . 13))) => SZ[15:13]
(sgf-str-from-prop '(C \"comment\")) => C[comment]"
  (let (prop-key prop-val-str)
    (setq prop-key (car prop))
    (setq prop-val-str
          (mapconcat (lambda (prop-val)
                       (cond ((or (eq prop-key 'B) (eq prop-key 'W)
                                  (eq prop-key 'AB) (eq prop-key 'AW))
                              (format "[%c%c]"
                                      (+ ?a (car prop-val))
                                      (+ ?a (cdr prop-val))))
                             ((eq prop-key 'SZ)
                              (let ((x (car prop-val))
                                    (y (cdr prop-val)))
                                (if (= x y) (format "[%d]" x) (format "[%d:%d]" x y))))
                             (t (format "[%s]" prop-val))))
                     (cdr prop)))
    (format "%S%s" prop-key prop-val-str)))


(defun sgf-str-from-node (node)
  "Convert a node to an SGF string.

(sgf-str-from-node '((FF 4) (SZ (15 . 13)) (AB (1 . 1) (1 . 2))))"
  (concat ";" (mapconcat (lambda (prop) (sgf-str-from-prop prop)) node)))


(ert-deftest sgf-str-from-node-test ()
  (should (string= (sgf-str-from-node '((B (0 . 0) (1 . 0)) (C "comment")))
                   ";B[aa][ba]C[comment]")))


(defun sgf-str-from-game-tree (lnode)
  "Convert a game tree to an SGF string."
  (let ((curr-lnode lnode)
        (next-lnodes (aref lnode 2))
        (node-str (sgf-str-from-node (aref lnode 1))))
    (if (null next-lnodes)
        node-str
      (let ((next-strs (mapcar #'sgf-from-game-tree next-lnodes)))
        (if (= (length next-lnodes) 1)
            ;; No fork, just append the next node string
            (concat node-str (car next-strs))
          ;; Fork, wrap each branch in parentheses
          (concat node-str "(" (mapconcat #'identity next-strs ")(") ")"))))))



(ert-deftest sgf-cycle-test ()
  (let* ((cases '("(;B[aa][ba]C[comment])"
                  "(;B[aa][ba]C[comment](;W[cc]C[comment])(;W[ee]))")))
    (dolist (sgf-str cases)
      (setq sgf-lst (sgf-str-to-game-tree sgf-str))
      (setq root (car sgf-lst))
      (setq root-lnode (board-linked-node nil root))
      (sgf-link-nodes-in-branch (cdr sgf-lst) root-lnode)
      (should (string= (format "(%s)" (sgf-str-from-game-tree root-lnode)) sgf-str)))))



(defun sgf-process-play (node)
  "Process a play node."
  (let* ((color (if (assoc 'B node) 'B 'W))
         (xy (car (alist-get color node)))
         (comment (car (alist-get 'C node))))
    (if comment (message comment))
    (cons color xy)))


(defconst sgf-game-info-properties
  '(("CP" "Copyright")
    ("US" "Enterer Name")
    ("AN" "Annotator Name")
    ("SO" "Source")
    ("EV" "Event Name")
    ("GN" "Game Name")
    ("RO" "Round Number")
    ("DT" "Date")
    ("PC" "Place")
    ("BT" "Black Team" :player black)
    ("PB" "Black Player" :player black)
    ("BR" "Black Rank" :player black)
    ("WT" "White Team" :player white)
    ("PW" "White Player" :player white)
    ("WR" "White Rank" :player white)
    ("RU" "Rule")
    ("OT" "Overtime Method")
    ("TM" "Time Limit" :type real)
    ("HA" "Handicap Stones" :type number)
    ("KM" "Komi" :type real)
    ("RE" "Result")
    ("ON" "Opening Moves")
    ("GC" "Comment" :type text))
  "Game Info Properties")


(defun nested-level-of-first (lst)
  "Return the nesting level of the first element in the list LST.
(nested-level-of-first '(a b (c d))) => 0
(nested-level-of-first '((a b) c d)) => 1"
  (if (listp (car lst))
      (1+ (nested-level-of-first (car lst)))  ;; If the first element is a list, go deeper.
    0))

(defun sgf-link-nodes-in-branch (game-branch head-lnode)
  "Link up nodes in a branch of the game tree."
  (let* ((prev-lnode head-lnode)
         curr-lnode nested-level)
    (dolist (i game-branch)
      (setq nested-level (nested-level-of-first i))
      (cond ((= nested-level 2) ;; this is a fork
             (sgf-link-nodes-in-branch i prev-lnode))
            ((= nested-level 1) ;; this is a node
               ;; create new node
               (setq curr-lnode (board-linked-node prev-lnode i))
               ;; link prev-node to the new node
               (aset prev-lnode 2 (append (aref prev-lnode 2) (list curr-lnode)))
               ;; this line reversed the order of branches
               ;; (aset prev-lnode 2 (cons curr-lnode (aref prev-lnode 2)))
               (setq prev-lnode curr-lnode))))))

;; todo: unwind-protect to restore the original state if anything goes wrong.
(defun sgf-forward-node (&optional branch)
  "Move to the next node in the game tree and update board."
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (svg  (overlay-get ol 'svg))
         (hot-areas (overlay-get ol 'hot-areas))
         (interval (car (overlay-get ol 'board-param)))
         (game-state (overlay-get ol 'game-state))
         (curr-lnode  (aref game-state 0))
         (mvnum      (aref game-state 1))
         (board-2d   (aref game-state 2))
         (next-lnodes (aref curr-lnode 2))
         (n (length next-lnodes))
         (prisoners (aref game-state 4))
         next-lnode play xy x y color new-prisoners)
        (if (null game-state) (error "No game state found."))
        (cond ((= n 0) (message "No next node.") nil)
              (t
               (if (= n 1) (setq branch 0))
               (if (null branch)
                   (setq branch (- (read-char (format "Select a branch (a-%c): " (+ ?a n -1))) ?a)))
               (if (or (< branch 0) (>= branch n))
                   (error "Invalid branch."))
               (setq next-lnode (nth branch next-lnodes))

               (setq mvnum (1+ mvnum))
               (aset game-state 1 mvnum)
               (setq play (sgf-process-play (aref next-lnode 1)))
               (setq color (car play)
                     xy (cdr play)
                     x (car xy)
                     y (cdr xy))
              ;; update game-state: make sure this aset is inplace.
               (aset (aref board-2d y) x color)
               (setq new-prisoners (board-capture xy board-2d))
               (dolist (prison new-prisoners)
                 (svg-remove svg (board-svg-move-number-id (car prison) (cdr prison)))
                 (svg-remove svg (board-svg-stone-id (car prison) (cdr prison))))
               (if (equal color 'B)     ;todo
                   (setcar prisoners (+ (length new-prisoners) (car prisoners)))
                 (setcdr prisoners (+ (length new-prisoners) (cdr prisoners))))
               (aset game-state 0 next-lnode)
               (board-svg-add-stone svg x y (symbol-name color) interval)
               (board-svg-add-move-number svg x y mvnum color interval)
               (overlay-put ol 'display (svg-image svg :map hot-areas))
               (overlay-put ol 'svg svg)
               ))))

(defun sgf-backward-node ()
  "Move to the previous node in the game tree and update board."
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (svg  (overlay-get ol 'svg))
         (hot-areas (overlay-get ol 'hot-areas))
         (game-state (overlay-get ol 'game-state))
         (curr-lnode (aref game-state 0))
         (mvnum     (aref game-state 1))
         (board-2d  (aref game-state 2))
         (prev-lnode (aref curr-lnode 0))
         play xy x y)
    (if (null game-state) (error "No game state found."))
    (if prev-lnode
        (progn (setq mvnum (1- mvnum))
               (aset game-state 1 mvnum)
               (setq play (sgf-process-play (aref curr-lnode 1)))
               (setq xy (cdr play)
                     x (car xy)
                     y (cdr xy))
               ;; update game-state: make sure this aset is inplace.
               (aset (aref board-2d y) x 'E)
               (aset game-state 0 prev-lnode)
               (svg-remove svg (board-svg-stone-id x y))
               (svg-remove svg (board-svg-move-number-id x y))
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
    (if (dom-attr (board-svg-move-numbers-group svg) 'visibility)
        ;; show the numbers
        (dom-remove-attribute (board-svg-move-numbers-group svg) 'visibility)
      ;; add visibility attribute to hide svg move number group
      (dom-set-attribute (board-svg-move-numbers-group svg) 'visibility "hidden"))
    (overlay-put ol 'svg svg)
    (overlay-put ol 'display (svg-image svg :map hot-areas))))

(defun sgf-view-next ()
  "Toggle the display of hint of next move."
  (interactive)
  (let* ((ol (car (overlays-in (point-min) (point-max))))
         (svg  (overlay-get ol 'svg))
         (grid (car (dom-by-id "grid")))
         (hot-areas (overlay-get ol 'hot-areas))
         (interval (nth 0 (overlay-get ol 'board-param)))
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
          ((eq xy-state 'E)
           (let ((n (length next-lnodes))
                 (prisoners 0)
                 (new-lnode (board-linked-node curr-lnode `((B ,xy)))))
             (aset curr-lnode 2 (append next-lnodes (list new-lnode)))
             ;; get to the root node from any node
             (while (aref root-lnode 0) (setq root-lnode (aref root-lnode 0)))
             (erase-buffer)
             (insert (sgf-str-from-game-tree root-lnode))
             (if (= n 0)
                 (sgf-forward-node) ; case 2.1
               (sgf-forward-node n)) ; case 2.2
             (message "Put stone at %d %d" (1+ x) (1+ y))))
          (t (message "Illegal move!"))))) ; case 3.


(defun sgf-pass ()
  "Pass the current. Add a new node of W[] or B[]"
  (interactive)
  )

(defmacro sgf--add-mark (shape add-mark-func)
  "Add a mark to the current node."
  `(let* ((ol (car (overlays-in (point-min) (point-max))))
          (svg  (overlay-get ol 'svg))
          (game-state (overlay-get ol 'game-state))
          (board-2d (aref game-state 1))
          (interval (nth 0 (overlay-get ol 'board-param)))
          (curr-lnode (aref game-state 0))
          (curr-node (aref curr-lnode 1))
          (xy (sgf-mouse-event-to-board-xy last-input-event))
          (x (car xy)) (y (cdr xy))
          (pos-state (aref (aref board-2d y) x)))
     (push curr-node (list shape xy))
     (apply add-mark-func svg interval x y pos-state)))


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
             (interval   (nth 0 (overlay-get ol 'board-param)))
             (margin     (nth 1 (overlay-get ol 'board-param)))
             (bar-height (nth 2 (overlay-get ol 'board-param)))
             (xy (posn-object-x-y (event-start event)))
             (x (/ (- (float (car xy)) margin) interval))
             (y (/ (- (float (cdr xy)) margin bar-height) interval)))
        (cons (round x) (round y)))))


;igo-ui-create-button
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


(defun sgf-process-added-bw (root)
  "Process the AB and AW in root node of an SGF game tree."

  "Process a play node."
  (let* ((color (if (assoc 'B node) 'B 'W))
        (xy (car (alist-get color node))))
    (cons color xy)))

(defun sgf-toggle-board-svg (&optional interval margin bar-height)
  "Visualize the board in the current buffer using SVG."
  (interactive)
  (let ((ol (overlays-in (point-min) (point-max))))
    (if ol (dolist (i ol) (delete-overlay i))
      (let* ((game-tree (sgf-str-to-game-tree (buffer-string)))
             (root (car game-tree))
             (root-lnode (board-linked-node nil root nil))
             (w-h (car (alist-get 'SZ root)))
             (w (car w-h)) (h (cdr w-h))
             (board-2d (board-create-2d w h))
             (interval (or interval board-svg-interval))
             (margin (or margin board-svg-margin))
             (bar-height (or bar-height board-svg-bar-height))
             (svg-hot-areas (board-svg-init w h interval margin bar-height))
             (svg (car svg-hot-areas))
             (hot-areas (cdr svg-hot-areas))
             game-state)
        ;; (setq hot-area-map `(
        ;;                     ((rect . (,hot-area-upper-left . ,hot-area-bottom-right))
        ;;                      board-grid
        ;;                      (help-echo "Click on the intersection" pointer hand))
        ;;                     ))

        ;; process root node
        (dolist (prop root)
          (let* ((prop-key (car prop))
                 (prop-vals (cdr prop))
                 x y stone-color)

            (cond ((or (eq prop-key 'AB) (eq prop-key 'AW))
                   (setq stone-color (intern (substring (symbol-name prop-key) 1)))
                   (dolist (pos prop-vals)
                     (setq x (car pos) y (cdr pos))
                     (aset (aref board-2d y) x stone-color)
                     (board-svg-add-stone svg x y stone-color interval))))))

        (setq ol (make-overlay (point-min) (point-max)))
        ;; root state
        (sgf-link-nodes-in-branch (cdr game-tree) root-lnode)
        (setq game-state
              (board-game-state root-lnode 0 board-2d nil (cons 0 0) nil))
        (overlay-put ol 'game-state  game-state)
        (overlay-put ol 'board-param (list interval margin bar-height))
        (overlay-put ol 'svg svg)
        (overlay-put ol 'hot-areas hot-areas)
        (overlay-put ol 'display (svg-image svg :map hot-areas))))))


;; (sgf-file-to-game-tree "tests/test-15x13.sgf")

;; (sgf-str-to-game-tree "(;FF[4]GM[1]SZ[15:13]AB[bb];B[ba];W[aa]SQ[cb]TR[bb](;B[ab]C[capture white stone.])(;B[ca]))")



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
