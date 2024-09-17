;;; sgf-io.el --- read and write SGF  -*- lexical-binding: t; -*-



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
      )
  "Regular expression to match a single SGF property value.")


;; use `regexp' to dynamic include other regexp patterns
(defvar sgf-property-re
  (rx (group (one-or-more alpha))        ; Property name: one or more alphabetic characters
      (regexp sgf-space-re)              ; Optional spaces between name and values
      (one-or-more                       ; Property values: one or more occurrences
       (regexp sgf-property-value-re)   ; Match a single value
       (regexp sgf-space-re)))            ; Optional spaces between values

  "Regular expression to match a single SGF property and its value(s), allowing whitespace(s) in between.")


(defvar sgf-node-re
  (rx (group ";") ; this group is not necessary; only for the font lock.
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


(defun sgf-convert-pos (pos-str)
  "(sgf-convert-pos \"aa\") => \"(0 . 0)\""
  ;; aa:ac like str is odd in length
  (if (string-empty-p pos-str)
      "" ; this is a pass. position is empty
    (if (= (logand (seq-length pos-str) 1) 1)
        (mapconcat (lambda (i) (format "%S" i)) (sgf-convert-multi-positions pos-str) " ")
      (format "%S" (sgf-convert-single-position pos-str)))))

(defun sgf-convert-single-position (pos-str)
  "for property AB, AW, AE."
  (let ((pos-str (downcase pos-str))
        (x-char (aref pos-str 0))
        (y-char (aref pos-str 1)))
    (cons (- x-char ?a) (- y-char ?a))))

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
  "Convert and replace the content in SGF-BUFFER to an elisp list.

If SGF-BUFFER is nil, use the current buffer. The buffer content should be SGF strings."
  (let (beg-prop beg-prop-val node prop prop-key prop-val)
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
            (while (string-match sgf-property-value-re prop beg-prop-val)
              (setq beg-prop-val (match-end 0))
              (setq prop-val (match-string 1 prop))
              (insert " ")
              (cond ((member prop-key '("B" "W" "AB" "AW" "AE" "MA" "SQ" "TR" "CR"))
                     (setq prop-val (sgf-convert-pos prop-val)))
                    ;; ((string= prop-key "DT")
                    ;;  (setq prop-val (parse-time-string prop-val))
                    ((string= prop-key "SZ")
                     (setq prop-val (sgf-convert-SZ prop-val)))
                    ((string= prop-key "LB")
                     (setq prop-val (sgf-convert-LB prop-val)))
                    (t                 ;; try convert to a number
                     (setq prop-val (or (string-to-number-or-nil prop-val)
                                        (format "%S" prop-val)))))
              (insert (format "%s" prop-val)))
            (insert ")")
            (setq beg-prop-val 0))
          (insert ")")
          (setq beg-prop 0))))))

;; (sgf-file-to-game-tree "tests/test-15x13.sgf")
;; (sgf-str-to-game-tree "(;FF[4]GM[1]SZ[15:13]AB[bb];B[ba];W[aa]SQ[cb]TR[bb](;B[ab]C[capture white stone.])(;B[ca]))")



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
(sgf-str-from-prop '(W)) => W[]
(sgf-str-from-prop '(TR (0 . 0))) => TR[aa]
(sgf-str-from-prop '(FF 4)) => FF[4]
(sgf-str-from-prop '(AB (1 . 1) (1 . 2))) => AB[bb][bc]
(sgf-str-from-prop '(SZ (15 . 13))) => SZ[15:13]
(sgf-str-from-prop '(C \"comment\")) => C[comment]"
  (let ((prop-key (car prop))
        (prop-val (cdr prop))
        prop-val-str)
    (if (null prop-val)
        ;; if prop-val is nil, put [] as the value.
        (setq prop-val-str "[]")
      (setq prop-val-str
            (mapconcat (lambda (prop-val)
                         (cond ((member prop-key '(B W AB AW TR CR MA SQ))
                                (format "[%c%c]"
                                        (+ ?a (car prop-val))
                                        (+ ?a (cdr prop-val))))
                               ((eq prop-key 'SZ)
                                (let ((x (car prop-val))
                                      (y (cdr prop-val)))
                                  (if (= x y) (format "[%d]" x) (format "[%d:%d]" x y))))
                               (t (format "[%s]" prop-val))))
                       prop-val)))
    (format "%S%s" prop-key prop-val-str)))


(defun sgf-str-from-node (node)
  "Convert a node to an SGF string.

(sgf-str-from-node '((FF 4) (SZ (15 . 13)) (AB (1 . 1) (1 . 2)))) => ;FF[4]SZ[15:13]AB[bb][bc]"
  (concat ";" (mapconcat (lambda (prop) (sgf-str-from-prop prop)) node)))


(defun sgf-str-from-game-tree (lnode)
  "Convert a game tree starting from LNODE to an SGF string."
  (let ((curr-lnode lnode)
        (next-lnodes (aref lnode 2))
        (node-str (sgf-str-from-node (aref lnode 1))))
    (if (null next-lnodes)
        node-str
      (let ((next-strs (mapcar #'sgf-str-from-game-tree next-lnodes)))
        (if (= (length next-lnodes) 1)
            ;; No fork, just append the next node string
            (concat node-str (car next-strs))
          ;; Fork, wrap each branch in parentheses
          (concat node-str "(" (mapconcat #'identity next-strs ")(") ")"))))))


(defun sgf-update-buffer-from-game (lnode &optional beg end)
  "Update the current buffer region with the SGF string representation of game."

  ;; move to the root node
  (while (aref lnode 0) (setq lnode (aref lnode 0)))
  (let ((sgf-str (sgf-str-from-game-tree lnode)))
    (delete-region (or beg (point-min)) (or end (point-max)))
    (insert sgf-str)))


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

(provide 'sgf-io)
;;; sgf-io.el ends here
