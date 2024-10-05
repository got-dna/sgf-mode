;;; sgf-write.el --- serialize and write out game -*- lexical-binding: t -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: go, serialize, sgf, fun

;; This file is not part of GNU Emacs


;;; Commentary:

;;

;;; Code:

;; TODO remove quote for str values and escape [ ] etc
(defun sgf-write-prop-to-str (prop)
  "Convert a property to an SGF string.

(sgf-write-prop-to-str '(B (0 . 0) (1 . 0))) => B[aa:ba]
(sgf-write-prop-to-str '(W)) => W[]
(sgf-write-prop-to-str '(TR (0 . 0))) => TR[aa]
(sgf-write-prop-to-str '(FF 4)) => FF[4]
(sgf-write-prop-to-str '(AB (1 . 1) (1 . 2))) => AB[bb:bc]
(sgf-write-prop-to-str '(SZ (15 . 13))) => SZ[15:13]
(sgf-write-prop-to-str '(LB ((0 . 27) . \"label\"))) => LB[aB:label]
(sgf-write-prop-to-str '(C \"comment\")) => C[comment]"
  (let ((prop-key (car prop))
        (prop-vals (cdr prop))
        prop-val-str)
    (if (null prop-vals)
        ;; if prop-vals is nil (eg a pass move), put [] as the value.
        (setq prop-val-str "[]")
      (setq prop-val-str
            (cond ((memq prop-key '(B W AB AW TR CR MA SQ))
                   (sgf-write-compressed-positions prop-vals))
                  ((eq prop-key 'LB)
                   (mapconcat (lambda (prop-val)
                                (format "[%c%c:%s]"
                                        (sgf-game--number-to-letter (car (car prop-val)))
                                        (sgf-game--number-to-letter (cdr (car prop-val)))
                                        (sgf-write-escape-text (cdr prop-val))))
                                prop-vals))
                  ((eq prop-key 'SZ)
                   (let ((x (caar prop-vals))
                         (y (cdar prop-vals)))
                     (if (= x y) (format "[%d]" x) (format "[%d:%d]" x y))))
                  ((eq prop-key 'C)
                   (format "[%s]" (sgf-write-escape-text (car prop-vals))))
                  (t (format "[%s]" (car prop-vals))))))
    (format "%S%s" prop-key prop-val-str)))


(defun sgf-write-escape-text (text)
  ;; see: https://www.red-bean.com/sgf/sgf4.html#text
  ;; (replace-regexp-in-string "\\([]:\\\\]\\)" "\\\\\\1" text) ;;escape :
  (replace-regexp-in-string "\\([]\\\\]\\)" "\\\\\\1" text))


(defun sgf-write-compressed-positions (xys)
  "Compress a sequence of positions.

For example:
(sgf-write-compressed-positions '((0 . 0) (1 . 0) (3 . 0) (4 . 0) (0 . 1) (1 . 1)))
(sgf-write-compressed-positions '((0 . 0) (1 . 0)))
(sgf-write-compressed-positions '((0 . 0)))"
  (mapconcat
   (lambda (rect)
     (let ((lt (string (sgf-game--number-to-letter (caar rect))
                       (sgf-game--number-to-letter (cdar rect))));;left-top
           (rb (string (sgf-game--number-to-letter (cadr rect))
                       (sgf-game--number-to-letter (cddr rect)))));;right-bottom
       (if (string= lt rb)
           (concat "[" lt "]")
         (concat "[" lt ":" rb "]"))))
   (sgf-write-rows-to-rects (sgf-write-xys-to-rows xys))
   ""))


(defun sgf-write-xys-to-rows (xys)
  "Convert a list of (x . y) coordinates to a list of rectangle blocks.
Each rectangle is represented as ((left . top) . (right . bottom)),
where all positions in the rectangle are filled in coords.

For example:
(sgf-write-xys-to-rows '((1 . 1) (1 . 2)))
(sgf-write-xys-to-rows '((0 . 0) (1 . 0) (3 . 0) (4 . 0) (0 . 1) (1 . 1)))
 => (((0 . 1) . (1 . 1))
     ((3 . 0) . (4 . 0))
     ((0 . 0) . (1 . 0)))"
  ;; Sort the coordinates by y then x"
  (let ((sorted-xys (sort xys :key (lambda (xy) (list (cdr xy) (car xy)))))
        rows)
    (while sorted-xys
      (let* ((start (car sorted-xys))
             (end start)
             (current-line (cdr start)))
        ;; Process continuous x positions on the same line
        (setq sorted-xys (cdr sorted-xys))
        (while (and sorted-xys
                    (= (cdr (car sorted-xys)) current-line)
                    (= (car (car sorted-xys)) (1+ (car end))))
          (setq end (car sorted-xys))
          (setq sorted-xys (cdr sorted-xys)))
        ;; Create a row from the collected positions
        (push (cons start end) rows)))
    rows))

(defun sgf-write-rows-to-rects (rows)
  "Convert a list of rows to a list of rectangle blocks.

For example:
(sgf-write-rows-to-rects '(((1 . 2) . (1 . 2))
                           ((1 . 1) . (1 . 1)))) =>
 (((1 . 1) . (1 . 2)))

(sgf-write-rows-to-rects '(((0 . 1) . (1 . 1))
                           ((3 . 0) . (4 . 0))
                           ((0 . 0) . (1 . 0)))) =>
 (((0 . 0) . (1 . 1))
  ((3 . 0) . (4 . 0)))"
  (let ((sorted-rows (sort rows))
        rects)
    (while sorted-rows
      (let* ((start-row (car sorted-rows))
             (end-row start-row)
             (left-y (cdar start-row))
             (left-x (caar start-row))
             (row-size (- (cadr start-row) left-x)))
        ;; Process continuous y positions
        (setq sorted-rows (cdr sorted-rows))
        (while (and sorted-rows
                    ;; Check if the next row is left aligned
                    (= (caar (car sorted-rows)) left-x)
                    ;; check if the next row is immediately below the current row
                    (= (cdar (car sorted-rows)) (1+ left-y))
                    ;; check if the next row has the same width
                    (= (- (cadr (car sorted-rows)) left-x) row-size))
          (setq end-row (car sorted-rows))
          (setq sorted-rows (cdr sorted-rows)))
        ;; Create a rectangle from the collected rows
        (push (cons (car start-row) (cdr end-row)) rects)))
    (nreverse rects)))


(defun sgf-write-node-to-str (node)
  "Convert a node to an SGF string.

(sgf-write-node-to-str '((FF 4) (SZ (15 . 13)) (AB (1 . 1) (1 . 2)))) => ;FF[4]SZ[15:13]AB[bb][bc]"
  (concat ";" (mapconcat (lambda (prop) (sgf-write-prop-to-str prop)) node)))


(defun sgf-write-game-to-str (&optional lnode)
  "Convert a game tree starting from LNODE to an SGF string."
  ;; (sgf-write-game-to-str (aref (overlay-get (sgf-get-overlay) 'game-state) 0))
  (let ((curr-lnode lnode)
        (next-lnodes (aref lnode 2))
        (node-str (sgf-write-node-to-str (aref lnode 1))))
    (if (null next-lnodes)
        node-str
      (let ((next-strs (mapcar #'sgf-write-game-tree-to-str next-lnodes)))
        (if (= (length next-lnodes) 1)
            ;; No fork, just append the next node string
            (concat node-str (car next-strs))
          ;; Fork, wrap each branch in parentheses
          (concat node-str "(" (mapconcat #'identity next-strs ")(") ")"))))))


(defun sgf-write-game-to-buffer (lnode &optional buffer beg end)
  "Update the buffer region with the SGF string representation of game."
  ;; move to the root node
  (while (setq lnode (aref lnode 0)))
  (let ((sgf-str (sgf-write-game-tree-to-str lnode)))
    (with-current-buffer (or buffer (current-buffer))
      (delete-region (or beg (point-min)) (or end (point-max)))
      (insert "(" sgf-str ")"))))


(provide 'sgf-write)
;;; sgf-write.el ends here
