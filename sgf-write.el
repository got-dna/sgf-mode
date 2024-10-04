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


(defun sgf-write-prop-to-str (prop)
  "Convert a property to an SGF string.

(sgf-write-prop-to-str '(B (0 . 0) (1 . 0))) => B[aa][ba]
(sgf-write-prop-to-str '(W)) => W[]
(sgf-write-prop-to-str '(TR (0 . 0))) => TR[aa]
(sgf-write-prop-to-str '(FF 4)) => FF[4]
(sgf-write-prop-to-str '(AB (1 . 1) (1 . 2))) => AB[bb][bc]
(sgf-write-prop-to-str '(SZ (15 . 13))) => SZ[15:13]
(sgf-write-prop-to-str '(LB (0 . 0) \"label\")) => LB[aa:label]
(sgf-write-prop-to-str '(C \"comment\")) => C[comment]"
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
                               ((equal prop-key 'LB)
                                (format "[%c%c:%s]"
                                        (+ ?a (car (car prop-val)))
                                        (+ ?a (cdr (car prop-val)))
                                        (cdr prop-val)))
                               ((equal prop-key 'SZ)
                                (let ((x (car prop-val))
                                      (y (cdr prop-val)))
                                  (if (= x y) (format "[%d]" x) (format "[%d:%d]" x y))))
                               (t (format "[%s]" prop-val))))
                       prop-val)))
    (format "%S%s" prop-key prop-val-str)))


(defun sgf-write-node-to-str (node)
  "Convert a node to an SGF string.

(sgf-write-node-to-str '((FF 4) (SZ (15 . 13)) (AB (1 . 1) (1 . 2)))) => ;FF[4]SZ[15:13]AB[bb][bc]"
  (concat ";" (mapconcat (lambda (prop) (sgf-write-prop-to-str prop)) node)))


(defun sgf-write-game-tree-to-str (lnode)
  "Convert a game tree starting from LNODE to an SGF string."
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
  (while (aref lnode 0) (setq lnode (aref lnode 0)))
  (let ((sgf-str (sgf-write-game-tree-to-str lnode)))
    (with-current-buffer (or buffer (current-buffer))
      (delete-region (or beg (point-min)) (or end (point-max)))
      (insert "(" sgf-str ")"))))




(provide 'sgf-write)
;;; sgf-write.el ends here
