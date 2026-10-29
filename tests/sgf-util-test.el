;;; sgf-util-test.el --- tests -*- lexical-binding: t -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;;; Code:
(require 'sgf-util)

(ert-deftest sgf-merge-alist-test ()
  (should (equal (sgf-merge-alist '((AB (5 . 16)) (AW (0 . 17)) (AB (5 . 17) (5 . 16))))
                 '((AB (5 . 16) (5 . 17)) (AW (0 . 17)))))

  (should (equal (sgf-merge-alist '((AB (5 . 16)) (AW (0 . 17))))
                 '((AB (5 . 16)) (AW (0 . 17)))))

  (should (equal (sgf-merge-alist '((AB 1) (AB 2 3) (AW 0) (AB)))
                 '((AB 1 2 3) (AW 0)))))

(ert-deftest sgf-board-clear-rectangular-test ()
  (dolist (size '((3 . 2) (2 . 3)))
    (let ((board (sgf-board-create (car size) (cdr size) 'B)))
      (sgf-board-clear board)
      (should (equal board (sgf-board-create (car size) (cdr size) 'E))))))

(ert-deftest sgf-tree-accessors-test ()
  (let* ((root (vector nil nil nil))
         (child (vector root nil nil))
         (leaf (vector child nil nil)))
    (aset root 2 (list child))
    (should (eq (sgf-get-root leaf) root))
    (should (eq (sgf-get-root root) root))
    (should-not (sgf-get-siblings root))
    (should (equal (sgf-get-siblings child) (list child)))))
