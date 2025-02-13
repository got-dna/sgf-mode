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
