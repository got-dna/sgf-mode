;;; sgf-io-test.el --- tests -*- lexical-binding: t -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;;; Code:

(require 'sgf-mode)

(ert-deftest sgf-board-hoshi-test ()
  "Test for board hoshi"
  (let ((cases '(((9 9) . ((4 . 4)))
                 ((16 16) . ((3 . 3) (12 . 3) (3 . 12) (12 . 12)))
                 ((13 13) . ((6 . 6) (3 . 3)  (9 . 3) (3 . 9) (9 . 9)))
                 ((19 19) . ((9 . 9) (3 . 3)  (15 . 3) (3 . 15) (15 . 15) (3 . 9) (15 . 9) (9 . 3) (9 . 15))))))
    (dolist (i cases)
      (should (equal (apply #'sgf-board-hoshi (car i)) (cdr i))))))


(ert-deftest sgf-process-move-test ()
  (should (equal (sgf-process-move '((B (1 . 2)) (C "comment")))
                 '(B 1 . 2)))
  (should (equal (sgf-process-move '((W) (C "comment")))
                 '(W))))


(ert-deftest sgf-neighbors-xy-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (should (equal (sort (sgf-neighbors-xy '(1 . 1) board-2d))
                   '((0 . 1) (1 . 0) (1 . 2) (2 . 1))))))


(ert-deftest sgf-check-liberty-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    ;(should (equal (car (sgf-check-liberty nil board-2d)) t))
    (should (equal (car (sgf-check-liberty '(1 . 1) board-2d)) t))
    (should (equal (sgf-check-liberty '(0 . 1) board-2d) '(nil (1 . 0) (0 . 0) (0 . 1))))
    (should (equal (sgf-check-liberty '(1 . 0) board-2d) '(nil (0 . 1) (0 . 0) (1 . 0))))))



(ert-deftest sgf-capture-stones-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    ;(should-not (sgf-capture-stones nil board-2d))
    (should-not (sgf-capture-stones '(2 . 2) board-2d))
    (should-not (sgf-capture-stones '(0 . 0) board-2d))
    (should (equal (sort (sgf-capture-stones '(1 . 1) board-2d))
                   '((0 . 0) (0 . 1) (1 . 0))))))


(ert-deftest sgf-suicide-stones-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (should (equal (sort (sgf-suicide-stones '(0 . 0)  board-2d))
                   '((0 . 0) (0 . 1) (1 . 0))))))


(ert-deftest sgf-merge-nodes-test ()
  (let ((node-1 '((B (0 . 0)) (C "abc") (LB ((0 . 2) . "A")) (TR (4 . 16) (4 . 15))))
        (node-2 '((B (0 . 0)) (C "efg") (LB ((0 . 2) . "A") ((0 . 3) . "B")) (TR (4 . 15) (5 . 16))))
        (merged '((B (0 . 0)) (C "abc efg") (LB ((0 . 2) . "A") ((0 . 3) . "B")) (TR (4 . 16) (4 . 15) (5 . 16)))))
    (sgf-merge-nodes node-1 node-2)
    (should (equal (sort node-1)
                   (sort merged)))))


(ert-deftest sgf-merge-branches-test ()
  (let* ((sgf "(;FF[4]GM[1]DT[2024-11-19]SZ[9]PL[B](;B[aa]TR[aa];W[bb];B[ba])(;B[aa]TR[ab];W[ba]))")
         (tree (sgf-parse-str-to-tree sgf))
         (lnode (sgf-tree-to-linked-nodes tree))
         (sgf-exp "(;FF[4]GM[1]DT[2024-11-19]SZ[9]PL[B];B[aa]TR[aa:ab](;W[bb];B[ba])(;W[ba]))"))
    (sgf--merge-branches lnode)
    (should (string=
             sgf-exp
             (replace-regexp-in-string
              "[[:space:]]+" ""
              (sgf-serialize-game-to-str lnode))))))


(provide 'sgf-mode-test)
;;; sgf-mode-test.el ends here
