;;; sgf-game-test.el --- test for sgf-game.el -*- lexical-binding: t -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;;; Code:

(ert-deftest sgf-game-board-hoshi-test ()
  "Test for board hoshi"
  (let ((cases '(((9 9) . ((4 . 4)))
                 ((16 16) . ((3 . 3) (12 . 3) (3 . 12) (12 . 12)))
                 ((13 13) . ((6 . 6) (3 . 3)  (9 . 3) (3 . 9) (9 . 9)))
                 ((19 19) . ((9 . 9) (3 . 3)  (15 . 3) (3 . 15) (15 . 15) (3 . 9) (15 . 9) (9 . 3) (9 . 15))))))
    (dolist (i cases)
      (should (equal (apply #'sgf-game-board-hoshi (car i)) (cdr i))))))


(ert-deftest sgf-game-prop-position-test ()
  (let ((cases '(("aa" . ((0 . 0)))
                 ("aa:ac" . ((0 . 0) (0 . 1) (0 . 2)))
                 ("aa:ca" . ((0 . 0) (1 . 0) (2 . 0)))
                 ("aa:cc" . ((0 . 0) (0 . 1) (0 . 2)
                             (1 . 0) (1 . 1) (1 . 2)
                             (2 . 0) (2 . 1) (2 . 2))))))

    (dolist (i cases)
      (should (equal (sgf-game-prop-position (car i)) (cdr i))))))


(ert-deftest sgf-game-prop-LB-test ()
  (let ((cases '(("ee:foo" . ((4 . 4) . "foo"))
                 ("bb:spam:a" . ((1 . 1) . "spam:a"))))
        label)
    (dolist (i cases)
      (setq label (car i))
      (should (equal (cdr i) (sgf-game-prop-LB label))))))


(ert-deftest sgf-buffer-to-game-tree-test ()
  (let ((cases
         '(("(;GM[1]FF[4]
               SZ[19]
               DT[2008-12-14]
               KM[0.0]AW[ja]
               [pa];LB[ae:x];B[ee]C[\"Black 25 takes larger territory on top\", as compared to variation (a) \\[3-1\\]]
               (;W[cb];B[ed]))"  ;; weired comment str; nested tree
            .
            "(((:GM 1)(:FF 4)(:SZ 19)(:DT (nil nil nil 14 12 2008 nil -1 nil))(:KM 0.0)(:AW \"ja\" \"pa\"))((:LB ((:label . \"x\") (:pos 0 . 4))))((:B (:pos 4 . 4))(:C \"\\\"Black 25 takes larger territory on top\\\", as compared to variation (a) \\\\[3-1\\\\]\"))(((:W (:pos 2 . 1)))((:B (:pos 4 . 3)))))"))))
            ;"(((:GM . 1) (:FF . 4) (:SZ . 19) (:DT NIL NIL NIL 14 12 2008 NIL NIL NIL) (:KM . 0.0) (:AW \"ja\" \"pa\")) ((:B :pos 4 . 4) (:C . \"a \\\"quote\\\"\")))"
    (dolist (i cases)
      (with-temp-buffer
        (insert (car i))
        (sgf-buffer-to-game-tree)
        (should (string= (cdr i) (buffer-string)))))))



(ert-deftest sgf-game-cycle-test ()
  "Test `sgf-str-to-game-tree' and `sgf-str-from-game-tree'."
  (let* ((cases '("(;FF[4]GM[1]SZ[3:2];B[aa];W[ba])"
                  "(;B[aa][ba]C[comment])"
                  "(;B[aa][ba]C[comment](;W[cc]C[comment])(;W[ee]))")))
    (dolist (sgf-str cases)
      (setq sgf-lst (sgf-str-to-game-tree sgf-str))
      (setq root (car sgf-lst))
      (setq root-lnode (sgf-linked-node nil root))
      (sgf-linkup-nodes-in-game-tree (cdr sgf-lst) root-lnode)
      (should (string= (format "(%s)" (sgf-str-from-game-tree root-lnode)) sgf-str)))))



(provide 'sgf-game-test)
;;; sgf-game-test.el ends here
