;;; sgf-game-test.el --- test for sgf-game.el -*- lexical-binding: t -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;;; Code:


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



(ert-deftest sgf-cycle-test ()
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
