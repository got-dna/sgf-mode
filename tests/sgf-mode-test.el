;;; sgf-io-test.el --- tests -*- lexical-binding: t -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;;; Code:


(ert-deftest sgf-process-move-test ()
  (should (equal (sgf-process-move '((B (1 . 2)) (C "comment")))
                 '(B 1 . 2)))
  (should (equal (sgf-process-move '((W) (C "comment")))
                 '(W))))


(ert-deftest sgf-neighbors-xy-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (sgf-neighbors-xy '(1 . 1) board-2d)
    (sgf-neighbors-xy nil board-2d)))


(ert-deftest sgf-check-liberty-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (should (equal (car (sgf-check-liberty nil board-2d)) t))
    (should (equal (car (sgf-check-liberty '(1 . 1) board-2d)) t))
    (should (equal (sgf-check-liberty '(0 . 1) board-2d) '(nil (1 . 0) (0 . 0) (0 . 1))))
    (should (equal (sgf-check-liberty '(1 . 0) board-2d) '(nil (0 . 1) (0 . 0) (1 . 0))))))


(ert-deftest sgf-capture-stones-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (should (equal (sgf-capture-stones '(2 . 2) board-2d) 0))
    (should (equal (sgf-capture-stones '(0 . 0) board-2d) 0))
    (should (equal (sgf-capture-stones '(1 . 1) board-2d) 3))
    ;;(should (equal (sgf-capture-stones '(0 . 2) board-2d) 3))
    (should (equal [[E E W]
                    [E W E]
                    [W E E]] board-2d))))



(ert-deftest sgf-capture-stones-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (should-not (sgf-capture-stones nil board-2d))
    (should-not (sgf-capture-stones '(2 . 2) board-2d))
    (should-not (sgf-capture-stones '(0 . 0) board-2d))
    (should (equal (sort (sgf-capture-stones '(1 . 1) board-2d))
                   '((0 . 0) (0 . 1) (1 . 0))))))
    ;;(should (equal (sgf-capture-stones '(0 . 2) board-2d) 3))


(ert-deftest sgf-suicide-p-test ()
  (let ((board-2d [[E B W]
                   [B W E]
                   [W E E]]))
    (should (sgf-suicide-p '(0 . 0) 'W board-2d))
    ;; make sure board-2d is not changed
    (should (equal (sgf-game-board-get '(0 . 0) board-2d) 'E))
    (should (sgf-suicide-p '(0 . 0) 'B board-2d))
    (should-not (sgf-suicide-p '(2 . 2) 'B board-2d))
    (should-not (sgf-suicide-p '(2 . 1) 'B board-2d))
    (should-not (sgf-suicide-p '(1 . 2) 'B board-2d))))


(provide 'sgf-mode-test)
;;; sgf-mode-test.el ends here
