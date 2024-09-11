(ert-deftest board-hoshi-test ()
  "Test for board-hoshi"
  (let ((cases '(((9 9) . ((4 . 4)))
                 ((16 16) . ((3 . 3) (12 . 3) (3 . 12) (12 . 12)))
                 ((13 13) . ((6 . 6) (3 . 3)  (9 . 3) (3 . 9) (9 . 9)))
                 ((19 19) . ((9 . 9) (3 . 3)  (15 . 3) (3 . 15) (15 . 15) (3 . 9) (15 . 9) (9 . 3) (9 . 15))))))
    (dolist (i cases)
      (should (equal (apply #'board-hoshi (car i)) (cdr i))))))


(ert-deftest board-pos-neighbors-test ()
  "Test for board-pos-neighbors"
  (let ((cases '(((0   19 19) . (nil 1   nil 19))
                 ((1   19 19) . (0   2   nil 20))
                 ((18  19 19) . (17  nil nil 37))
                 ((180 19 19) . (179 181 161 199))
                 ((360 19 19) . (359 nil 341 nil))
                 ((2   3  3)  . (1   nil nil 5)))))
    (dolist (i cases)
      (should (equal (apply #'board-pos-neighbors (car i)) (cdr i))))))


(ert-deftest board-neighbor-pos-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (board-neighbor-pos '(1 . 1) board-2d)))


(ert-deftest board-alive-p-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (should (equal (car (board-alive-p '(1 . 1) board-2d)) t))
    (should (equal (board-alive-p '(0 . 1) board-2d) '(nil (1 . 0) (0 . 0) (0 . 1))))
    (should (equal (board-alive-p '(1 . 0) board-2d) '(nil (0 . 1) (0 . 0) (1 . 0))))))


(ert-deftest board-capture-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (should (equal (board-capture '(2 . 2) board-2d) 0))
    (should (equal (board-capture '(0 . 0) board-2d) 0))
    (should (equal (board-capture '(1 . 1) board-2d) 3))
    ;;(should (equal (board-capture '(0 . 2) board-2d) 3))
    (should (equal [[E E W]
                    [E W E]
                    [W E E]] board-2d))))



(ert-deftest board-capture-test ()
  (let ((board-2d [[B B W]
                   [B W E]
                   [W E E]]))
    (should-not (board-capture '(2 . 2) board-2d))
    (should-not (board-capture '(0 . 0) board-2d))
    (should (equal (sort (board-capture '(1 . 1) board-2d))
                   '((0 . 0) (0 . 1) (1 . 0))))))
    ;;(should (equal (board-capture '(0 . 2) board-2d) 3))
