;;; sgf-io-test.el --- tests -*- lexical-binding: t -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;;; Code:

(ert-deftest string-to-number-or-nil-test ()
  (let ((cases '(("123" . 123)
                 ("123.456" . 123.456)
                 ("+123" . 123)
                 ("-123" . -123)
                 ("123." . 123)
                 ("123.456.789" . nil)
                 ("123a" . nil)
                 ("a123" . nil)
                 ("123 " . nil)
                 (" 123" . nil)
                 (".1"   . 0.1)
                 ("0" . 0)
                 ("0." . 0)
                 ("0.0"  . 0.0)
                 ("+0.0" . 0.0)
                 ("1e2" . nil) ; does not work for scientific notation
                 ("." . nil)
                 (" " . nil)
                 ("" . nil)))
        failed-cases test exp obs)
    (dolist (i cases)
      (setq test (car i))
      (setq exp (cdr i))
      (message "starting test: %s" test)
      (setq obs (string-to-number-or-nil test))
      (condition-case err
          (should (equal exp obs))
        (ert-test-failed
         (push `((case ,test) (expected ,exp) (observed ,obs) (message ,err))
               failed-cases))))
    (when failed-cases
      (ert-fail failed-cases))))

;; (sgf-process-pos "aa:ac")
(ert-deftest sgf-process-pos-test ()
  (let ((cases '(("aa" . "(0 . 0)")
                 ("ab" . "(0 . 1)")
                 ("ba" . "(1 . 0)")
                 ("bb" . "(1 . 1)")
                 ("aa:ac" . "(0 . 0) (0 . 1) (0 . 2)")
                 ("aa:ba" . "(0 . 0) (1 . 0)")))
        pos)
    (dolist (i cases)
      (setq pos (car i))
      (should (equal (cdr i) (sgf-process-pos pos))))))

(ert-deftest sgf-process-LB-test ()
  (let ((cases '(("ee:foo" . (((4 . 4) . "foo")))
                 ("bb:spam" . (((1 . 1) . "spam")))))
        label)
    (dolist (i cases)
      (setq label (car i))
      (should (equal (cdr i) (sgf-process-LB label))))))

(ert-deftest sgf-process-SZ-test ()
  (should (equal (sgf-process-SZ "15") '(15 . 15)))
  (should (equal (sgf-process-SZ "15:13") '(15 . 13))))

(ert-deftest sgf-process-DT-test ()
  (should (equal (sgf-process-DT "2008-12-14") '(0 0 0 14 12 2008 nil nil nil)))
  (should (equal (sgf-process-DT "2008") '(0 0 0 1 1 2008 nil nil nil))))

(ert-deftest sgf-property-value-re-test ()
  "check if the re matches and extracts the a property value in SGF"
  (let ((cases '(("[a a]" . "a a")
                 ("[]" . "")
                 ("[some\\] text]" . "some\\] text")))
        prop-value)
    (dolist (i cases)
      (message "starting test: %s" i)
      (setq prop-value (car i))
      (string-match sgf-property-value-re prop-value)
      (should (string= (cdr i) (match-string 1 prop-value))))))


(ert-deftest sgf-property-re-test ()
  "check if the string exactly matches the a property in SGF"
  (let ((exact-pattern (concat "^" sgf-property-re "$"))
        (match-cases '("B[aa]"           ; a property with single value
                       "AB [cc][d d] [ee]" ; a property with multiple values with arbitrary spaces
                       "C [] "            ; a property with empty value
                       "C [[some\\] text]") ; a property with value with backslash escaped ]
                     )
        (unmatch-cases '(""
                         ";AB[cc][dd]"
                         "C[some text"
                         "C[some text \\]"
                         "C[some] text]")))
    (dolist (i match-cases)
      (message "starting match case: %s" i)
      (should (string-match-p exact-pattern i)))
    (dolist (i unmatch-cases)
      (message "starting unmatch case: %s" i)
      (should-not (string-match-p exact-pattern i)))))


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



(ert-deftest sgf-str-from-node-test ()
  (should (string= (sgf-str-from-node '((B (0 . 0) (1 . 0)) (C "comment")))
                   ";B[aa][ba]C[comment]")))



(ert-deftest sgf-cycle-test ()
  (let* ((cases '("(;B[aa][ba]C[comment])"
                  "(;B[aa][ba]C[comment](;W[cc]C[comment])(;W[ee]))")))
    (dolist (sgf-str cases)
      (setq sgf-lst (sgf-str-to-game-tree sgf-str))
      (setq root (car sgf-lst))
      (setq root-lnode (sgf-linked-node nil root))
      (sgf-linkup-nodes-in-game-tree (cdr sgf-lst) root-lnode)
      (should (string= (format "(%s)" (sgf-str-from-game-tree root-lnode)) sgf-str)))))



(provide 'sgf-io-test)
;;; sgf-mode-test.el ends here
