(ert-deftest katago--pos-to-xy-test ()
  (should (equal (katago--pos-to-xy "A1") '(0 . 0)))
  (should (equal (katago--pos-to-xy "J1") '(8 . 0)))
  (should (equal (katago--pos-to-xy "pass") nil)))
