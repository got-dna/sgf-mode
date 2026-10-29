;;; katago-test.el --- KataGo tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'katago)

(ert-deftest katago--pos-to-xy-test ()
  (should (equal (katago--pos-to-xy "A1") '(0 . 0)))
  (should (equal (katago--pos-to-xy "J1") '(8 . 0)))
  (should (equal (katago--pos-to-xy "pass") nil)))

(ert-deftest katago-analysis-filter-chunks-test ()
  (with-temp-buffer
    (let* ((proc (make-pipe-process :name "katago-test" :buffer (current-buffer)
                                    :noquery t))
           (responses nil)
           (katago-analysis-callback
            (lambda (turn moves) (push (cons turn moves) responses))))
      (unwind-protect
          (progn
            (katago-analysis-filter proc "{\"turnNumber\":0,\"moveInfos\":[]}")
            (should-not responses)
            (katago-analysis-filter proc "\n{\"turnNum")
            (katago-analysis-filter proc "ber\":1,\"moveInfos\":[]}\n{\"warning\":\"test\"}\n")
            (should (equal (reverse responses) '((0) (1))))
            (should (equal (process-get proc 'katago-pending) "")))
        (delete-process proc)))))

(ert-deftest katago-gtp-process-routing-test ()
  (let ((katago-process nil)
        (katago-analysis-process nil))
    (should-error (katago-gtp-send-cmd "showboard") :type 'user-error)
    (with-temp-buffer
      (let ((analysis (make-pipe-process :name "katago-analysis-test"
                                         :buffer (current-buffer) :noquery t))
            (gtp (make-pipe-process :name "katago-gtp-test"
                                    :buffer (current-buffer) :noquery t)))
        (unwind-protect
            (progn
              (setq katago-process gtp katago-analysis-process analysis)
              (katago-gtp-send-cmd "quit")
              (should-not katago-process)
              (should (process-live-p analysis))
              (katago-gtp-send-cmd "quit")
              (should (process-live-p analysis)))
          (delete-process analysis)
          (delete-process gtp))))))
