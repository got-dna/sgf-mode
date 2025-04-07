(require 'sgf-graph)
(require 'ert)


(defun create-test-lnode ()
  "Create a test lnode for testing."
  (let* ((d1 (vector nil '((C "d1")) nil))
         (d2 (vector nil '((C "d2")) nil))
         (d3 (vector nil '((C "d3")) nil))
         (c1 (vector nil '((C "c1")) (list d1 d2 d3)))
         (c2 (vector nil '() nil))
         (b (vector nil '((C "b")) (list c1 c2)))
         (a (vector nil '() (list b))))
    (aset b 0 a)
    (aset c2 0 b)
    (aset c1 0 b)
    (aset d1 0 c1)
    (aset d2 0 c1)
    (aset d3 0 c1)
    a))


(ert-deftest sgf-graph-subtree-v-test-simple ()
  (let* ((lnode (create-test-lnode))
         (obs (with-temp-buffer
                (sgf-graph-subtree-v lnode)
                (buffer-string)))
         (exp (mapconcat 'identity
                         '("*"
                           "`-*:b"
                           "  |-a:c1"
                           "  | |-a:d1"
                           "  | |-b:d2"
                           "  | `-c:d3"
                           "  `-b"
                           "")
                         "\n")))
    (should (equal obs exp))))


(ert-deftest sgf-graph-subtree-h-test-simple ()
  (let* ((lnode (create-test-lnode))
         (obs (with-temp-buffer
                (sgf-graph-subtree-h lnode)
                (buffer-string)))
         (exp (mapconcat 'identity
                         '("*-o-a-a"
                           "  | |-b"
                           "  | `-c"
                           "  `-b"
                           "")
                         "\n")))
  (should (equal obs exp))))


(ert-deftest sgf-graph-tree-test ()
  (require 'project)
  ;; TODO any better alternatives to get tests dir?
  (let* ((root-dir (project-root (project-current)))
         (test-dir "tests")
         (data-filename "test-13x15.sgf")
         (data-filepath (file-name-concat root-dir test-dir data-filename))
         (game-state (sgf-parse-file-to-* data-filepath 'sgf-parse-buffer-to-game-state))
         (lnode (aref game-state 0))
         (obs-h (with-temp-buffer
                  (sgf-graph-subtree-h lnode)
                  (buffer-string)))
         (exp-h (mapconcat 'identity
                           '("*-o-x-a-x-o-x"
                             "    `-b"
                             "")
                           "\n"))
         (obs-v (with-temp-buffer
                  (sgf-graph-subtree-v lnode)
                  (buffer-string)))
         (exp-v (mapconcat 'identity
                           '("*"
                             "`-*"
                             "  `-*"
                             "    |-a:comment@[foo]"
                             "    | `-*"
                             "    |   `-*"
                             "    |     `-*"
                             "    `-b"
                             "")
                           "\n")))
    (add-text-properties 6 7 '(face (sgf-graph-comment-node sgf-graph-bm-node) help-echo "comment@[foo]") exp-h)
    (add-text-properties 2 3   '(face sgf-graph-te-node) exp-h)
    (add-text-properties 4 5   '(face sgf-graph-bm-node) exp-h)
    (add-text-properties 8 9   '(face sgf-graph-te-node) exp-h)
    (add-text-properties 10 11 '(face sgf-graph-do-node) exp-h)
    (add-text-properties 12 13 '(face sgf-graph-it-node) exp-h)
    (should (equal-including-properties obs-h exp-h))
    (should (equal obs-v exp-v))))
