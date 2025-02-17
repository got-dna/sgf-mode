;;; sgf-io-test.el --- tests -*- lexical-binding: t -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;;; Code:
(require 'sgf-io)
(require 'sgf-mode)


(ert-deftest sgf-encode-node-test ()
  (should (equal (sgf-encode-node '((FF 4) (SZ (15 . 13)) (AB (1 . 1) (1 . 2))))
                 ";FF[4]SZ[15:13]AB[bb:bc]")))


(ert-deftest sgf-decode-prop-pos-test ()
  (let ((cases '(("aa" . ((0 . 0)))
                 ("aa:ac" . ((0 . 0) (0 . 1) (0 . 2)))
                 ("aa:ca" . ((0 . 0) (1 . 0) (2 . 0)))
                 ("aa:cc" . ((0 . 0) (0 . 1) (0 . 2)
                             (1 . 0) (1 . 1) (1 . 2)
                             (2 . 0) (2 . 1) (2 . 2))))))

    (dolist (i cases)
      (should (equal (sgf-decode-prop-pos (car i)) (cdr i))))))


(ert-deftest sgf-encode-prop-pos-test ()
  (should (equal (sgf-encode-prop-pos '())
                 ""))
  (should (equal (sgf-encode-prop-pos '((0 . 0) (1 . 0) (3 . 0) (4 . 0) (0 . 1) (1 . 1)))
                 "[aa:bb][da:ea]"))
  (should (equal (sgf-encode-prop-pos '((0 . 0) (1 . 0)))
                 "[aa:ba]"))
  (should (equal (sgf-encode-prop-pos '((0 . 0)))
                 "[aa]")))


(ert-deftest sgf-io-xys-to-rows ()
  (should (equal (sgf-io-xys-to-rows '()) nil))
  (should (equal (sgf-io-rows-to-rects
                  (sgf-io-xys-to-rows
                   '((15 . 5) (15 . 6) (15 . 7) (15 . 8) (15 . 9) (15 . 10))))
                 '(((15 . 5) . (15 . 10)))))
  (should (equal (sgf-io-xys-to-rows '((1 . 1)))
                 '(((1 . 1) . (1 . 1)))))
  (should (equal (sgf-io-xys-to-rows '((0 . 0) (1 . 0) (3 . 0) (4 . 0) (0 . 1) (1 . 1)))
                 '(((0 . 1) . (1 . 1))
                   ((3 . 0) . (4 . 0))
                   ((0 . 0) . (1 . 0))))))


(ert-deftest sgf-io-rows-to-rects ()
  (should (equal (sgf-io-rows-to-rects '()) nil))
  (should (equal (sgf-io-rows-to-rects '(((1 . 2) . (1 . 2))
                                         ((1 . 1) . (1 . 1))))
                 '(((1 . 1) . (1 . 2)))))
  (should (equal (sgf-io-rows-to-rects '(((0 . 1) . (1 . 1))
                                         ((3 . 0) . (4 . 0))
                                         ((0 . 0) . (1 . 0))))
                 '(((0 . 0) . (1 . 1))
                   ((3 . 0) . (4 . 0))))))


(ert-deftest sgf-decode-prop-LB-test ()
  (let ((cases '(("ee:foo" . ((4 . 4) . "foo"))
                 ("bb:spam:a" . ((1 . 1) . "spam:a"))))
        label)
    (dolist (i cases)
      (setq label (car i))
      (should (equal (cdr i) (sgf-decode-prop-LB label))))))


(ert-deftest sgf-encode-prop-test ()
  "Test the function `sgf-encode-prop` with various inputs."
  (should (equal (sgf-encode-prop '(B (0 . 0) (1 . 0))) "B[aa:ba]"))
  (should (equal (sgf-encode-prop '(W)) "W[]"))
  (should (equal (sgf-encode-prop '(TR (0 . 0))) "TR[aa]"))
  (should (equal (sgf-encode-prop '(FF 4)) "FF[4]"))
  (should (equal (sgf-encode-prop '(AB (1 . 1) (1 . 2))) "AB[bb:bc]"))
  (should (equal (sgf-encode-prop '(SZ (15 . 13))) "SZ[15:13]"))
  (should (equal (sgf-encode-prop '(LB ((0 . 27) . "label"))) "LB[aB:label]"))
  (should (equal (sgf-encode-prop '(C "comment")) "C[comment]"))
  (should (equal (sgf-encode-prop '(C "com\"ment")) "C[com\"ment]"))
  (should (equal (sgf-encode-prop '(C "comment@\[foo\]")) "C[comment@[foo\\]]")))


(ert-deftest sgf-parse-syntex-tree-test ()
  (require 'project)
  ;; TODO any better alternatives to get tests dir?
  (let* ((root-dir (project-root (project-current)))
         (test-dir "tests")
         (data-filename "test-13x15.sgf")
         (data-filepath (file-name-concat root-dir test-dir data-filename))
         (obs (sgf-parse-file-to-* data-filepath 'sgf-parse-buffer-to-syntax-tree))
         (exp [1
               ([2
                 ([3 "FF" ([5 "4" 8]) 8] [8 "GM" ([10 "1" 13]) 13]
                  [13 "SZ" ([15 "13:15" 22]) 22]
                  [22 "AB" ([24 "ca" 28] [28 "bb" 32] [32 "db:eb" 39] [39 "cc" 43])
                      43]
                  [43 "AW" ([45 "da:ea" 52] [52 "fb" 56] [56 "dc:ec" 63]) 63]
                  [63 "PL" ([65 "W" 68]) 68] [68 "CA" ([70 "UTF-8" 77]) 77]
                  [77 "AP" ([79 "Sabaki:0.52.2" 94]) 94]
                  [94 "KM" ([96 "6.5" 101]) 101]
                  [101 "DT" ([103 "2024-09-19" 115]) 116])
                 116]
                [116 ([117 "W" ([118 "ce" 122]) 122] [122 "TE" ([124 "1" 127]) 127])
                     127]
                [127 ([128 "B" ([129 "be" 133]) 133] [133 "BM" ([135 "1" 138]) 139])
                     139]
                ([139
                  ([141
                    ([143 "W" ([145 "cb" 151]) 151] [151 "BM" ([153 "1" 156]) 157]
                     [157 "SQ" ([159 "bb" 163]) 163]
                     [163 "TR" ([165 "db" 169] [169 "eb" 173]) 173]
                     [173 "LB" ([175 "fb:A" 181]) 181]
                     [181 "C" ([182 "comment@\\[foo\\]" 199]) 199]
                     [199 "MN" ([201 "15" 205]) 207])
                    207]
                   [207
                    ([208 "B" ([209 "db" 213]) 213] [213 "TE" ([215 "1" 218]) 220])
                    220]
                   [220
                    ([221 "W" ([222 "eb" 226]) 226] [226 "DO" ([228 "1" 231]) 233])
                    233]
                   [233
                    ([234 "B" ([235 "cb" 239]) 239] [239 "IT" ([241 "1" 244]) 245])
                    245])
                  246]
                 [247 ([248 ([249 "W" ([250 "ba" 254]) 254]) 254]) 255]))
               258]
              ))
    (should (equal obs exp))))


(ert-deftest sgf-io-cycle-test ()
  "Test `sgf-str-to-game-tree' and `sgf-str-from-game-tree'."
  (let* ((cases '("(;FF[4]GM[1]SZ[3:2])"
                  "(;FF[4]GM[1]SZ[3:2];B[aa];W[ba])"
                  "(;FF[4]GM[1]SZ[9];B[ba]C[comment](;W[cc];B[ab])(;W[ee]))"
                 "(;FF[4]GM[1]SZ[13:15]AB[bb][ca][cc][db:eb]AW[da:ea][dc:ec][fb]PL[W]CA[UTF-8]AP[Sabaki:0.52.2]KM[6.5]DT[2024-09-19];W[ce];B[be](;W[cb]SQ[bb]TR[db:eb]LB[fb:A]C[comment@[foo\\]]MN[15];B[db];W[eb];B[cb])(;W[ba]))"))
         game-state)
    (dolist (sgf-str cases)
      (with-temp-buffer
        (erase-buffer)
        (insert sgf-str)
        (setq game-state (sgf-parse-buffer-to-game-state (point-min) (point-max)))
        (should (string= sgf-str
                         (replace-regexp-in-string
                          "[[:space:]]" "" ; remove spaces in the serialized str
                          (sgf-serialize-game-to-str (aref game-state 0)))))))))


(ert-deftest sgf-serialize-lnode-to-json-test ()
  (require 'project)
  ;; TODO any better alternatives to get tests dir?
  (let* ((root-dir (project-root (project-current)))
         (test-dir "tests")
         (fn "test-13x15")
         (fp (file-name-concat root-dir test-dir fn))
         (sgf-fp (concat fp ".sgf"))
         (json-fp (concat fp ".json"))
         (exp (json-read-file json-fp))
         (root-lnode (sgf-parse-file-to-* sgf-fp
                                          'sgf-parse-buffer-to-linked-node))
         (curr-lnode root-lnode))
    (while (aref curr-lnode 2)
      (setq curr-lnode (car (aref curr-lnode 2))))
    ;; (obs (json-parse-string obs-str :object-type 'alist))
    (should (equal
             (sgf-serialize-lnode-to-json curr-lnode) exp))))
