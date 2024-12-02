;;; sgf-graph.el --- visualize game in branch  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: SGF, go, game


;;; Commentary:
;; Visualize the game in branch like this:
;; *
;; |
;; *
;; |\
;; * s
;; | |\
;; * * *
;;     |
;;     *

;;; Code:

(require 'sgf-util)
(require 'sgf-svg)
(require 'sgf-io)


(defun sgf-graph-branches ()
  "The current node will be highlighted in red."
  (interactive))

(defun sgf-graph-tree ()
  "Generate an ASCII representation of the tree from a doubly-linked node."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (output-buffer (generate-new-buffer "*SGF TREE*")))
    (with-current-buffer output-buffer
      (insert (sgf-graph-subtree curr-lnode "" ""))
      (read-only-mode 1) ; make it read only
      (toggle-truncate-lines 1) ; do not wrap long lines
      (goto-char (point-min)))
    (pop-to-buffer output-buffer)
    output-buffer))

(defun sgf-graph-subtree (lnode prefix last-prefix)
  "Recursively generate ASCII tree representation.

PREFIX is the current indentation prefix.
LAST-PREFIX is the prefix for the last item in a branch."
  (let* ((node (aref lnode 1))
         (children (aref lnode 2))
         (comment (alist-get 'C node))
         (comment-suffix (if comment (concat ":" (car comment)) ""))
         (result (format "%s%s\n" prefix comment-suffix)))
    (when children
      (let ((child-count (length children)))
        (dolist (child-index (number-sequence 0 (1- child-count)))
          (let* ((is-last (= child-index (1- child-count)))
                 (child (nth child-index children))
                 (branch-prefix
                  (concat
                   last-prefix
                   (if is-last "`" "|")
                   "-"
                   (if (> child-count 1)
                       (format "%c" (+ ?a child-index))
                     "*")))
                 (next-prefix
                  (concat
                   last-prefix
                   (if is-last "  " "| "))))
            (setq result
                  (concat result
                          (sgf-graph-subtree
                           child
                           branch-prefix
                           next-prefix)))))))
    result))

;; Example usage
(defun test-ascii-graph ()
  "Demonstrate the ASCII graph generation."
  (interactive)
  (let* ((d-node (vector nil "ddaf" nil))
         (c-node (vector nil "caa" (list d-node)))
         (c1-node (vector nil "clda" nil))
         (b-node (vector nil "bspam" (list c1-node c-node)))
         (a-node (vector nil "afoo" (list b-node))))
    (aset b-node 0 a-node)
    (aset c-node 0 b-node)
    (aset c1-node 0 b-node)
    (aset d-node 0 c-node)
    (sgf-graph-tree a-node)))
