;;; sgf-graph.el --- visualize game in branch  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: SGF, go, game


;;; Commentary:
;; Visualize the game in branch like this:

;;; Code:

(require 'sgf-util)
(require 'sgf-svg)
(require 'sgf-io)



;; todo:  "The current node will be highlighted in red."
;;;###autoload
(defun sgf-graph-tree (&optional direction bname)
  "Generate an graph to show all game variations in a tree structure.

DIRECTION is the direction of the tree structure. BNAME is the name of
the output buffer."
  (interactive
   (list
    current-prefix-arg ; Handle the direction as a numeric prefix argument
    (read-buffer "Output buffer name: " "*SGF TREE*")))
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (output-buffer (generate-new-buffer bname)))
    (with-current-buffer output-buffer
      (if direction
          (sgf-graph-subtree-h curr-lnode)
        (insert (sgf-graph-subtree-v curr-lnode "*" "")))
      ;; (read-only-mode 1) ; make it read only
      (view-mode 1) ; make it view mode
      (toggle-truncate-lines 1) ; do not wrap long lines
      (goto-char (point-min)))
    (pop-to-buffer output-buffer)))


(defun sgf-graph-pos-to-path ()
  "Generate a path based on the current position in an SGF graph.
Each step in the path corresponds to the column and line traversals
from the current position to the root of the graph.

See also `sgf-traverse' and `sgf-graph-path-to-pos'."
  (interactive)
  (let ((path '())
        (steps (/ (1+ (current-column)) 2))
        column)
    (save-excursion
      (while (> (line-number-at-pos) 1)
        (unless (eq ?* (char-after))
          (push (char-after) path))
        (forward-char -2)
        ;; move to the same column of the previous line.
        (setq column (current-column))
        (forward-line -1)
        (forward-char column)
        (while (eq (char-after) ?|)
          (forward-line -1)
          (forward-char column)))))
    (push steps path))


(defun sgf-graph-path-to-pos (path)
  "Move to the point in the graph buffer for the path.

See also `sgf-traverse' and `sgf-graph-pos-to-path'."
  (interactive "xTraverse path: ")
  (let ((steps (pop path))
        (column 0)
        branch)
    (goto-char (point-min))
    (dotimes (i steps)
        (setq column (+ column 2))
        (forward-line 1)
        (forward-char column)
        (unless (eq (char-after) ?*)
          (setq branch (pop path))
          (while (not (eq branch (char-after)))
              (forward-line)
              (forward-char column))))))


(defun sgf-graph-subtree-v (lnode prefix last-prefix)
  "Recursively generate vertical ASCII tree representation from a doubly-linked node.

The play move comment will be shown. PREFIX is the current indentation
prefix. LAST-PREFIX is the prefix for the last item in a branch.
TODO: is it more efficient to replace string with list for `result'?"
  (let* ((children (aref lnode 2))
         (node (aref lnode 1))
         (comment (alist-get 'C node))
         (comment-suffix (if comment (concat ":" (car comment)) ""))
         (result (format "%s%s\n" prefix comment-suffix)))
    (when children
      (let* ((child-count (length children)))
        (dotimes (i child-count)
          (let* ((is-last (= i (1- child-count)))
                 (child (nth i children))
                 (label (if (= child-count 1) "*" (char-to-string (+ ?a i))))
                 (branch-prefix (concat last-prefix (if is-last "`-" "|-") label))
                 (next-prefix (concat last-prefix (if is-last "  " "| "))))
            (setq result (concat result
                                 (sgf-graph-subtree-v
                                  child
                                  branch-prefix
                                  next-prefix)))))))
    result))

(defun sgf-graph-subtree-h (lnode buffer line-n)
  "Recursively generate ASCII tree representation for SGF subtree.
LNODE is the current node, BUFFER is the target buffer, LINE-INDEX is the current line."
  (if-let ((children (aref lnode 2)))
      (with-current-buffer buffer
        (if (= (point-min) (point-max))
            (insert "*")                ; root
          (goto-char (point-min))
          (message "rest line to forward: %d %d" line-n (forward-line line-n))
          (end-of-line))
        (let* ((prefix (make-string (1- (current-column)) ? ))
               (child-count (length children)))
          (dotimes (i child-count)
            (let* ((is-last (= i (1- child-count)))
                   (is-first (= i 0))
                   (child (nth i children))
                   (label (if (= child-count 1) "*" (char-to-string (+ ?a i)))))
              (cond
               (is-first (insert (concat "-" label)))
               (is-last (insert (concat "\n" prefix "`-" label)))
               (t (insert (concat "\n" prefix "|-" label))))
              (sgf-graph-subtree-h child buffer (1+ line-n)))
              )))))

(defun transform-string (str size)
  "Transform STR by keeping '|', replacing '`' with '|', and converting all
other characters to spaces."
  (let ((result (make-string size ?\s))) ;; Initialize a string of spaces
    (dotimes (i (min size (length str)) result)
      (let ((char (aref str i)))
        (aset result i
              (if (or (eq char ?|)
                      (eq char ?\`))
                  ?|
                ?\s))))))

(defun sgf-graph-subtree-h (root-lnode)
  "Generate ASCII tree representation for SGF subtree non-recursively.
ROOT-NODE is the root node, BUFFER is the target buffer, LINE-N is the starting line."
  (goto-char (point-min))
  (let ((stack (list (list root-lnode 1))))
    (insert "*") ; root
    (while stack
      (let* ((current (pop stack))
             (lnode (car current))
             (line-n (cadr current))
             (children (aref lnode 2))
             (child-count (length children)))
        ;; move to the end of line line-n
        (goto-char 0)
        (end-of-line line-n)
        ;; Get the next line and transform it
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position 2)
                      (line-end-position 2)))
               (prefix (transform-string
                        line
                        (- (current-column) 1))))
          (message "  line: %s\nprefix: %s" line prefix)
          (dotimes (i child-count)
            (let* ((is-last (= i (1- child-count)))
                   (is-first (= i 0))
                   (child (nth i children)))
              (insert (if is-first
                          "-"
                        (concat "\n" prefix (if is-last "`-" "|-"))))
              (insert (if (= child-count 1) "*" (char-to-string (+ ?a i))))
              (push (list child (+ i line-n)) stack))))))))


(provide 'sgf-graph)
