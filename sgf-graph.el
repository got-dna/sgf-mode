;;; sgf-graph.el --- visualize game in tree graph  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: SGF, go, game


;;; Commentary:


;;; Code:

(require 'sgf-util)
(require 'sgf-svg)
(require 'sgf-io)


(defface sgf-graph-current-node
  '((t :foreground "magenta" :weight bold))
  "Face for the current node in the graph tree."
  :group 'sgf-graph)

(defface sgf-graph-comment-node
  '((t :inherit link))
  "Face for the comment node in the graph tree."
  :group 'sgf-graph)


;;;###autoload
(defun sgf-graph-tree (&optional vertical bname ov)
  "Generate an graph to show all game variations in a tree structure.

The prefix argument VERTICAL specifies the direction of the tree
structure. By default, the tree is graphed in horizontal direction. If
prefix argument is provided or VERTICAL is t, the tree will be graphed
in vertical direction. BNAME is the name of the output buffer."
  (interactive "P")
  (let* ((ov (or ov (sgf-get-overlay)))
         (lnode (sgf-get-lnode ov))
         (path (sgf-lnode-path lnode))
         ;; get the existing graph buffer or create a new one
         (graph-buffer (overlay-get ov 'graph-buffer)))
    (unless (buffer-live-p graph-buffer)
      (setq graph-buffer
            (generate-new-buffer
             (or bname
                 (read-buffer "Output buffer name: "
                              (if vertical "*SGF TREE V*" "*SGF TREE H*"))))))
    ;; display the graph buffer before `recenter'
    (display-buffer graph-buffer)
    ;; put the graph buffer in the overlay
    (overlay-put ov 'graph-buffer graph-buffer)
    ;; move to the root-lnode
    (while (aref lnode 0) (setq lnode (aref lnode 0)))
    (with-current-buffer graph-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if vertical
            (sgf-graph-subtree-v lnode)
          (sgf-graph-subtree-h lnode))
        (setq truncate-lines t) ; do not wrap long lines
        ;; it seems `recenter' only works in the active window
        (with-selected-window (get-buffer-window graph-buffer)
          ;; highlight the current node of the game
          (sgf-graph-path-to-pos vertical path)
          (add-face-text-property (1- (point)) (point) 'sgf-graph-current-node)
          (recenter -1)))
      (sgf-graph-mode)
      ;; enable cursor intangible mode to allow cursor to move only on nodes
      (cursor-intangible-mode 1)
      ;; define and set local variables:
      ;; 1. the game that the graph tree buffer is associated.
      ;; 2. the direction of the graph
      ;; This has to be done after the mode is enabled - major mode enabling
      ;; kills all local variables.
      (setq-local sgf-graph-which-game ov)
      (setq-local sgf-graph-vertical vertical))))


(defun sgf-graph-valid-char-p (char)
  "Check if CHAR is a valid character for a graph node.

Allow `*', and a-z."
  (or (eq char ?*)
      (and (>= char ?a)
           (<= char ?z))))


(defun sgf-graph-pos-to-path (vertical)
  "Generate a path based on the current position in an SGF graph.
Each step in the path corresponds to the column and line traversals
from the current position to the root of the graph.

VERTICAL is the prefix argument to specify whether the graph tree is
vertical or horizontal (default). See also `sgf-traverse' and
`sgf-graph-path-to-pos'."
  (interactive "P")
  (unwind-protect
      (if (sgf-graph-valid-char-p (char-before))
          (let ((path '())
                (steps (/ (1- (current-column)) 2))
                column)
            (cursor-intangible-mode -1)
            (save-excursion
              (forward-char -1)
              (while (> (point) 1)
                (unless (eq ?* (char-after))
                  (push (char-after) path))
                (forward-char -2)
                ;; move to the same column of the previous line.
                (setq column (current-column))
                (when vertical
                  (forward-line -1)
                  (forward-char column))
                (while (or (eq (char-after) ?|) ; for vertical graph
                           (eq (char-after) ?`)); for horizontal graph
                  (forward-line -1)
                  (forward-char column))))
            (push steps path)
            (message "%s" (sgf-path-to-str path))
            path)
        (message "It seems the cursor is not on the valid node in graph."))
    (cursor-intangible-mode 1)))


(defun sgf-graph-path-to-pos (vertical path)
  "Move to the point in the graph tree for the path.

VERTICAL is the prefix argument to specify whether the graph tree is
vertical or horizontal (default). See also `sgf-traverse' and
`sgf-graph-pos-to-path'."
  (interactive "P\nxTraverse path: ")
  (unwind-protect
      (let ((steps (pop path)) (column 0) char branch)
        (cursor-intangible-mode -1)
        (goto-char 1)
        (catch 'exit-loop
          (dotimes (i steps)
            (setq column (+ column 2))
            (if vertical
                (progn (forward-line 1)
                       (forward-char column))
              (forward-char 2))
            (setq char (char-after))
            (unless (sgf-graph-valid-char-p char)
              (message "Moved %d steps to invalid char %c." i char)
              (throw 'exit-loop i))
            (when (and path (/= char ?*))
              (setq branch (pop path))
              (while (not (eq branch (char-after)))
                (forward-line)
                (forward-char column))))
          (forward-char)))
    (cursor-intangible-mode 1)))


(defun sgf-graph-subtree-v (root-lnode)
  "Generate vertical ASCII tree representation from a doubly-linked node.

The play move comment will be shown."
  (let ((stack (list (list root-lnode "*" "" ""))))
    (while stack
      (let* ((current (pop stack))
             (lnode (nth 0 current))
             (branch-label (nth 1 current))
             (branch-prefix (nth 2 current))
             (prefix (nth 3 current))
             (node (aref lnode 1))
             (comment (alist-get 'C node))
             (comment-suffix (if comment (format ":%s\n" (car comment)) "\n"))
             (children (aref lnode 2))
             (child-count (length children))
             (i (1- child-count)))
        ;; Insert the current node representation
        (insert (propertize branch-prefix 'cursor-intangible t))
        (insert branch-label)
        (insert (propertize comment-suffix 'cursor-intangible t))
        ;; Add children to the stack in reverse order for proper traversal
        (while (>= i 0)
          (let* ((is-last (= i (1- child-count)))
                 (child (nth i children))
                 (branch-label (if (= child-count 1) "*" (char-to-string (+ ?a i))))
                 (branch-prefix (concat prefix (if is-last "`-" "|-")))
                 (prefix (concat prefix (if is-last "  " "| "))))
            (push (list child branch-label branch-prefix prefix) stack))
          (setq i (1- i)))))))


(defun sgf-graph--transform-str (str size)
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
  "Generate horizontal ASCII tree representation for SGF subtree non-recursively.

ROOT-NODE is the root node."
  (let ((stack (list (list root-lnode 1))))
    (insert "*") ; root
    (while stack
      (let* ((current (pop stack))
             (lnode (car current))
             (node (aref lnode 1))
             (comment (alist-get 'C node))
             (line-n (cadr current))
             (children (aref lnode 2))
             (child-count (length children)))
        ;; move to the end of line line-n
        (goto-char 0)
        (end-of-line line-n)
        (if comment
            (add-text-properties (1- (point)) (point)
                                 `(help-echo ,(car comment) face sgf-graph-comment-node)))
        ;; Get the next line and transform it to the prefix
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position 2)
                      (line-end-position 2)))
               (prefix (sgf-graph--transform-str
                        line
                        (- (current-column) 1))))
          ;; (message "  line: %s\nprefix: %s" line prefix)
          (dotimes (i child-count)
            (let* ((is-last (= i (1- child-count)))
                   (is-first (= i 0))
                   (child (nth i children)))
              (insert (propertize
                       (if is-first
                           "-"
                         (concat "\n" prefix (if is-last "`-" "|-")))
                       'cursor-intangible t))
              (insert (if (= child-count 1) "*" (char-to-string (+ ?a i))))
              (push (list child (+ i line-n)) stack))))))
    ;; add newline to the end of buffer
    (goto-char (point-max))
    (insert "\n")))


(defun sgf-graph-sync-game ()
  "Sync the game state to the current node in the graph tree."
  (interactive)
  (let ((ov sgf-graph-which-game)
        (path (sgf-graph-pos-to-path sgf-graph-vertical)))
    (sgf-first-move nil ov)
    (sgf-traverse path ov t)
    (display-buffer (overlay-buffer ov))
    (message "Synced the game state to the current node in the graph tree.")))


(defvar-keymap sgf-graph-mode-map
  :doc "Keymap for SGF Graph mode."
  "C-c p" #'sgf-graph-path-to-pos
  "C-c P" #'sgf-graph-pos-to-path
  "C-c s" #'sgf-graph-sync-game)


(define-derived-mode sgf-graph-mode nil "SGF-Graph"
  "Major mode for viewing SGF graph tree."
  :keymap sgf-graph-mode-map
  (view-mode 1))


(provide 'sgf-graph)
