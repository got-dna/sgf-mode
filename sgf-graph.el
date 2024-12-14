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

(defvar-local sgf-graph--game nil
  "The game overlay the sgf tree graph is associated with.")

(defvar-local sgf-graph--vertical nil
  "The direction of the sgf tree graph is oriented.")

;;;###autoload
(defun sgf-graph-tree (&optional ov bname vertical interactive-call)
  "Generate an graph to show all game variations in a tree structure.

The prefix argument VERTICAL specifies the direction of the tree
structure. By default, the tree is graphed in horizontal direction. If
prefix argument is provided or VERTICAL is t, the tree will be graphed
in vertical direction. BNAME is the name of the output buffer."
  (interactive "i\ni\nP\np")
  (let* ((ov (or ov (sgf-get-overlay)))
         ;; get the existing graph buffer or create a new one
         (graph-buffer (overlay-get ov 'graph-buffer))
         (exist-p (buffer-live-p graph-buffer)))
    (when (or interactive-call exist-p)
      (unless exist-p
        (setq graph-buffer
              (get-buffer-create
               (or bname
                   (read-buffer "Output buffer name: "
                                (if vertical "*SGF TREE V*" "*SGF TREE H*")))))
        ;; put the graph buffer in the overlay
        (overlay-put ov 'graph-buffer graph-buffer))
      ;; display the graph buffer before `recenter'
      (display-buffer graph-buffer)
      (with-current-buffer graph-buffer
        (let* ((lnode (sgf-get-lnode ov))
               (path (sgf-lnode-path lnode))
               (inhibit-read-only t)
               (vertical (or vertical sgf-graph--vertical)))
          ;; move to the root-lnode
          (while (aref lnode 0) (setq lnode (aref lnode 0)))
          (erase-buffer)
          (if vertical
              (sgf-graph-subtree-v lnode)
            (sgf-graph-subtree-h lnode))
          (setq truncate-lines t) ; do not wrap long lines
          ;; it seems `recenter' only works in the active window
          (with-selected-window (get-buffer-window graph-buffer)
            ;; move to the current node of the game
            (sgf-graph-path-to-pos vertical path)
            (sgf-graph-hl-before-cursor))
          (unless (eq major-mode 'sgf-graph-mode) (sgf-graph-mode))
          ;; set local variables:
          ;; 1. the game that the graph tree buffer is associated.
          ;; 2. the direction of the graph
          ;; This has to be done after the mode is enabled - major mode enabling
          ;; kills all local variables.
          (setq sgf-graph--game ov)
          (setq sgf-graph--vertical vertical))))))


(defun sgf-graph-valid-char-p (char)
  "Check if CHAR is a valid character for a graph node.

Allow `*', and a-z."
  (and (integerp char)
       (or (eq char ?*)
           (and (>= char ?a)
                (<= char ?z)))))


(defun sgf-graph-pos-to-path (vertical &optional interactival-call)
  "Generate a path based on the current position in an SGF graph.
Each step in the path corresponds to the column and line traversals
from the current position to the root of the graph.

VERTICAL is the prefix argument to specify whether the graph tree is
vertical or horizontal (default). See also `sgf-traverse' and
`sgf-graph-path-to-pos'."
  (interactive "P\np")
  (if (sgf-graph-valid-char-p (char-before))
      (let ((path '())
            (steps (/ (1- (current-column)) 2))
            column)
        (save-excursion
          (backward-char)
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
        (if interactival-call (message "%s" (sgf-path-to-str path)))
        path)
    (message "It seems the cursor is not on the valid node in graph.")
    ;; return nil if failed to get path
    nil))


(defun sgf-graph-path-to-pos (vertical path)
  "Move to the point in the graph tree for the path.

VERTICAL is the prefix argument to specify whether the graph tree is
vertical or horizontal (default). See also `sgf-traverse' and
`sgf-graph-pos-to-path'."
  (interactive "P\nxTraverse path: ")
  (let ((steps (pop path)) (column 0) char branch)
    (goto-char (point-min))
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
            (forward-char column)))))
    (forward-char 1)))


(defun sgf-graph-subtree-v (root-lnode)
  "Generate vertical ASCII tree representation from a doubly-linked node.

The play move comment will be shown."
  (let ((stack (list (list root-lnode "*" ""))))
    (while stack
      (let* ((current (pop stack))
             (lnode (nth 0 current))
             (branch (nth 1 current))
             (prefix (nth 2 current))
             (node (aref lnode 1))
             (comment (alist-get 'C node))
             (comment-suffix (if comment (concat ":" (car comment)) ""))
             (children (aref lnode 2))
             (child-count (length children))
             (i (1- child-count)))
        ;; Insert the current node representation
        (insert (format "%s%s\n" branch comment-suffix))
        ;; Add children to the stack in reverse order for proper traversal
        (while (>= i 0)
          (let* ((is-last (= i (1- child-count)))
                 (child (nth i children))
                 (label (if (= child-count 1) "*" (char-to-string (+ ?a i))))
                 (branch (concat prefix (if is-last "`-" "|-") label))
                 (prefix (concat prefix (if is-last "  " "| "))))
            (push (list child branch prefix) stack))
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
              (insert (if is-first
                          "-"
                        (concat "\n" prefix (if is-last "`-" "|-"))))
              (insert (if (= child-count 1) "*" (char-to-string (+ ?a i))))
              (push (list child (+ i line-n)) stack))))))
    ;; add newline to the end of buffer
    (goto-char (point-max))
    (insert "\n")))


(defun sgf-graph-sync-game (&optional interactive-call)
  "Sync the game state to the current node in the graph tree."
  (interactive "p")
  (let ((ov sgf-graph--game)
        (path (sgf-graph-pos-to-path sgf-graph--vertical)))
    (when path
      (sgf-first-move ov)
      (sgf-traverse path ov t)
      (when interactive-call
        (pop-to-buffer (overlay-buffer ov))
        (message "Synced the game state to the current node in the graph tree.")))))


(defun sgf-forward-char ()
  "Custom forward character movement, putting cursor only after a node."
  (interactive)
  (unless (eobp) (forward-char 1))
  (while (and (not (eobp))
              (not (eq (char-before) ?-)))
    (forward-char 1))
  (unless (eobp) (forward-char 1)))


(defun sgf-backward-char ()
  "Custom backward character movement, putting cursor only after a node."
  (interactive)
  (if (> (point) 2) (backward-char 2))
  (while (and (not (bobp))
              (not (eq (char-before) ?-)))
    (backward-char 1))
  (forward-char 1))


(defun sgf-graph-hl-before-cursor ()
  "Highlight the valid character before the cursor."
  (remove-overlays nil nil)
  (if (sgf-graph-valid-char-p (char-before))
      (let ((ov (make-overlay (1- (point)) (point))))
        (overlay-put ov 'face 'sgf-graph-current-node)
        (overlay-put ov 'priority 100))))


(defvar-keymap sgf-graph-mode-map
  :doc "Keymap for SGF Graph mode."
  :suppress t
  "f" #'sgf-forward-char
  "b" #'sgf-backward-char
  "p" #'sgf-graph-pos-to-path
  "P" #'sgf-graph-path-to-pos
  "s" #'sgf-graph-sync-game)


(define-derived-mode sgf-graph-mode nil "SGF-Graph"
  "Major mode for viewing SGF graph tree."
  :keymap sgf-graph-mode-map
  (setq buffer-read-only t)
  (add-hook 'post-command-hook #'sgf-graph-hl-before-cursor t t))


(provide 'sgf-graph)
