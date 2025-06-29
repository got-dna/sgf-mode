;;; sgf-graph.el --- visualize game in tree graph  -*- lexical-binding: t; -*-



;; Author: Zech Xu
;; Version: 1.0
;; Package-Requires: ((emacs "30.1") (sgf-util "1.0") (sgf-svg "1.0") (sgf-io "1.0"))
;; Homepage: https://github.com/RNAer/sgf-mode
;; Keywords: SGF, go, game


;;; Commentary:


;;; Code:

(require 'sgf-util)
(require 'sgf-svg)
(require 'sgf-io)

(defvar sgf-graph-buffer-name "*SGF TREE*"
  "The default buffer name for the SGF tree graph.")

(defface sgf-graph-current-node
  '((t :foreground "magenta" :weight bold))
  "Face for the current node in the graph tree."
  :group 'sgf-graph)

(defface sgf-graph-comment-node
  '((t :inherit link))
  "Face for the comment node in the graph tree."
  :group 'sgf-graph)

(defmacro sgf-define-node-face (name color)
 "Define face for move annotation of type NAME using COLOR."
 `(defface ,(intern (format "sgf-graph-%s-node" name))
    ;; specify both horizontal and vertical box width
    `((t :box (:line-width (-1 . -3) :color ,,color :style flat-button)))
    ,(format "Face for the %s move annotation." name)))

(sgf-define-node-face "it" sgf-it-color)
(sgf-define-node-face "te" sgf-te-color)
(sgf-define-node-face "do" sgf-do-color)
(sgf-define-node-face "bm" sgf-bm-color)

(defvar-local sgf-graph--game nil
  "The game overlay the sgf tree graph is associated with.")

(defvar-local sgf-graph--direction 'h
  "The direction of the sgf tree graph is oriented.")

;;;###autoload
(defun sgf-graph-hv (&optional vertical ov)
  "Generate an vertical or horizontal (default) graph to show all game variations in a tree structure."
  (interactive "P")
  (sgf-graph-tree ov "*SGF TREE*" (if vertical 'v 'h) t))


(defun sgf-graph-tree (&optional ov bname direction force)
  "Generate an graph to show all game variations in a tree structure.

DIRECTION specifies the direction of the tree structure. By default, the
tree is graphed in horizontal direction. BNAME is the name of the output
buffer. FORCE is boolean to force to create the buffer if it exists in
the overlay property."
  (let* ((ov (or ov (sgf-get-overlay-at)))
         ;; get the existing graph buffer or create a new one
         (graph-buffer (overlay-get ov 'graph-buffer))
         (exist-p (buffer-live-p graph-buffer)))
    (when (or exist-p force)
      (unless exist-p
        (setq graph-buffer
              (get-buffer-create
               (or bname (read-buffer "Output buffer name: " sgf-graph-buffer-name))))
        ;; put the graph buffer in the overlay
        (overlay-put ov 'graph-buffer graph-buffer))
      ;; display the graph buffer before `recenter'
      (display-buffer graph-buffer)
      (with-current-buffer graph-buffer
        (let* ((lnode (sgf-get-lnode ov))
               (path (sgf-lnode-path lnode))
               (inhibit-read-only t)
               (cursor-in-non-selected-windows 'box)
               (direction (or direction sgf-graph--direction)))
          ;; move to the root-lnode
          (while (aref lnode 0) (setq lnode (aref lnode 0)))
          (erase-buffer)
          (cond ((eq direction 'v) (sgf-graph-subtree-v lnode))
                ((eq direction 'h) (sgf-graph-subtree-h lnode))
                (t (error "Invalid direction: %s" direction)))
          (setq truncate-lines t) ; do not wrap long lines
          ;; it seems `recenter' only works in the active window
          (with-selected-window (get-buffer-window graph-buffer)
            ;; move to the current node of the game
            (sgf-graph-path-to-pos (eq direction 'v) path)
            (sgf-graph-hl-before-cursor))
          (unless (eq major-mode 'sgf-graph-mode) (sgf-graph-mode))
          ;; set local variables:
          ;; 1. the game that the graph tree buffer is associated.
          ;; 2. the direction of the graph
          ;; This has to be done after the mode is enabled - major mode enabling
          ;; kills all local variables.
          (setq sgf-graph--game ov)
          (setq sgf-graph--direction direction))))))


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
  (interactive "P\np" sgf-graph-mode)
  (if (sgf-graph-valid-char-p (char-before))
      (let ((path '())
            (steps (/ (1- (current-column)) 2))
            column)
        (save-excursion
          (backward-char)
          (while (> (point) 1)
            (unless (memq (char-after) '(?* ?o ?x))
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
  (interactive "P\nxTraverse path: " sgf-graph-mode)
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
        (when (and path (not (memq (char-after) '(?* ?o ?x))))
          (setq branch (pop path))
          (while (not (eq branch (char-after)))
            (forward-line)
            (forward-char column)))))
    (forward-char 1)))


(defun sgf-graph-node-annotation-face (node)
 "Return node annotation face for NODE."
 (cond ((alist-get 'IT node) 'sgf-graph-it-node)
       ((alist-get 'TE node) 'sgf-graph-te-node)
       ((alist-get 'DO node) 'sgf-graph-do-node)
       ((alist-get 'BM node) 'sgf-graph-bm-node)))


(defun sgf-graph-subtree-v (root-lnode)
  "Generate vertical ASCII tree representation for the game from a doubly-linked node.

The play move comment will be shown. See also `sgf-graph-subtree-h'."
  (let ((stack (list (list root-lnode "*" ""))))
    (while stack
      (let* ((current (pop stack))
             (lnode (nth 0 current))
             (branch (nth 1 current))
             (prefix (nth 2 current))
             (node (aref lnode 1))
             (annt (sgf-graph-node-annotation-face node))
             (comment (alist-get 'C node))
             (comment-suffix (if comment (concat ":" (car comment)) ""))
             (children (aref lnode 2))
             (child-count (length children))
             (i (1- child-count)))
        ;; Insert the current node representation
        (insert branch)
        (put-text-property (1- (point)) (point) 'face annt)
        (insert comment-suffix "\n")
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
  "Generate horizontal ASCII tree representation for the game (non-recursively).

ROOT-LNODE is the doubly linked root node. See also `sgf-graph-subtree-v'."
  (let* ((node (aref root-lnode 1))
         (children (aref root-lnode 2))
         (stack (list (list node children 1))))
    (insert "*") ; root
    (while stack
      (let* ((current (pop stack))
             (line-n (nth 2 current))
             (children (nth 1 current))
             (child-count (length children))
             (node (nth 0 current))
             (annt (sgf-graph-node-annotation-face node))
             (comment (car (alist-get 'C node)))
             (props (cond ((and comment annt)
                           `(help-echo ,comment face (sgf-graph-comment-node ,annt)))
                          ((and comment (null annt))
                           `(help-echo ,comment face sgf-graph-comment-node))
                          ((and (null comment) annt)
                           `(face ,annt)))))
        ;; move to the end of line line-n
        (goto-char 0)
        (end-of-line line-n)
        (add-text-properties (1- (point)) (point) props)
        ;; Get the next line and transform it to the prefix
        (let* ((line (buffer-substring-no-properties (pos-bol 2) (pos-eol 2)))
               (prefix (sgf-graph--transform-str line (- (current-column) 1))))
          ;; (message "  line: %s\nprefix: %s" line prefix)
          (dotimes (i child-count)
            (let* ((is-last (= i (1- child-count)))
                   (is-first (= i 0))
                   (child (nth i children))
                   (node (aref child 1))
                   (move (sgf-process-move node))
                   (stone (car move)))
                (insert (if is-first
                            "-"
                          (concat "\n" prefix (if is-last "`-" "|-"))))
                (insert (if (= child-count 1)
                            (if (eq stone 'B) "x" "o")
                          (char-to-string (+ ?a i))))

                (push (list node (aref child 2) (+ i line-n)) stack))))))
    ;; add newline to the end of buffer
    (goto-char (point-max))
    (insert "\n")))


(defun sgf-graph-sync-game (&optional interactive-call)
  "Sync the game state to the current node in the graph tree."
  (interactive "p" sgf-graph-mode)
  (let ((ov sgf-graph--game)
        (path (sgf-graph-pos-to-path (eq sgf-graph--direction 'v))))
    (when path
      (sgf-first-move ov)
      (sgf-traverse path ov t)
      (when interactive-call
        (pop-to-buffer (overlay-buffer ov))
        (message "Synced the game state to the current node in the graph tree.")))))


(defun sgf-graph-forward-char ()
  "Custom forward character movement, putting cursor only after a node."
  (unless (eobp) (forward-char 1))
  (while (and (not (eobp))
              (not (eq (char-before) ?-)))
    (forward-char 1))
  (unless (eobp) (forward-char 1)))


(defun sgf-graph-backward-char ()
  "Custom backward character movement, putting cursor only after a node."
  (if (> (point) 2) (backward-char 2))
  (while (and (not (bobp))
              (not (eq (char-before) ?-)))
    (backward-char 1))
  (forward-char 1))


(defun sgf-graph-forward-node (&optional n)
  "Forward to the next node in the graph tree."
  (interactive "P" sgf-graph-mode)
  (unless (eolp) (sgf-graph-forward-char))
  (let* ((col (current-column))
         (n (or n 0))
         (char (+ ?a n)))
    (while (not (memq (char-before) (list char ?x ?o ?*)))
      (forward-line)
      (move-to-column col))))


(defun sgf-graph-backward-node ()
  "Backward to the previous node in the graph tree."
  (interactive nil sgf-graph-mode)
  (let ((col (current-column)))
    (while (and (not (memq (char-before) (list ?x ?o ?* ?a)))
                (not (bobp)))
      (forward-line -1)
      (move-to-column col)))
  (sgf-graph-backward-char))


(defun sgf-graph-hl-before-cursor ()
  "Highlight char before the cursor if it is a valid node character."
  (remove-overlays nil nil)
  (if (sgf-graph-valid-char-p (char-before))
      (let ((ov (make-overlay (1- (point)) (point))))
        (overlay-put ov 'face 'sgf-graph-current-node)
        (overlay-put ov 'priority 100))))


(defun sgf-graph-forward-comment ()
  "Forward to the next node with comment."
  (interactive nil sgf-graph-mode)
  (sgf-graph-forward-char)
  (while (not (get-text-property (1- (point)) 'help-echo))
    (sgf-graph-forward-char)))


(defun sgf-graph-backward-comment ()
  "Forward to the previous node with comment."
  (interactive nil sgf-graph-mode)
  (sgf-graph-backward-char)
  (while (not (get-text-property (1- (point)) 'help-echo))
    (sgf-graph-backward-char)))


(defvar-keymap sgf-graph-mode-map
  :doc "Keymap for SGF Graph mode."
  :suppress t
  "<mouse-1>" 'ignore
  "f" #'sgf-graph-forward-node
  "b" #'sgf-graph-backward-node
  "F" #'sgf-graph-forward-comment
  "B" #'sgf-graph-backward-comment
  ;; "M-f" is forward-word, ie forward to next branch char;
  ;; "M-b" is likewise.
  "p" #'sgf-graph-pos-to-path
  "P" #'sgf-graph-path-to-pos
  "s" #'sgf-graph-sync-game)


(define-derived-mode sgf-graph-mode special-mode "SGF-Graph"
  "Major mode for viewing SGF graph tree."
  :keymap sgf-graph-mode-map
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (add-hook 'post-command-hook #'sgf-graph-hl-before-cursor t t))


(provide 'sgf-graph)
;;; sgf-graph.el ends here
