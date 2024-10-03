;;; sgf-game.el --- prepare game data object -*- lexical-binding: t -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords


;;; Commentary:
;; process syntax tree parsed from SGF buffer/file to data object for game.

;;; Code:

(require 'sgf-parse)


(defun sgf-game-from-file-pop (file name)
  "Display in a popup buffer the pretty printed Emacs Lisp object of game state converted from
the SGF content of FILE."
  (interactive "fRead SGF file: \nsOutput to buffer name: ")
  (sgf-parse-file file 'sgf-game-from-buffer name))


(defun sgf-game-from-buffer (beg end)
  "Convert the sgf content of buffer to emacs lisp object of game state."
  (let ((pre-root-lnode (vector nil nil nil))
        (sgf-tree (sgf-parse-buffer beg end))
        root-lnode root-node)
    (sgf-game-linkup-nodes pre-root-lnode sgf-tree)
    ;; remove pre-root-lnode
    (setq root-lnode (car (aref pre-root-lnode 2)))
    (aset root-lnode 0 nil)
    (sgf-game-start root-lnode)))


(defun sgf-game-start (root-lnode)
  "Create a game state object from the root of linked nodes of the SGF tree."
  (let* ((root-node (aref root-lnode 1))
         (size (car (alist-get 'SZ root-node)))
         (w (car size)) (h (cdr size))
         (board-2d (sgf-board-2d-create w h 'E))
         (turn (car (alist-get 'PL root-node))))
    (sgf-game-setup-board root-node board-2d)
    (sgf-game-state root-lnode board-2d nil turn)))


(defun sgf-game-setup-board (root board-2d &optional clear)
  ;; process root node to add setup stones
  (dolist (prop root)
    (let* ((prop-key (car prop))
           (prop-vals (cdr prop))
           (setup-stone (cond ((eq prop-key 'AB) 'B)
                              ((eq prop-key 'AW) 'W))))
      (if setup-stone
          (dolist (xy prop-vals)
            (sgf-board-2d-set xy setup-stone board-2d))))))


(defun sgf-game-linkup-nodes (head-lnode sgf-tree)
  "Convert parsed SGF syntax tree to doubly linked list of nodes."
  (let ((prev-lnode head-lnode)
        (tree (aref sgf-tree 1)) ; retrieve the real data from vector
        curr-node curr-lnode)
    (dolist (item tree)
      (cond ((listp item)
             ;; it is a fork of subtrees
             (dolist (sgf-subtree item)
               (sgf-game-linkup-nodes prev-lnode sgf-subtree)))
            ((vectorp item)
             ;; it is a node
             (setq curr-node (sgf-game-node item))
             (setq curr-lnode (sgf-game-linked-node prev-lnode curr-node))
             (aset prev-lnode 2 (append (aref prev-lnode 2) (list curr-lnode))))
            (t (error "Invalid node or subtree: %s." item)))
      (setq prev-lnode curr-lnode)))
  head-lnode)


;; Linked Node Object
(defun sgf-game-linked-node (prev-node &optional current-node next-nodes)
  "Define linked node object. It is doubly linked list."
  (vector prev-node current-node next-nodes))

;; Game State
(defun sgf-game-state (linked-node
                       board-2d
                       &optional
                       ko
                       turn
                       prisoners
                       undos)
  "Define game state object. The move number, board-2d, ko, prisoners are re-computed every time when traversing the moves."
  (vector linked-node                 ; 0 current node
          board-2d                    ; 1
          ko                          ; 3 ko position
          (or turn 'B)                        ; 2 move turn
          ;; 4. accumulated number of prisoners (b . w)
          (or prisoners (cons 0 0))
          ;; 5. stack of changes. each change contains:
          ;; - a list of cons cells: xy positions for black stones
          ;; - a list of cons cells: xy positions for white stones
          ;; - a list of cons cells: xy positions for empty
          ;; - ko position (cons cell)
          ;; - turn
          undos))


(defun sgf-game-node (sgf-node)
  "Parse SGF node in syntax tree into node object."
  (let ((node (aref sgf-node 1))) ; retrieve the real data from vector
    (mapcar #'sgf-game-prop node)))


(defun sgf-game-prop (sgf-prop)
  "Process SGF property into property object."
  (let* ((key (intern (aref sgf-prop 1)))
         (values (aref sgf-prop 2))
         (prop (list key)))

    (dolist (value values)
      (let* ((val-str (aref value 1))
             (beg-pos (aref value 0))
             (end-pos (aref value 2))
             (prop-val
              (cond ((member key '(B W AB AW AE MA SQ TR CR))
                     (sgf-game-prop-position val-str beg-pos end-pos))
                    ((eq key 'C)
                     (list (sgf-game--prettify-text val-str)))
                    ((eq key 'PL)
                     (cond ((string= val-str "B") '(B))
                           ((string= val-str "W") '(W))
                           (t (error "%sInvalid player value - should be B or W (%s)."
                                     (sgf-game--format-location beg-pos end-pos) val-str))))
                    ((eq key 'SZ)
                     (list (sgf-game-prop-SZ val-str beg-pos end-pos)))
                    ((eq key 'GM)
                     (unless (string= val-str "1")
                       (error "%sGame type is not Go (GM[1])."
                              (sgf-game--format-location beg-pos end-pos) val-str)))
                    ((eq key 'MN)
                     (list (string-to-number val-str)))
                    ((eq key 'LB)
                     (list (sgf-game-prop-LB val-str)))
                    (t (list val-str)))))
        (nconc prop prop-val)))
    prop))


(defun sgf-game-prop-SZ (val &optional beg end)
  "Process the SZ property from an SGF file and return a cons cell of board width and height."
  (let* ((nums (split-string val ":"))
         (width (car nums))
         (height (or (cadr nums) width))
         (w (string-to-number width))
         (h (string-to-number height)))
    (if (and (>= w 1) (<= w 52) (>= h 1) (<= h 52))
        (cons w h)
      (error "%sInvalid board size (w=%S h=%S) (%s)"
             (sgf-game--format-location beg end) w h val))))


(defun sgf-game-prop-LB (val &optional beg end)
  "for property LB: label a position or stone. e.g. LB[ee:label]"
  (if (not (equal (elt val 2) ?:))
      (error "%sInvalid label property value (%s)."
             (sgf-game--format-location beg end) val))
  (let* ((pos (substring val 0 2))
         (label (substring val 3)))
    (cons (sgf-game-prop-single-position pos) label)))


(defun sgf-game-prop-single-position (val &optional beg end)
  "Process a position property from an SGF file and return a cons cell of x and y."
  (if (/= (length val) 2)
      (error "%sPosition value needs to be 2 letters (%s)."
             (sgf-game--format-location beg end) val))
  (let* ((x (sgf-game--letter-to-number (elt val 0)))
         (y (sgf-game--letter-to-number (elt val 1))))
    (if (and (>= x 0) (<= x 51) (>= y 0) (<= y 51))
        (cons x y)
      (error "%sPosition value is out of range (x=%d y=%d) (%s)."
             (sgf-game--format-location beg end) x y val))))


(defun sgf-game-prop-position (val &optional beg end)
  "https://www.red-bean.com/sgf/sgf4.html#3.5.1
  Convert a multi-position string in 'aa:ac' format into a list of numeric pairs."
  (let* ((positions (split-string val ":"))
         (count (length positions)))
    (cond ((= count 1)
           (list (sgf-game-prop-single-position val beg end)))
          ((= count 2)
           (let* ((tl (sgf-game-prop-single-position (car positions) beg end)) ; top left
                  (br (sgf-game-prop-single-position (cadr positions) beg end)); bottom right
                  (tl-x (car tl)) (tl-y (cdr tl))
                  (br-x (car br)) (br-y (cdr br))
                  (y-seq (number-sequence tl-y br-y))
                  (x-seq (number-sequence tl-x br-x)))
             (apply 'append
                    (mapcar (lambda (x)
                              (mapcar (lambda (y) (cons x y)) y-seq))
                            x-seq))))
          (t (error "%sOnly one colon is allowed in compressed position value (%s)."
                    (sgf-game--format-location beg end) val)))))



(defun sgf-game--letter-to-number (ch)
  "Convert a letter to an integer.

?a-?z ?A-?Z to 0-25 26-51"
  (cond
   ((and (>= ch ?a) (<= ch ?z)) (- ch ?a))
   ((and (>= ch ?A) (<= ch ?Z)) (+ (- ch ?A) 26))
   (t (error "Invalid position letter '%c'" ch))))


(defun sgf-game--format-location (begin end)
  (cond ((and begin end) (format "%s-%s: " begin end))
        (begin (format "%s: " begin))
        (end (format "%s: " end))
        (t "")))

;; todo: format and prettify text like move comment str
(defun sgf-game--prettify-text (txt)
  ;; Delete escaped line-break
  (setq txt (replace-regexp-in-string "\\\\\\(\n\r?\\|\r\n?\\)" "" txt))
  ;; Escape character
  (setq txt (replace-regexp-in-string "\\\\\\(.\\)" "\\1" txt))
  ;; Convert Spaces
  (setq txt (replace-regexp-in-string "\t\\|\v\\|\n\r?\\|\r\n?" " " txt))
  txt)


(provide 'sgf-game)
;;; sgf-game.el ends here
