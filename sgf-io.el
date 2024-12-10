;;; sgf-io.el --- input and output sgf file and prep data   -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides
;; 1. functions to parse SGF content into a syntax tree
;; 2. functions to process syntax tree further into amenable list of emacs object.
;; 3. functions to serialize game object into SGF string and write out to file.

;; SGF format specs:
;; https://homepages.cwi.nl/~aeb/go/misc/sgf.html#contents

;;; Code:

(require 'sgf-util)


;;; parse SGF into syntax tree


(defun sgf-parse-get-char ()
  (when (< (point) (point-max))
    (forward-char)
    (char-before)))

(defun sgf-parse-ws-p (c)
  (and
   (integerp c)
   (or (= c ? )
       (= c ?\t)
       (= c ?\v)
       (= c ?\n)
       (= c ?\r))))

(defun sgf-parse-upcase-p (c)
  (and (integerp c)
       (>= c ?A)
       (<= c ?Z)))

(defun sgf-parse-skip-ws ()
  (while (sgf-parse-ws-p (char-after))
    (forward-char)))

(defun sgf-parse-match-char (c)
  (let ((strm-c (char-after)))
    (if (equal strm-c c)
        (forward-char)
      (error "%d: Unexpected %s (expecting '%c')."
             (point)
             (if strm-c (concat "'" (char-to-string strm-c) "'") "end of SGF")
             c))))

(defun sgf-parse-scan-upcase ()
  "Scan [A-Z]* from strm."
  (let (ch chars)
    (while (sgf-parse-upcase-p (setq ch (char-after)))
      (push ch chars)
      (forward-char))
    (concat (nreverse chars))))


;;
;; Parse SGF
;;
;; Syntax:
;;
;;  Collection = GameTree+
;;  GameTree   = "(" Sequence GameTree* ")"
;;  Sequence   = Node+
;;  Node       = ";" Property*
;;  Property   = PropKey PropValue+
;;  PropKey    = CHAR+
;;  PropValue  = "[" CValueType "]"
;;  CValueType = (ValueType | Compose)
;;  ValueType  = (None | Number | Real | Double | Color | SimpleText |
;;                Text | Point  | Move | Stone)
;;  (see: https:www.red-bean.com/sgf/sgf4.html )
;;
;; Functions corresponding to non-terminal symbols:
;; - sgf-parse-trees :: GameTree*
;; - sgf-parse-tree :: GameTree
;; - sgf-parse-nodes :: Node*
;; - sgf-parse-node :: Node
;; - sgf-parse-props :: Property*
;; - sgf-parse-prop :: Property
;; - sgf-parse-prop-value :: PropValue

(defun sgf-parse-trees ()
  "Parse 0 or more GameTree and return list of tree object."
  ;; Tree*
  ;; (...) (...) (...)
  (let (trees)
    (while (progn (sgf-parse-skip-ws) (equal (char-after) ?\())
      (push (sgf-parse-tree) trees))
    (nreverse trees)))

(defun sgf-parse-tree ()
  "Parse GameTree and return a tree object [begin-pos nodes/subtrees end-pos]."

  (sgf-parse-skip-ws)

  ;; ( Node+ Tree* )
  ;; ( ;... ;... ;... (...) (...) (...) )
  (let (begin-pos nodes subtrees end-pos)
    ;; (
    (setq begin-pos (point))
    (sgf-parse-match-char ?\()

    ;; Node+
    (setq nodes (sgf-parse-nodes))
    (if (null nodes)
        (error "%d: GameTree requires one or more Nodes." (point)))

    ;; Tree*
    (setq subtrees (sgf-parse-trees))

    ;; subtree could be nil;
    ;; if not nil, treat subtree as the same level as the parallel nodes.
    (if subtrees (nconc nodes (list subtrees)))

    ;; )
    (sgf-parse-match-char ?\))
    (setq end-pos (point))

    (vector begin-pos nodes end-pos)))

(defun sgf-parse-nodes ()
  "Parse 0 or more Node and return list of node object."
  ;; Node*
  ;; ;... ;... ;...
  (let (nodes)
    (while (progn (sgf-parse-skip-ws) (equal (char-after) ?\;))
      (push (sgf-parse-node) nodes))
    (nreverse nodes)))

(defun sgf-parse-node ()
  "Parse Node and return a node object [begin-pos properties end-pos]."
  (sgf-parse-skip-ws)
  ;; ; Property*
  ;; ; PID[PValue][PValue][PValue]PID[PValue][PValue][PValue]...
  (let (begin-pos properties end-pos)
    ;; ;
    (setq begin-pos (point))
    (sgf-parse-match-char ?\;)

    ;; Property*
    (setq properties (sgf-parse-props))

    (setq end-pos (point))

    (vector begin-pos properties end-pos)))

(defun sgf-parse-props ()
  ;; Property*
  ;; PID[PValue][PValue][PValue]PID[PValue][PValue][PValue]...
  (let (properties)
    (while (progn (sgf-parse-skip-ws) (sgf-parse-upcase-p (char-after)))
      (push (sgf-parse-prop) properties))
    (nreverse properties)))

(defun sgf-parse-prop ()
  "Return property object [begin-pos key values end-pos]"

  (sgf-parse-skip-ws)

  ;; PropKey PropValue+
  (let (begin-pos key values end-pos)
    (setq begin-pos (point))

    ;; PropKey
    (setq key (sgf-parse-scan-upcase))
    (if (string-empty-p key)
        (error "%d: Missing property key" (point)))

    ;; PropValue+
    (while (progn (sgf-parse-skip-ws) (equal (char-after) ?\[))
      (push (sgf-parse-prop-value) values))
    (setq values (nreverse values))
    (if (null values)
        (error "%d: Property requires one or more PropValues" (point)))

    (setq end-pos (point))

    (vector begin-pos key values end-pos)))

(defun sgf-parse-prop-value ()
  "Return processed property value object [begin-pos content end-pos]"
  (sgf-parse-skip-ws)

  (let (begin-pos ch chars end-pos)
    (setq begin-pos (point))
    (sgf-parse-match-char ?\[)

    (while (progn (sgf-parse-skip-ws)
                  (not (equal (setq ch (sgf-parse-get-char)) ?\])))
      ;; end of strm
      (if (null ch)
          (error "%d: Unexpected end of Property Value" (point)))
      ;; push ch
      (push ch chars)
      ;; NOTE: Do not resolve compose value here(colon separation). Depends on property type.

      ;; skip \], convert \r \n\r \r\n to \n
      (if (= ch ?\\)
          (let ((escaped-ch (sgf-parse-get-char)))
            (cond
             ;; convert \\\n\r and \\\r\n to single \n
             ((or (and (= escaped-ch ?\n) (= (char-after) ?\r))
                  (and (= escaped-ch ?\r) (= (char-after) ?\n)))
              (forward-char)
              (push ?\n chars))
             ;; convert single \r to \n
             ((= escaped-ch ?\r)
              (push ?\n chars))
             ;; ], \, :, \n, spaces, etc...
             (t (push escaped-ch chars))))))

    (setq end-pos (point))
    ;; return value
    (vector begin-pos (concat (nreverse chars)) end-pos)))


(defun sgf-parse-buffer-to-syntax-tree (beg end)
  "Parse SGF content between BEGIN and END in current buffer into syntax tree."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((tree (sgf-parse-tree)))
        ;; Check if there is text after end of SGF.
        (sgf-parse-skip-ws)
        (if (char-after) (error "%d: Unnecessary text after end of SGF." (point)))
        tree))))


(defun sgf-parse-buffer-to-linked-node (beg end)
  "Convert SGF syntax tree to doubly linked list of nodes.

Return the root / head of the linked node."
  (let ((tree (sgf-parse-buffer-to-syntax-tree beg end))
        (pre-root-lnode (vector nil nil nil))
        root-lnode)
    (sgf-linkup-nodes pre-root-lnode tree)
    ;; remove pre-root-lnode
    (setq root-lnode (car (aref pre-root-lnode 2)))
    (aset root-lnode 0 nil)
    root-lnode))


(defun sgf-parse-str-to-* (str fn)
  "The str parsing version of the corresponding buffer parsing for SGF.

See also `sgf-parse-file-to-*' and functions prefixed with `sgf-parse-buffer-to'."
  (with-temp-buffer
    (insert str)
    (funcall fn (point-min) (point-max))))


;;;###autoload
(defun sgf-parse-file-to-* (file fn &optional name)
  "The file parsing version of the corresponding buffer parsing for SGF.

Either return the parsing result from function FN or pop up and display
a buffer of NAME with the pretty printed Emacs Lisp object of syntax
tree or game state parsed from the SGF content of FILE.

See also `sgf-parse-str-to-*' and functions prefixed with `sgf-parse-buffer-to'."
  ;; (interactive "fRead SGF file: \naFunction to parse with: \nBOutput to buffer name: ")
  (interactive
   (list
    (read-file-name "Read SGF file: ")
    (intern (completing-read "Parse function: "
                             '(sgf-parse-buffer-to-syntax-tree
                               sgf-parse-buffer-to-linked-node
                               sgf-parse-buffer-to-game-state)))
    (read-buffer "Output buffer: " "*SGF Parsed*" 'confirm)))
  (let ((result
         (with-temp-buffer
           (insert-file-contents-literally file)
           (funcall fn (point-min) (point-max))))) ;; Process the buffer
    (if name
        (let ((buffer (generate-new-buffer name)))
          (with-current-buffer buffer
            (insert (pp-to-string result))
            (emacs-lisp-mode))  ;; Enable emacs-lisp-mode for syntax highlighting
          (pop-to-buffer buffer))
      result)))


;;; Linked Node Object
(defun sgf-linked-node (prev-node &optional current-node next-nodes)
  "Define linked node object. It is doubly linked list."
  (vector prev-node current-node next-nodes))

;;; Game State Object
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
          ko                          ; 2 ko position
          (or turn 'B)                ; 3 move turn
          ;; 4. accumulated number of prisoners (b . w)
          (or prisoners (cons 0 0))
          ;; 5. stack of changes. each change contains:
          ;; - a list of cons cells: xy positions for black stones
          ;; - a list of cons cells: xy positions for white stones
          ;; - a list of cons cells: xy positions for empty
          ;; - ko position (cons cell)
          ;; - turn
          undos))


(defun sgf-parse-buffer-to-game-state (beg end)
  "Parse the SGF content of buffer to emacs lisp object of game state."
  (let* ((root-lnode (sgf-parse-buffer-to-linked-node beg end)))
    (sgf-root-lnode-to-game-state root-lnode)))


(defun sgf-root-lnode-to-game-state (root-lnode)
  "Create a game state object from the root of linked nodes."
  (let* ((root-node (aref root-lnode 1))
         (size (car (alist-get 'SZ root-node)))
         (w (car size)) (h (cdr size))
         (board-2d (sgf-board-create w h 'E))
         (turn (car (alist-get 'PL root-node))))
    ;; if PL is not specified, check the first move to set the correct turn;
    ;; if there is not any move, then set black to start the move.
    (unless turn
      (let* ((next-lnodes (aref root-lnode 2))
             (next-lnode (car next-lnodes)))
        (if next-lnode
             (let* ((next-node (aref next-lnode 1))
                    (move (sgf-process-move next-node))
                    (stone (car move)))
               (setq turn stone)))))

    (sgf-add-setup-stones root-node board-2d)
    (sgf-show-comment root-lnode)
    (sgf-game-state root-lnode board-2d nil turn)))


(defun sgf-linkup-nodes (head-lnode sgf-tree)
  "Convert parsed SGF syntax tree to doubly linked list of nodes."
  (let ((prev-lnode head-lnode)
        (tree (aref sgf-tree 1)) ; retrieve the real data from vector
        curr-node curr-lnode)
    (dolist (item tree)
      (cond ((listp item)
             ;; it is a fork of subtrees
             (dolist (sgf-subtree item)
               (sgf-linkup-nodes prev-lnode sgf-subtree)))
            ((vectorp item)
             ;; it is a node
             (setq curr-node (sgf-decode-node item))
             (setq curr-lnode (sgf-linked-node prev-lnode curr-node))
             ;; TODO (nconc (aref prev-lnode 2) (list curr-lnode))
             (aset prev-lnode 2 (append (aref prev-lnode 2) (list curr-lnode))))
            (t (error "Invalid node or subtree: %s." item)))
      (setq prev-lnode curr-lnode)))
  head-lnode)


(defun sgf-decode-node (sgf-node)
  "Parse SGF node in syntax tree into node object."
  (let ((node (aref sgf-node 1))) ; retrieve the real data from vector
    (mapcar #'sgf-decode-prop node)))


(defun sgf-decode-prop (sgf-prop)
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
                     (sgf-decode-prop-pos val-str beg-pos end-pos))
                    ((eq key 'C)
                     (list (sgf-io--prettify-text val-str)))
                    ((eq key 'PL)
                     (cond ((string= val-str "B") '(B))
                           ((string= val-str "W") '(W))
                           (t (error "%sInvalid player value: %s (expected B or W)."
                                     (sgf-io--format-location beg-pos end-pos) val-str))))
                    ((eq key 'SZ)
                     (list (sgf-decode-prop-SZ val-str beg-pos end-pos)))
                    ((eq key 'GM)
                     (if (string= val-str "1")
                         (list val-str)
                       (error "%sGame type is not Go: %s (expected GM[1])."
                              (sgf-io--format-location beg-pos end-pos) val-str)))
                    ((eq key 'MN)
                     (list (string-to-number val-str)))
                    ((eq key 'LB)
                     (list (sgf-decode-prop-LB val-str)))
                    (t (list val-str)))))
        (nconc prop prop-val)))
    prop))


(defun sgf-decode-prop-SZ (val &optional beg end)
  "Process the SZ property from an SGF file and return a cons cell of board width and height."
  (let* ((nums (string-split val ":" t))
         (width (car nums))
         (height (or (cadr nums) width))
         (w (string-to-number width))
         (h (string-to-number height)))
    (if (and (>= w 1) (<= w 52) (>= h 1) (<= h 52))
        (cons w h)
      (error "%sInvalid board size (w=%S h=%S) (%s)"
             (sgf-io--format-location beg end) w h val))))


(defun sgf-decode-prop-LB (val &optional beg end)
  "for property LB: label a position or stone. e.g. LB[ee:label]"
  (if (not (equal (elt val 2) ?:))
      (error "%sInvalid label property value (%s)."
             (sgf-io--format-location beg end) val))
  (let* ((pos (substring val 0 2))
         (label (substring val 3)))
    (cons (sgf-decode--prop-pos pos) label)))


(defun sgf-decode--prop-pos (val &optional beg end)
  "Process a 2-letter position property from an SGF file and return a cons cell of x and y."
  (if (/= (length val) 2)
      (error "%sPosition value needs to be 2 letters (%s)."
             (sgf-io--format-location beg end) val))
  (let* ((x (sgf-decode-c2d (elt val 0)))
         (y (sgf-decode-c2d (elt val 1))))
    (if (and (>= x 0) (<= x 51) (>= y 0) (<= y 51))
        (cons x y)
      (error "%sPosition value is out of range (x=%d y=%d) (%s)."
             (sgf-io--format-location beg end) x y val))))


(defun sgf-decode-prop-pos (val &optional beg end)
  "Convert a single 2-letter or compressed position string (eg `aa:ac') into a list of numeric pairs.

https://www.red-bean.com/sgf/sgf4.html#3.5.1

See also `sgf-encode-prop-pos'."
  (let* ((positions (string-split val ":" t))
         (count (length positions)))
    (cond ((= count 0) nil)   ; no position letter exists - a pass. return nil
          ((= count 1)
           (list (sgf-decode--prop-pos val beg end)))
          ((= count 2)
           (let* ((tl (sgf-decode--prop-pos (car positions) beg end)) ; top left
                  (br (sgf-decode--prop-pos (cadr positions) beg end)); bottom right
                  (tl-x (car tl)) (tl-y (cdr tl))
                  (br-x (car br)) (br-y (cdr br))
                  (y-seq (number-sequence tl-y br-y))
                  (x-seq (number-sequence tl-x br-x)))
             (apply 'append
                    (mapcar (lambda (x)
                              (mapcar (lambda (y) (cons x y)) y-seq))
                            x-seq))))
          (t (error "%sOnly zero or one colon is allowed in compressed position value (%s)."
                    (sgf-io--format-location beg end) val)))))


(defun sgf-decode-c2d (c)
  "Convert a letter to an integer.

?a-?z ?A-?Z to 0-25 26-51"
  (cond
   ((and (>= c ?a) (<= c ?z)) (- c ?a))
   ((and (>= c ?A) (<= c ?Z)) (+ (- c ?A) 26))
   (t (error "Invalid position letter '%c'." c))))


(defun sgf-encode-d2c (n)
  "See also `sgf-decode-c2d'."
  (cond
   ((and (>= n 0) (<= n 25)) (+ ?a n))
   ((and (>= n 26) (<= n 51)) (+ ?A (- n 26)))
   (t (error "Invalid position number '%d'." n))))


(defun sgf-io--format-location (begin end)
  "Return a string indicating buffer location."
  (cond ((and begin end) (format "%s-%s: " begin end))
        (begin (format "%s: " begin))
        (end (format "%s: " end))
        (t "")))


(defun sgf-io--prettify-text (txt)
  "Format and prettify text like comment string."
  ;; Delete escaped line-break
  (setq txt (replace-regexp-in-string "\\\\\\(\n\r?\\|\r\n?\\)" "" txt))
  ;; Convert escaped text back into its raw form.
  ;; eg "This is a \\n test \\t string." -> "This is a n test t string."
  (setq txt (replace-regexp-in-string "\\\\\\(.\\)" "\\1" txt))
  ;; Convert Spaces
  (setq txt (replace-regexp-in-string "\t\\|\v\\|\n\r?\\|\r\n?" " " txt))
  txt)


(defun sgf-io--escape-text (txt)
  ;; see: https://www.red-bean.com/sgf/sgf4.html#text
  ;; (replace-regexp-in-string "\\([]:\\\\]\\)" "\\\\\\1" text) ;;escape :
  (replace-regexp-in-string "\\([]\\\\]\\)" "\\\\\\1" txt))


;; TODO remove quote for str values and escape [ ] etc
(defun sgf-encode-prop (prop)
  "Convert to a string of SGF property."
  (let ((prop-key (car prop))
        (prop-vals (cdr prop))
        prop-val-str)
    (if (null prop-vals)
        ;; if prop-vals is nil (eg a pass move), put [] as the value.
        (setq prop-val-str "[]")
      (setq prop-val-str
            (cond ((memq prop-key '(B W AB AW TR CR MA SQ))
                   (sgf-encode-prop-pos prop-vals))
                  ((eq prop-key 'LB)
                   (mapconcat (lambda (prop-val)
                                (format "[%c%c:%s]"
                                        (sgf-encode-d2c (car (car prop-val)))
                                        (sgf-encode-d2c (cdr (car prop-val)))
                                        (sgf-io--escape-text (cdr prop-val))))
                              prop-vals))
                  ((eq prop-key 'SZ)
                   (let ((x (caar prop-vals))
                         (y (cdar prop-vals)))
                     (if (= x y) (format "[%d]" x) (format "[%d:%d]" x y))))
                  ((eq prop-key 'C)
                   (format "[%s]" (sgf-io--escape-text (car prop-vals))))
                  (t (format "[%s]" (car prop-vals))))))
    (format "%S%s" prop-key prop-val-str)))


(defun sgf-encode-prop-pos (xys)
  "Encode a sequence of position(s) into SGF format.

See also `sgf-io-from-positions'."
  (mapconcat
   (lambda (rect)
     (let ((lt (string (sgf-encode-d2c (caar rect))
                       (sgf-encode-d2c (cdar rect))))  ;left-top
           (rb (string (sgf-encode-d2c (cadr rect))
                       (sgf-encode-d2c (cddr rect))))) ;right-bottom
       (if (string= lt rb)
           (concat "[" lt "]")
         (concat "[" lt ":" rb "]"))))
   (sgf-io-rows-to-rects (sgf-io-xys-to-rows xys))
   ""))


(defun sgf-io-xys-to-rows (xys)
  "Convert a list of (x . y) coordinates to a list of rectangle blocks.
Each rectangle is represented as ((left . top) . (right . bottom)),
where all positions in the rectangle are filled in coords."
  ;; Sort the coordinates by y then x
  (let ((sorted-xys (sort xys :key (lambda (xy) (list (cdr xy) (car xy)))))
        rows)
    (while sorted-xys
      (let* ((start (car sorted-xys))
             (end start)
             (current-line (cdr start)))
        ;; Process continuous x positions on the same line
        (setq sorted-xys (cdr sorted-xys))
        (while (and sorted-xys
                    (= (cdr (car sorted-xys)) current-line)
                    (= (car (car sorted-xys)) (1+ (car end))))
          (setq end (car sorted-xys))
          (setq sorted-xys (cdr sorted-xys)))
        ;; Create a row from the collected positions
        (push (cons start end) rows)))
    rows))

(defun sgf-io-rows-to-rects (rows)
  "Convert a list of rows to a list of rectangle blocks."
  (let ((sorted-rows (sort rows))
        rects)
    (while sorted-rows
      (let* ((start-row (car sorted-rows)) ; 1st row
             (end-row start-row)
             (lt-x (caar start-row)) ; x of left top position
             (lt-y (cdar start-row)) ; y of left top position
             (n 1) ; number of rows in this rect
             (row-width (- (cadr start-row) lt-x)))
        ;; Process continuous y positions
        (setq sorted-rows (cdr sorted-rows))
        (while (and sorted-rows
                    ;; Check if the next row is left aligned
                    (= (caar (car sorted-rows)) lt-x)
                    ;; check if the next row is immediately below the current row
                    (= (cdar (car sorted-rows)) (+ n lt-y))
                    ;; check if the next row has the same width
                    (= (- (cadr (car sorted-rows)) lt-x) row-width))
          (setq end-row (car sorted-rows))
          (setq n (1+ n))  ; increment the number of rows in the rect
          (setq sorted-rows (cdr sorted-rows)))
        ;; Create a rectangle from the collected rows
        (push (cons (car start-row) (cdr end-row)) rects)))
    (nreverse rects)))


(defun sgf-encode-node (node)
  "Convert a node to an SGF string."
  (concat ";" (mapconcat (lambda (prop) (sgf-encode-prop prop)) node)))


(defun sgf-serialize-lnode (lnode)
  "Convert a game tree starting from LNODE to an SGF string."
  (let ((stack '()) (output '())
        (indent 0))
    ;; Push the root node onto the stack
    (push (list lnode 0 nil) stack)

    (while stack
      (let* ((item (pop stack))
             (node (nth 0 item))
             (child-index (nth 1 item))
             (is-branch (nth 2 item))
             (next-lnodes (aref node 2))
             (n (length next-lnodes)))

        ;; Handle the current node
        (when (= child-index 0)
          (when is-branch
            (setq indent (1+ indent))
            (push (concat "\n" (make-string indent ?\s) "(") output))
          (push (sgf-encode-node (aref node 1)) output))

        ;; Process the children
        (if (< child-index n)
            (progn
              ;; Increment child index and push the current node back
              (push (list node (1+ child-index) is-branch) stack)
              ;; Push the next child node onto the stack
              (push (list (nth child-index next-lnodes) 0 (> n 1)) stack))
          ;; No more children, close the branch if needed
          (when is-branch
            (setq indent (1- indent))
            (push ")" output)))))

    ;; Return the final result as a concatenated string
     (apply #'concat (nreverse output))))


(defun sgf-serialize-game-to-str (lnode)
  "Serialize the whole game to a string."
  ;; Move to the root node
  (while (aref lnode 0) (setq lnode (aref lnode 0)))
  (format "(%s)" (sgf-serialize-lnode lnode)))


(defun sgf-serialize-game-to-str-no-variation ()
  "Output the SGF string for each game variation after the LNODE.

It keeps the moves leading to the LNODE and ignore any other variations
before that. See also `sgf-remove-variations'. This function is normally
not used and kept for reference."
  (interactive)
  (let* ((ov (sgf-get-overlay))
         (game-state (overlay-get ov 'game-state))
         (curr-lnode (aref game-state 0))
         (output '())
         (prev-lnode (aref curr-lnode 0)))
    ;; serialize the nodes before lnode
    (while (not (sgf-root-p prev-lnode))
      (push (sgf-encode-node (aref prev-lnode 1)) output)
      (setq prev-lnode (aref prev-lnode 0)))
    (push (sgf-encode-node (aref prev-lnode 1)) output)
    (message "(%s\n%s)" (apply #'concat output) (sgf-serialize-lnode curr-lnode))))


(defun sgf-serialize-game-to-buffer (&optional ov)
  "Update the buffer region with the SGF string representation of game.

If OV is nil, it will use the overlay at point."
  (interactive "i")
  (let* ((ov (or ov (sgf-get-overlay)))
         (buffer (overlay-buffer ov))
         (beg (overlay-start ov))
         (end (overlay-end ov))
         (game-state (overlay-get ov 'game-state))
         (lnode (aref game-state 0))
         ;; disable all modification hooks including the overlay
         ;; modification hooks temporarily.
         (inhibit-modification-hooks t)
         (inhibit-read-only t))
    (with-current-buffer buffer
      ;; (setq buffer-read-only nil)
      (save-excursion
        ;; Wraps the operation in undo-boundary calls to ensure it's
        ;; treated as a distinct unit in the undo list
        (undo-boundary)
        ;; group the delete and insert operations into a single undo unit.
        (combine-change-calls beg end
          (delete-region beg end)
          (insert (sgf-serialize-game-to-str lnode))
          (insert "\n"))
        (undo-boundary)))))


(provide 'sgf-io)
;;; sgf-io.el ends here
