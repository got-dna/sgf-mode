;;; sgf-io.el --- sgf IO   -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides
;; 1. functions to parse SGF content into a syntax tree
;; 2. functions to process syntax tree further into amenable list of emacs object.
;; 3. functions to serialize game object into SGF string and write out to file.

;; SGF format specs:
;; https://homepages.cwi.nl/~aeb/go/misc/sgf.html#contents

;;; Code:


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
  (let (ch wss)
    (while (sgf-parse-ws-p (char-after))
      (forward-char))))

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


;; Game Info Properties

(defconst sgf-parse-game-info-properties
  '(("CP" "Copyright")
    ("US" "Enterer Name")
    ("AN" "Annotator Name")
    ("SO" "Source")
    ("EV" "Event Name")
    ("GN" "Game Name")
    ("RO" "Round Number")
    ("DT" "Date")
    ("PC" "Place")
    ("BT" "Black Team" :player black)
    ("PB" "Black Player" :player black)
    ("BR" "Black Rank" :player black)
    ("WT" "White Team" :player white)
    ("PW" "White Player" :player white)
    ("WR" "White Rank" :player white)
    ("RU" "Rule")
    ("OT" "Overtime Method")
    ("TM" "Time Limit" :type real)
    ("HA" "Handicap Stones" :type number)
    ("KM" "Komi" :type real)
    ("RE" "Result")
    ("ON" "Opening Moves")
    ("GC" "Comment" :type text)))


(defun sgf-parse-game-info-prop-type (prop)
  (or (plist-get (cddr prop) :type) 'simpletext))


(defun sgf-parse-buffer (beg end)
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


;;;###autoload
(defun sgf-parse-file-pop (file name)
  "Display in a popup buffer the pretty printed Emacs Lisp object of syntax tree converted from
the SGF content of FILE."
  (interactive "fRead SGF file: \nBOutput to buffer name: ")
  (sgf-io-read-file file 'sgf-parse-buffer name))  ;; Call the helper with the parsing function


(defun sgf-io-read-file (file process-fn &optional name)
  "Read SGF content from FILE, process it with PROCESS-FN, and optionally
output to and pop up a buffer named NAME. the result of PROCESS-FN."
  (let ((result
         (with-temp-buffer
           (insert-file-contents-literally file)
           (funcall process-fn (point-min) (point-max))))) ;; Process the buffer
    (if name
        (let ((buffer (generate-new-buffer name)))
          (with-current-buffer buffer
            (insert (pp-to-string result))
            (emacs-lisp-mode))  ;; Enable emacs-lisp-mode for syntax highlighting
          (pop-to-buffer buffer)))
    result))




;;; process syntax tree into linked node, board, and starting game state

;;;###autoload
(defun sgf-game-from-file-pop (file name)
  "Display in a popup buffer the pretty printed Emacs Lisp object of game
state converted from the SGF content of FILE."
  (interactive "fRead SGF file: \nsOutput to buffer name: ")
  (sgf-io-read-file file 'sgf-game-from-buffer name))


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
         (board-2d (sgf-game-board-create w h 'E))
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
            (sgf-game-board-set xy setup-stone board-2d))))))


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
             (setq curr-node (sgf-io-from-node item))
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


(defun sgf-io-from-node (sgf-node)
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
                     (if (string= val-str "1")
                         (list val-str)
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
  (let* ((nums (string-split val ":" t))
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
  (let* ((positions (string-split val ":" t))
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


(defun sgf-game--letter-to-number (c)
  "Convert a letter to an integer.

?a-?z ?A-?Z to 0-25 26-51"
  (cond
   ((and (>= c ?a) (<= c ?z)) (- c ?a))
   ((and (>= c ?A) (<= c ?Z)) (+ (- c ?A) 26))
   (t (error "Invalid position letter '%c'." c))))


(defun sgf-game--number-to-letter (n)
  "See also `sgf-game--letter-to-number'."
  (cond
   ((and (>= n 0) (<= n 25)) (+ ?a n))
   ((and (>= n 26) (<= n 51)) (+ ?A (- n 26)))
   (t (error "Invalid position number '%d'." n))))


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


(defun sgf-game-board-create (w h &optional default)
  "Create a empty 2D board of size WxH with DEFAULT value."
  (let ((board-2d (make-vector h nil)))
    (dotimes (i h) ;; for each row
      (aset board-2d i (make-vector w default)))
    board-2d))


(defun sgf-game-board-clear (board-2d)
  "Clear 2D board"
  (dotimes (i (length board-2d))       ;; Loop over rows
      (dotimes (j (length (aref board-2d i)))  ;; Loop over columns in each row
        (sgf-game-board-set (cons i j) 'E board-2d))))  ;; Set each cell to 'E'


(defun sgf-game-board-get (xy board-2d)
  (if (consp xy)
      (aref (aref board-2d (cdr xy)) (car xy))))


(defun sgf-game-board-set (xy v board-2d)
  "Do nothing if xy is nil"
  (if (consp xy)
      (aset (aref board-2d (cdr xy)) (car xy) v)))


(defun sgf-game-board-hoshi (w h)
  "Return a list of hoshi positions on a board of size WxH."
  (append
   ;; center position
   (if (and (= (logand w 1) 1) (= (logand h 1) 1)) (list (cons (/ (1- w) 2)  (/ (1- h) 2))))
   ;; 4 corners
   (if (and (> w 12) (> h 12))
       (list (cons 3       3)
             (cons (- w 4) 3)
             (cons 3       (- h 4))
             (cons (- w 4) (- h 4))))
   ;; 4 sides
   (if (and (> w 18) (> h 18) (= (logand h 1) 1))
       (list (cons 3       (/ (1- h) 2))
             (cons (- w 4) (/ (1- h) 2))))
   (if (and (> w 18) (> h 18) (= (logand w 1) 1))
       (list (cons (/ (1- w) 2) 3)
             (cons (/ (1- w) 2) (- h 4))))))



;; TODO remove quote for str values and escape [ ] etc
(defun sgf-write-prop-to-str (prop)
  "Convert a property to an SGF string.

(sgf-write-prop-to-str '(B (0 . 0) (1 . 0))) => B[aa:ba]
(sgf-write-prop-to-str '(W)) => W[]
(sgf-write-prop-to-str '(TR (0 . 0))) => TR[aa]
(sgf-write-prop-to-str '(FF 4)) => FF[4]
(sgf-write-prop-to-str '(AB (1 . 1) (1 . 2))) => AB[bb:bc]
(sgf-write-prop-to-str '(SZ (15 . 13))) => SZ[15:13]
(sgf-write-prop-to-str '(LB ((0 . 27) . \"label\"))) => LB[aB:label]
(sgf-write-prop-to-str '(C \"comment\")) => C[comment]"
  (let ((prop-key (car prop))
        (prop-vals (cdr prop))
        prop-val-str)
    (if (null prop-vals)
        ;; if prop-vals is nil (eg a pass move), put [] as the value.
        (setq prop-val-str "[]")
      (setq prop-val-str
            (cond ((memq prop-key '(B W AB AW TR CR MA SQ))
                   (sgf-write-compressed-positions prop-vals))
                  ((eq prop-key 'LB)
                   (mapconcat (lambda (prop-val)
                                (format "[%c%c:%s]"
                                        (sgf-game--number-to-letter (car (car prop-val)))
                                        (sgf-game--number-to-letter (cdr (car prop-val)))
                                        (sgf-write-escape-text (cdr prop-val))))
                                prop-vals))
                  ((eq prop-key 'SZ)
                   (let ((x (caar prop-vals))
                         (y (cdar prop-vals)))
                     (if (= x y) (format "[%d]" x) (format "[%d:%d]" x y))))
                  ((eq prop-key 'C)
                   (format "[%s]" (sgf-write-escape-text (car prop-vals))))
                  (t (format "[%s]" (car prop-vals))))))
    (format "%S%s" prop-key prop-val-str)))


(defun sgf-write-escape-text (text)
  ;; see: https://www.red-bean.com/sgf/sgf4.html#text
  ;; (replace-regexp-in-string "\\([]:\\\\]\\)" "\\\\\\1" text) ;;escape :
  (replace-regexp-in-string "\\([]\\\\]\\)" "\\\\\\1" text))


(defun sgf-write-to-positions (xys)
  "Compress a sequence of positions.

For example:
(sgf-write-compressed-positions '((0 . 0) (1 . 0) (3 . 0) (4 . 0) (0 . 1) (1 . 1)))
(sgf-write-compressed-positions '((0 . 0) (1 . 0)))
(sgf-write-compressed-positions '((0 . 0)))"
  (mapconcat
   (lambda (rect)
     (let ((lt (string (sgf-game--number-to-letter (caar rect))
                       (sgf-game--number-to-letter (cdar rect))));;left-top
           (rb (string (sgf-game--number-to-letter (cadr rect))
                       (sgf-game--number-to-letter (cddr rect)))));;right-bottom
       (if (string= lt rb)
           (concat "[" lt "]")
         (concat "[" lt ":" rb "]"))))
   (sgf-write-rows-to-rects (sgf-write-xys-to-rows xys))
   ""))


(defun sgf-write-xys-to-rows (xys)
  "Convert a list of (x . y) coordinates to a list of rectangle blocks.
Each rectangle is represented as ((left . top) . (right . bottom)),
where all positions in the rectangle are filled in coords.

For example:
(sgf-write-xys-to-rows '((1 . 1) (1 . 2)))
(sgf-write-xys-to-rows '((0 . 0) (1 . 0) (3 . 0) (4 . 0) (0 . 1) (1 . 1)))
 => (((0 . 1) . (1 . 1))
     ((3 . 0) . (4 . 0))
     ((0 . 0) . (1 . 0)))"
  ;; Sort the coordinates by y then x"
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

(defun sgf-write-rows-to-rects (rows)
  "Convert a list of rows to a list of rectangle blocks.

For example:
(sgf-write-rows-to-rects '(((1 . 2) . (1 . 2))
                           ((1 . 1) . (1 . 1)))) =>
 (((1 . 1) . (1 . 2)))

(sgf-write-rows-to-rects '(((0 . 1) . (1 . 1))
                           ((3 . 0) . (4 . 0))
                           ((0 . 0) . (1 . 0)))) =>
 (((0 . 0) . (1 . 1))
  ((3 . 0) . (4 . 0)))"
  (let ((sorted-rows (sort rows))
        rects)
    (while sorted-rows
      (let* ((start-row (car sorted-rows))
             (end-row start-row)
             (left-y (cdar start-row))
             (left-x (caar start-row))
             (row-size (- (cadr start-row) left-x)))
        ;; Process continuous y positions
        (setq sorted-rows (cdr sorted-rows))
        (while (and sorted-rows
                    ;; Check if the next row is left aligned
                    (= (caar (car sorted-rows)) left-x)
                    ;; check if the next row is immediately below the current row
                    (= (cdar (car sorted-rows)) (1+ left-y))
                    ;; check if the next row has the same width
                    (= (- (cadr (car sorted-rows)) left-x) row-size))
          (setq end-row (car sorted-rows))
          (setq sorted-rows (cdr sorted-rows)))
        ;; Create a rectangle from the collected rows
        (push (cons (car start-row) (cdr end-row)) rects)))
    (nreverse rects)))


(defun sgf-write-node-to-str (node)
  "Convert a node to an SGF string.

(sgf-write-node-to-str '((FF 4) (SZ (15 . 13)) (AB (1 . 1) (1 . 2)))) => ;FF[4]SZ[15:13]AB[bb][bc]"
  (concat ";" (mapconcat (lambda (prop) (sgf-write-prop-to-str prop)) node)))


(defun sgf-write-game-to-str (&optional lnode)
  "Convert a game tree starting from LNODE to an SGF string."
  ;; (sgf-write-game-to-str (aref (overlay-get (sgf-get-overlay) 'game-state) 0))
  (let ((curr-lnode lnode)
        (next-lnodes (aref lnode 2))
        (node-str (sgf-write-node-to-str (aref lnode 1))))
    (if (null next-lnodes)
        node-str
      (let ((next-strs (mapcar #'sgf-write-game-tree-to-str next-lnodes)))
        (if (= (length next-lnodes) 1)
            ;; No fork, just append the next node string
            (concat node-str (car next-strs))
          ;; Fork, wrap each branch in parentheses
          (concat node-str "(" (mapconcat #'identity next-strs ")(") ")"))))))


(defun sgf-write-game-to-buffer (lnode &optional buffer beg end)
  "Update the buffer region with the SGF string representation of game."
  ;; move to the root node
  (while (setq lnode (aref lnode 0)))
  (let ((sgf-str (sgf-write-game-tree-to-str lnode)))
    (with-current-buffer (or buffer (current-buffer))
      (delete-region (or beg (point-min)) (or end (point-max)))
      (insert "(" sgf-str ")"))))


(provide 'sgf-write)
;;; sgf-write.el ends here

(provide 'sgf-io)
;;; sgf-io.el ends here
