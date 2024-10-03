;;; sgf-parse.el --- parse SGF into syntax tree   -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides functions to parse SGF content into a syntax tree.
;; SGF format specs:
;; https://homepages.cwi.nl/~aeb/go/misc/sgf.html#contents
;;; Code:


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


(defun sgf-parse-file (file process-fn &optional name)
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


(defun sgf-parse-file-pop (file name)
  "Display in a popup buffer the pretty printed Emacs Lisp object of syntax tree converted from
the SGF content of FILE."
  (interactive "fRead SGF file: \nsOutput to buffer name: ")
  (sgf-parse-file file 'sgf-parse-buffer name))  ;; Call the helper with the parsing function


(provide 'sgf-parse)
;;; sgf-parser.el ends here
