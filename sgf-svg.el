;;; sgf-svg.el --- svg visualization  -*- lexical-binding: t; -*-

;; Author: Zech Xu
;; Version: version
;; Package-Requires: ((svg "0.5.1") (emacs "29.4"))
;; Homepage: homepage
;; Keywords: svg, go, game

;;; Commentary:
;; SVG rendering of Go board and game moves.

;;; Code:

(require 'svg)
(require 'sgf-util)

(defcustom sgf-svg-interval 25
  "Default pixels for the size of grid cells.
It is a reference for all other element sizes."
  :type '(number)
  :group 'sgf-svg)

(defcustom sgf-svg-margin 30
  "Default pixels for the margin of the board."
  :type '(number)
  :group 'sgf-svg)

(defcustom sgf-svg-bar 25
  "Default pixels for the bar height of the board."
  :type '(number)
  :group 'sgf-svg)

(defcustom sgf-svg-padding 5
  "Default padding for the board. Used in buttons and other elements."
  :type '(number)
  :group 'sgf-svg)


(defcustom sgf-svg-font-size  (* sgf-svg-interval 0.6)
  "Default font size for the board."
  :type '(number)
  :group 'sgf-svg)

(defcustom sgf-svg-font-family "Arial"
  "Default font family for the board."
  :type '(string)
  :group 'sgf-svg)

(defvar sgf-svg--node-id-stones "stones"
  "The id of the group node for stones on the board.")

(defvar sgf-svg--node-id-mvnums "mvnums"
  "The id of the group node for move numbers on the board.")

(defvar sgf-svg--node-id-nexts "nexts"
  "The id of the group node for next moves on the board.")

(defvar sgf-svg--node-id-marks "marks"
  "The id of the group node for marks on the board.")


(defun sgf-svg-stone-gradient (svg color stops)
  "Create stone gradients"
  (let* ((defs (svg-node svg 'defs)) ; Create the <defs> node
         ;; Create the stone gradient
         (gradient (svg-node defs 'radialGradient :id color
                             :r "0.6" :fy "0.3" :fx "0.7" :cy "0.5" :cx "0.5")))
    (mapc (lambda (stop)
            (svg-node gradient 'stop :offset (car stop) :stop-color (cdr stop)))
          stops)))


(defun sgf-svg-init (w h)
  (let* ((grid-w (* sgf-svg-interval (1- w)))
         (grid-h (* sgf-svg-interval (1- h)))
         (board-w (+ sgf-svg-margin grid-w sgf-svg-margin))
         (board-h (+ sgf-svg-margin grid-h sgf-svg-margin))
         (line-width 0.5)
         (star-radius 2)
         (idx-font-scale 0.7)
         (hot-grid-t-l (cons sgf-svg-margin (+ sgf-svg-bar sgf-svg-margin)))
         (hot-grid-b-r (cons (+ sgf-svg-margin (* (1- w) sgf-svg-interval))
                             (+ sgf-svg-bar sgf-svg-margin (* (1- h) sgf-svg-interval))))
         (hot-areas (list (list (cons 'rect (cons hot-grid-t-l hot-grid-b-r))
                                'hot-grid (list 'pointer 'hand))))
         svg status-bar board grid menu-bar ; svg nodes
         idx)
    ;; Note that the order of svg elements matters
    (setq svg (svg-create board-w (+ sgf-svg-bar board-h sgf-svg-bar)
                          :text-anchor "middle"
                          :font-family sgf-svg-font-family))

    ;; Stones' Gradient
    (sgf-svg-stone-gradient svg "B" '((0 . "#606060") (100 . "#000000")))
    (sgf-svg-stone-gradient svg "W" '((0 . "#ffffff") (100 . "#b0b0b0")))

    ;; Menu Bar at top
    (setq menu-bar (svg-node svg 'g :id "menu-bar" :fill "gray"))

    ;; (svg-rectangle menu-bar 0 menu-bar-y board-w sgf-svg-bar :fill "gray")
    (nconc hot-areas (sgf-svg-create-menu-buttons menu-bar))

    ;; Board Rect
    (setq board (svg-node svg 'g
                          :id "game-board"
                          :transform (format "translate(%s, %s)" 0 sgf-svg-bar)
                          :fill "black"))
    (svg-rectangle board 0 0 board-w board-h :fill "#e3aa4e")
    ;; Board Grid
    (setq grid (svg-node board 'g
                         :id "game-grid"
                         :font-family sgf-svg-font-family
                         :transform (format "translate(%s, %s)" sgf-svg-margin sgf-svg-margin)))
    ;; Grid Lines
    (dotimes (n w)
      ;; vertical lines
      (setq idx (format "%c" (if (< n (- ?I ?A)) (+ ?A n) (+ ?A n 1)))) ; skip char I
      ;; (setq idx (format "%c" (sgf-encode-d2c n)))
      (svg-text grid idx :class "grid-idx"
                :font-size (* sgf-svg-font-size idx-font-scale)
                :x (* sgf-svg-interval n) :y (- sgf-svg-font-size)
                :dominant-baseline "hanging")
      (svg-text grid idx :class "grid-idx"
                :font-size (* sgf-svg-font-size idx-font-scale)
                :x (* sgf-svg-interval n) :y (* sgf-svg-interval h))
      (svg-line grid (* sgf-svg-interval n) 0 (* sgf-svg-interval n) (* grid-h)
                :stroke "black" :stroke-width line-width))
    (dotimes (n h)
      ;; horizontal lines
      (setq idx (number-to-string (- h n)))
      (svg-text grid idx :class "grid-idx"
                :font-size (* sgf-svg-font-size idx-font-scale)
                :x (- sgf-svg-font-size) :y (* sgf-svg-interval n)
                :text-anchor "end" :dy ".25em")
      (svg-text grid idx :class "grid-idx"
                :font-size (* sgf-svg-font-size idx-font-scale)
                :x (+ grid-w sgf-svg-font-size) :y (* sgf-svg-interval n)
                :text-anchor "start" :dy ".25em")
      (svg-line grid 0 (* sgf-svg-interval n) grid-w (* sgf-svg-interval n)
                :stroke "black" :stroke-width line-width))

    ;; Hoshi/Stars
    (dolist (hoshi (sgf-board-hoshi w h))
      (svg-circle grid
                  (* sgf-svg-interval (car hoshi))
                  (* sgf-svg-interval (cdr hoshi))
                  star-radius))

    ;; Layers: different types of information on board
    (svg-node grid 'g :id sgf-svg--node-id-stones)
    (svg-node grid 'g :id sgf-svg--node-id-mvnums
              :font-size sgf-svg-font-size
              :font-weight "bold")
    (svg-node grid 'g :id sgf-svg--node-id-nexts
              :font-size sgf-svg-font-size
              :font-weight "bold")
    (svg-node grid 'g :id sgf-svg--node-id-marks
              :font-size sgf-svg-font-size
              :font-weight "bold")

    ;; Status Bar
    (setq status-bar (svg-node svg 'g
                               :id "status-bar"
                               :font-weight "bold" :font-family sgf-svg-font-family
                               :transform
                               (format "translate(%s, %s)" 0 (+ sgf-svg-bar board-h))))
    (svg-rectangle status-bar 0 0 board-w sgf-svg-bar :fill "gray")
    (svg-circle status-bar (/ sgf-svg-interval 2)
                (/ sgf-svg-interval 2) (/ sgf-svg-interval 3) :gradient "B")
    (svg-circle status-bar (- board-w (/ sgf-svg-interval 2))
                (/ sgf-svg-interval 2) (/ sgf-svg-interval 3) :gradient "W")
    ;; show what play turn it is
    (svg-rectangle status-bar 0 (- sgf-svg-bar sgf-svg-padding)
                   sgf-svg-interval sgf-svg-padding
                   :id "status-b" :fill "gray")
    (svg-rectangle status-bar (- board-w sgf-svg-interval) (- sgf-svg-bar sgf-svg-padding)
                   sgf-svg-interval sgf-svg-padding
                   :id "status-w" :fill "gray")
    ;; show move number
    (svg-text status-bar "0" :x (/ board-w 2) :y sgf-svg-bar
              :id "status-n" :fill "white"
              :dy "-0.5em")
    ;; show prisoner number
    (svg-text status-bar "0" :x (* sgf-svg-interval 2) :y sgf-svg-bar
              :id "status-pb" :fill "white"
              :dy "-0.5em")
    (svg-text status-bar "0" :x (- board-w sgf-svg-interval sgf-svg-interval) :y sgf-svg-bar
              :id "status-pw" :fill "white"
              :dy "-0.5em")

    (cons svg hot-areas)))


(defun sgf-svg-create-menu-buttons (menu-bar)
  "Create all the buttons to the svg node MENU-BAR and return the hot areas for the buttons.

SGF-SVG-BAR is the starting y coordinate for menu bar."
  (let ((btns '((hot-menu "Menu" "menu")
                (hot-first "|<" "Move to the beginning of the game")
                (hot-backward "<" "Move backward")
                (hot-forward ">" "Move forward")
                (hot-last ">|" "Move to the end of the game")
                (hot-del "Del" "Delete the current move")
                (hot-pass "Pass" "Pass the current move")))
        (x sgf-svg-padding) (y sgf-svg-padding)
        (height (- sgf-svg-bar (* 2 sgf-svg-padding))))

    (mapcar (lambda (btn)
              (let* ((hot-area-id (nth 0 btn))
                     (name        (nth 1 btn))
                     (tooltip     (nth 2 btn))
                     (width (+ (* 2 sgf-svg-padding) (string-pixel-width name)))
                     hot-area)
                (svg-rectangle menu-bar x y width height :rx 3 :ry 3 :fill "#fff")
                (svg-text menu-bar name
                          :x (+ x (/ width 2)) :y height
                          :fill "#000")
                (setq hot-area (list (cons 'rect
                                           (cons (cons x 0)
                                                 (cons (+ x width)
                                                       sgf-svg-bar )))
                                     hot-area-id (list 'pointer 'hand
                                                       'help-echo tooltip)))
                (setq x (+ x width sgf-svg-padding))
                hot-area))
            btns)))


(defun sgf-svg-set-color (xy-state)
  "Set mark/text color according to background color of the intersections on board."
  (cond ((equal xy-state 'B) "white")
        ((equal xy-state 'W) "black")
        ((equal xy-state 'E) "white")
        (t "white")))


(defun sgf-svg-update-status-turn (svg stone)
  "Update the current turn on the status bar."
  (let ((status-w (car (dom-by-id svg "^status-w$")))
        (status-b (car (dom-by-id svg "^status-b$"))))
    (cond ((equal stone 'W)
           (dom-set-attribute status-w 'fill "#f00")
           (dom-set-attribute status-b 'fill "gray"))
          ((equal stone 'B)
           (dom-set-attribute status-w 'fill "gray")
           (dom-set-attribute status-b 'fill "#f00"))
          (t (error "Invalid stone color %s" stone)))))


(defun sgf-svg-update-status-mvnum (svg mvnum)
  "Update the current move number on the status bar."
  (let ((status-n (car (dom-by-id svg "^status-n$"))))
    ;; update the svg element text
    (setcar (nthcdr 2 status-n) (number-to-string mvnum))))


(defun sgf-svg-update-status-prisoners (svg prisoners)
  "Show the current prisoners on the status bar.
PRISONERS is a cons cell of black and white prisoner counts."
  (let ((status-pb (car (dom-by-id svg "^status-pb$")))
        (status-pw (car (dom-by-id svg "^status-pw$")))
        (pb (car prisoners))
        (pw (cdr prisoners)))
    (setcar (nthcdr 2 status-pb) (number-to-string pb))
    (setcar (nthcdr 2 status-pw) (number-to-string pw))))


(defun sgf-svg-add-marks (svg node board-2d)
  "Process and update the marks on the board for a node.

It removes the old marks and adds the new marks."
  ;; make sure to remove old marks
  (let ((marks-group (sgf-svg-group-marks svg))
        type)
    (sgf-svg-clear-node-content marks-group)
    (dolist (prop node)
      (setq type (car prop))
      (if (member type '(SQ TR CR MA))
          (dolist (xy (cdr prop))
            (let* ((x (car xy))
                   (y (cdr xy))
                   (xy-state (sgf-board-get xy board-2d))
                   (color (sgf-svg-set-color xy-state)))
              (sgf-svg-add-mark type marks-group x y color))))
      (if (equal type 'LB)
          (dolist (prop-val (cdr prop))
            (let* ((label (cdr prop-val))
                   (xy (car prop-val))
                   (x (car xy))
                   (y (cdr xy))
                   (xy-state (sgf-board-get xy board-2d))
                   (color (sgf-svg-set-color xy-state)))
              ;; (message "---------%S %S" xy-state color)
              (sgf-svg-add-text marks-group x y label color)))))))


(defun sgf-svg-add-nexts (svg curr-lnode)
  "Update and show branches/next move(s) on board svg."
  (let* ((next-lnodes (aref curr-lnode 2))
         (branch-count (length next-lnodes))
         (branch-index 0)
         (next-group (sgf-svg-group-next svg))
         text play stone xy x y)
    (sgf-svg-clear-node-content next-group)
    (dolist (next-lnode next-lnodes)
      (if (= branch-count 1)
          (setq text "x")
        (setq text (string (+ ?a branch-index))))
      (setq play (sgf-process-move (aref next-lnode 1))
            stone (car play) xy (cdr play) x (car xy) y (cdr xy))
      (if (consp xy) ; xy is not nil, i.e. next move is not pass
          (sgf-svg-add-text next-group x y text (sgf-svg-set-color (sgf-enemy-stone stone))))
      (setq branch-index (1+ branch-index)))))


(defun sgf-svg-add-text (svg x y text color &optional attributes)
  (apply #'svg-text svg text
         :x (* x sgf-svg-interval) :y (* y sgf-svg-interval)
         :fill color
         :dy ".25em" ; or
         ;; :baseline-shift "-30%"
         ;; librsvg issue: https://github.com/lovell/sharp/issues/1996
         ;; :alignment-baseline "central"
         attributes))

(defun sgf-svg-group-next (svg) (car (dom-by-id svg sgf-svg--node-id-nexts)))
(defun sgf-svg-group-marks (svg) (car (dom-by-id svg sgf-svg--node-id-marks)))
(defun sgf-svg-group-stones (svg) (car (dom-by-id svg sgf-svg--node-id-stones)))
(defun sgf-svg-group-mvnums (svg) (car (dom-by-id svg sgf-svg--node-id-mvnums)))

(defun sgf-svg-stone-id (x y) (format "stone-%s-%s" x y))
(defun sgf-svg-mvnum-id (x y) (format "mvnum-%s-%s" x y))


(defun sgf-svg-add-stones (svg game-state)
  "Add stones to the board."
  (let ((svg-group (sgf-svg-group-stones svg))
        (board-2d (aref game-state 1)))
    (sgf-svg-clear-node-content svg-group)
    (dotimes (y (length board-2d))
      (dotimes (x (length (aref board-2d y)))
        (let ((state (sgf-board-get (cons x y) board-2d)))
          (unless (equal state 'E) (sgf-svg-add-stone svg-group x y state)))))))


(defun sgf-svg-add-stone (svg-group x y stone)
  "STONE is a symbol."
  (let ((cx (* x sgf-svg-interval))
        (cy (* y sgf-svg-interval))
        (r (* sgf-svg-interval 0.48)))
    (svg-circle svg-group cx cy r
                ;;:id (sgf-svg-stone-id x y)
                :gradient stone)))

(defun sgf-svg-add-mvants (svg game-state)
  "Add move annotations to the board.

For the move annotation, add circle ring of color to the stone on the board."
  (let ((board-2d (aref game-state 1))
        (curr-lnode (aref game-state 0))
        (annotated-xys (make-hash-table :test 'equal))
        (svg-group (sgf-svg-group-stones svg)))
    (while (not (sgf-root-p curr-lnode))
      (let* ((node (aref curr-lnode 1))
             (move (sgf-process-move node))
             (xy    (cdr move))
             (color (cond ((alist-get 'BM node) "red") ; good move
                          ((alist-get 'DO node) "magenta") ; doubtful move
                          ((alist-get 'IT node) "yellow") ; interesting move
                          ((alist-get 'TE node) "green")))); tesuji move
        (if (and xy color ; not a pass and has annotation
                 (not (sgf-xy-is-empty-p xy board-2d)) ; xy is not empty on current board
                 (not (gethash xy annotated-xys)))
            (svg-circle svg-group
                        (* (car xy) sgf-svg-interval)
                        (* (cdr xy) sgf-svg-interval)
                        (* 0.48 sgf-svg-interval)
                        :fill "none" :stroke color :stroke-width 2))
        (puthash xy t annotated-xys)
        ;; move to the previous node
        (setq curr-lnode (aref curr-lnode 0))))))


(defun sgf-svg-add-mvnums (svg game-state)
  "Add move numbers to the board."
  (let* ((svg-group (sgf-svg-group-mvnums svg))
         (board-2d (aref game-state 1))
         (curr-lnode (aref game-state 0))
         (mvnum (sgf-lnode-move-number curr-lnode))
         (numbered-xys (make-hash-table :test 'equal))
         (last-move-p t))
    (sgf-svg-update-status-mvnum svg mvnum)
    (sgf-svg-clear-node-content svg-group)
    (while (not (sgf-root-p curr-lnode))
      (let* ((node (aref curr-lnode 1))
             (move (sgf-process-move node))
             (xy    (cdr move))
             (stone (car move))
             (last-color (when last-move-p
                           ;; first iteration (ie last move) is red
                           (setq last-move-p nil) "red")))
        (if (and xy (not (gethash xy numbered-xys)))
            (let* ((xy-state (sgf-board-get xy board-2d))
                   (actual-color (or last-color
                                     (sgf-svg-set-color (if (eq xy-state 'E)
                                                     ;; this is a removed, captured stone
                                                            (sgf-enemy-stone stone)
                                                          xy-state)))))
              ;; add move number only if it does not already have a number.
              (sgf-svg-add-mvnum svg-group (car xy) (cdr xy) mvnum actual-color)
              (puthash xy t numbered-xys)))
        ;; move to the previous node
        (setq curr-lnode (aref curr-lnode 0))
        ;; decrement move number for the previous node, or
        ;; re-calculate we just passed thru a node with MN
        (setq mvnum (if (alist-get 'MN node)
                        (sgf-lnode-move-number curr-lnode)
                      (1- mvnum)))))))


(defun sgf-svg-add-mvnum (svg-group x y mvnum color)
  "COLOR is str."
  (sgf-svg-add-text svg-group
                    x y (number-to-string mvnum) color
                    (list :id (sgf-svg-mvnum-id x y))))


(defun sgf-svg-add-square (svg x y &rest attributes)
  (let ((r (* 0.2 sgf-svg-interval)))
    (apply 'svg-rectangle svg
           (- (* x sgf-svg-interval) r) (- (* y sgf-svg-interval) r) (* 2 r) (* 2 r)
           attributes)))


(defun sgf-svg-add-triangle (svg x y &rest attributes)
  (let ((cx (* x sgf-svg-interval))
        (cy (* y sgf-svg-interval))
        (r (* 0.2 sgf-svg-interval))
        (rt3 1.5))
    (apply 'svg-polygon svg
           (list (cons cx (+ cy (* r rt3 -0.55)))
                 (cons (+ cx r) (+ cy (* r rt3 0.45)))
                 (cons (- cx r) (+ cy (* r rt3 0.45))))
           attributes)))


(defun sgf-svg-add-circle (svg x y &rest attributes)
  (apply 'svg-circle svg (* x sgf-svg-interval) (* y sgf-svg-interval) (* 0.2 sgf-svg-interval) attributes))


(defun sgf-svg-add-cross (svg x y &rest attributes)
  (let ((cx (* x sgf-svg-interval))
        (cy (* y sgf-svg-interval))
        (r (* 0.2 sgf-svg-interval)))
    (apply 'svg-path svg
           (list (list 'moveto (list (cons (- cx r) (- cy r))))
                 (list 'lineto (list (cons (+ cx r) (+ cy r))))
                 (list 'moveto (list (cons (+ cx r) (- cy r))))
                 (list 'lineto (list (cons (- cx r) (+ cy r)))))
           attributes)))


(defun sgf-svg-add-mark (type svg-group x y color)
  "Add a mark to the marks group in a svg."
  (let* ((adders '((SQ . sgf-svg-add-square)
                   (CR . sgf-svg-add-circle)
                   (TR . sgf-svg-add-triangle)
                   (MA . sgf-svg-add-cross)))
         (adder (cdr (assoc type adders))))
    (funcall adder svg-group x y
             :fill "none" :stroke color :stroke-width 2)))


(defun sgf-svg-toggle-visibility (svg-group)
  "Toggle the visibility of the SVG-GROUP."
  (let ((visibility (dom-attr svg-group 'visibility)))
    (dom-set-attribute svg-group 'visibility
                       (if (or (null visibility) (equal visibility "visible"))
                           "hidden" "visible"))))


(defun sgf-svg-clear-node-content (node)
  "Remove all content under the SVG NODE.

Attributes are kept."
  (if node
      ;; Keep the tag name and attributes, remove all children
      (setcdr (cdr node) nil)))


(provide 'sgf-svg)
