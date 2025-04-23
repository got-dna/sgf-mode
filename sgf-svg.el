;;; sgf-svg.el --- svg visualization  -*- lexical-binding: t; -*-

;; Author: Zech Xu
;; Version: 1.0
;; Package-Requires: ((emacs "30.1"))
;; Homepage: homepage
;; Keywords: svg, go, game

;;; Commentary:
;; SVG rendering of Go board and game moves.

;;; Code:

(require 'svg)
(require 'sgf-util)

(defcustom sgf-svg-size 28
  "Default pixels for the size of grid cells.
It is a reference for all other element sizes."
  :type '(number) :group 'sgf-svg)


(defcustom sgf-svg-stone-size 0.48
  "Default size for stone radius"
  :type '(number) :group 'sgf-svg)


(defcustom sgf-svg-font-family "Arial"
  "Default font family for the board."
  :type '(string) :group 'sgf-svg)


(defun sgf-svg-group (svg group-id)
  (or (car (dom-by-id svg group-id))
      ;; create the new group if it does not exist
      (let ((grid (car (dom-by-id svg "grid"))))
        (svg-node grid 'g :id group-id))))


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
  "Create the svg object for the game with width W and height H."
  (let* ((grid-w (* sgf-svg-size (1- w)))
         (grid-h (* sgf-svg-size (1- h)))
         (board-w (* sgf-svg-size (1+ w)))
         (board-h (* sgf-svg-size (1+ h)))
         (line-width 0.5)
         (hot-grid-t-l (cons sgf-svg-size (+ sgf-svg-size sgf-svg-size)))
         (hot-grid-b-r (cons (* w sgf-svg-size) (* (1+ h) sgf-svg-size)))
         (hot-areas (list (list (cons 'rect (cons hot-grid-t-l hot-grid-b-r))
                                'hot-grid (list 'pointer 'hand))))
         svg bar board grid ; svg nodes
         idx)
    ;; Note that the order of svg elements matters
    (setq svg (svg-create board-w (+ board-h sgf-svg-size)))

    ;; Stones' Gradient
    (sgf-svg-stone-gradient svg "B" '((0 . "#606060") (100 . "#000000")))
    (sgf-svg-stone-gradient svg "W" '((0 . "#ffffff") (100 . "#b0b0b0")))

    ;; Status Bar
    (setq bar (svg-node svg 'g :id "bar"
                        :font-family sgf-svg-font-family
                        :font-weight "bold"
                        :text-anchor "middle"))
    (svg-rectangle bar 0 0 board-w sgf-svg-size :fill "gray")
    (sgf-svg-add-circle-xyr bar 0.4       0.5 0.3 :gradient "B")
    (sgf-svg-add-circle-xyr bar (+ 0.6 w) 0.5 0.3 :gradient "W")
    ;; show what play turn it is
    (sgf-svg-add-circle-xyr bar 0.4 0.5       0.1 :id "status-b" :fill "none")
    (sgf-svg-add-circle-xyr bar (+ 0.6 w) 0.5 0.1 :id "status-w" :fill "none")
    ;; show move number
    (svg-text bar "0" :x (/ board-w 2) :y sgf-svg-size :id "status-n" :fill "white" :dy "-0.75em")
    ;; show prisoner number
    (svg-text bar "0" :x sgf-svg-size :y sgf-svg-size
              :id "status-pb" :fill "white" :dy "-0.75em")
    (svg-text bar "0" :x (- board-w sgf-svg-size) :y sgf-svg-size
              :id "status-pw" :fill "white" :dy "-0.75em")
    ;; Board Rect
    (setq board (svg-node svg 'g :id "board" :fill "black"
                          :transform (format "translate(%s,%s)" 0 sgf-svg-size)))
    (svg-rectangle board 0 0 board-w board-h :fill "#e3aa4e")
    ;; Board Grid
    (setq grid (svg-node board 'g :id "grid"
                         :font-family sgf-svg-font-family
                         :font-size sgf-svg-size
                         :font-weight "bold"
                         :text-anchor "middle"
                         :transform (format "translate(%s,%s)" sgf-svg-size sgf-svg-size)))
    (setq grid-idx (svg-node grid 'g :id "grid-idx" :font-size "0.4em"))
    ;; Grid Lines
    (dotimes (n w)
      ;; vertical lines
      (setq idx (format "%c" (if (< n (- ?I ?A)) (+ ?A n) (+ ?A n 1)))) ; skip char I
      ;; (setq idx (format "%c" (sgf-encode-d2c n)))
      (svg-text grid-idx idx :x (* sgf-svg-size n) :y (* sgf-svg-size h)
                :dy "-0.5em")
      (svg-line grid-idx (* sgf-svg-size n) 0 (* sgf-svg-size n) (* grid-h)
                :stroke "black" :stroke-width line-width))
    (dotimes (n h)
      ;; horizontal lines
      (setq idx (number-to-string (- h n)))
      (svg-text grid-idx idx :x (- sgf-svg-size) :y (* sgf-svg-size n)
                :text-anchor "start" :dy ".5em")
      (svg-line grid-idx 0 (* sgf-svg-size n) grid-w (* sgf-svg-size n)
                :stroke "black" :stroke-width line-width))

    ;; Hoshi/Stars
    (dolist (hoshi (sgf-board-hoshi w h))
      (sgf-svg-add-circle-xyr grid (car hoshi) (cdr hoshi) 0.07))

    (cons svg hot-areas)))


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
    (cond ((eq stone 'W)
           (dom-set-attribute status-w 'fill "#f00")
           (dom-set-attribute status-b 'fill "none"))
          ((eq stone 'B)
           (dom-set-attribute status-w 'fill "none")
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


(defun sgf-svg-update-ko (svg ko)
  "Clear old ko position and label new ko position."
  (let ((group (sgf-svg-group svg "ko")))
    (sgf-svg-clear-group-content group)
    (if ko (sgf-svg-add-text group (car ko) (cdr ko) "X" "red"))))


(defun sgf-svg-update-marks (svg node board-2d)
  "Toggle or update the marks on the board for a node.

It removes the old marks and adds the new marks."
  ;; make sure to remove old marks
  (let ((group (sgf-svg-group svg "marks"))
        type)
    (sgf-svg-clear-group-content group)
    (dolist (prop node)
      (setq type (car prop))
      (if (member type '(SQ TR CR MA))
          (dolist (xy (cdr prop))
            (let* ((x (car xy)) (y (cdr xy))
                   (xy-state (sgf-board-get xy board-2d))
                   (color (sgf-svg-set-color xy-state)))
              (sgf-svg-add-mark type group x y color))))
      (if (eq type 'LB)
          (dolist (prop-val (cdr prop))
            (let* ((label (cdr prop-val))
                   (xy (car prop-val))
                   (x (car xy)) (y (cdr xy))
                   (xy-state (sgf-board-get xy board-2d))
                   (color (sgf-svg-set-color xy-state)))
              ;; (message "---------%S %S" xy-state color)
              (sgf-svg-add-text group x y label color
                                :font-size "0.5em")))))))


(defun sgf-svg-update-hints (svg curr-lnode)
  "Update hints of the next move(s) on board svg."
  (let* ((group (sgf-svg-group svg "hints"))
         (next-lnodes (aref curr-lnode 2))
         (branch-count (length next-lnodes)))
    (sgf-svg-clear-group-content group)
    (dotimes (i branch-count)
      (let* ((text (if (= branch-count 1)
                       (setq text "x")
                     (setq text (string (+ ?a i)))))
             (next-lnode (nth i next-lnodes))
             (node (aref next-lnode 1))
             (play (sgf-process-move node))
             (stone (car play)) (xy (cdr play)) (x (car xy)) (y (cdr xy)))
        (if (consp xy) ; xy is not nil, i.e. next move is not pass
            (sgf-svg-add-text group x y text
                              (sgf-svg-set-color (sgf-enemy-stone stone))
                              :font-size "0.7em"))))))


(defun sgf-svg-update-stones (svg game-state)
  "Add stones to the board."
  (let ((group (sgf-svg-group svg "stones"))
        (board-2d (aref game-state 1)))
    (sgf-svg-clear-group-content group)
    (dotimes (y (length board-2d))
      (dotimes (x (length (aref board-2d y)))
        (let ((state (sgf-board-get (cons x y) board-2d)))
          (unless (eq state 'E)
            (sgf-svg-add-circle-xyr group
                                    x y sgf-svg-stone-size
                                    :gradient state)))))))


(defun sgf-svg-interpolate-color (color1 color2 ratio)
  "Interpolate between COLOR1 and COLOR2 based on RATIO between 0 and 1."
  (let* ((color1-rgb (color-name-to-rgb color1))
         (color2-rgb (color-name-to-rgb color2))
         (r (+ (* (nth 0 color1-rgb) (- 1 ratio))
               (* (nth 0 color2-rgb) ratio)))
         (g (+ (* (nth 1 color1-rgb) (- 1 ratio))
               (* (nth 1 color2-rgb) ratio)))
         (b (+ (* (nth 2 color1-rgb) (- 1 ratio))
               (* (nth 2 color2-rgb) ratio))))
    ;; (message "color ratio: %.2f %.2f %.2f" val lim ratio)
    (format "#%02x%02x%02x"
            (round (* 255 r))
            (round (* 255 g))
            (round (* 255 b)))))


(defun sgf-svg-update-katago (svg katago &optional score-p)
  "Updating SVG with KataGo analysis data.

KATAGO is the katago output for next moves. If it is nil, do nothing,
clear the katago svg node."
  (let* ((group (sgf-svg-group svg "katago"))
         (beg-color "red")
         (end-color "green")
         (min-delta (if score-p
                        (sgf-svg--katago-score-range katago)
                      (cons 0 100)))
         (min-value (car min-delta))
         (delta-max (cdr min-delta)))
    (sgf-svg-clear-group-content group)
    (dotimes (i (length katago))
      (let* ((move (nth i katago))
             (xy (car move))
             (x (car xy)) (y (cdr xy))
             (info (cdr move))
             (score (plist-get info :score))
             (winrate (plist-get info :winrate))
             (value (if score-p score winrate))
             (ratio (/ (- value min-value) delta-max))
             (color (sgf-svg-interpolate-color beg-color end-color ratio)))
        (if (null xy)
            (message "The KataGo move of 1st choice is a pass. It probably indicates the end of the game.")
          (sgf-svg-add-circle-xyr group x y sgf-svg-stone-size
                                  :fill color :opacity 0.3)
          ;; show index
          (sgf-svg-add-text group x y (format "%d" (1+ i)) "white"
                            :baseline-shift "90%"
                            :font-size (* sgf-svg-size 0.3))
          ;; show score
          (sgf-svg-add-text group x y (format "%.1f" score) "black"
                            :font-size (* sgf-svg-size 0.35))
          ;; show winrate
          (sgf-svg-add-text group x y (format "%d" (round winrate)) "black"
                            :baseline-shift "-100%"
                            :font-size (* sgf-svg-size 0.3)))))))

(defun sgf-svg--katago-score-range (katago)
  (if katago
      (let* ((scores (mapcar (lambda (move) (plist-get (cdr move) :score)) katago))
             (max-score (apply 'max scores))
             (min-score (apply 'min scores))
             (delta-max (- max-score min-score)))
        (cons min-score delta-max))))


(defun sgf-svg-update-katago-pv (svg pv turn)
  "Show the principal variation of KataGo analysis on the board."
  (let* ((group (sgf-svg-group svg "katago")))
    (sgf-svg-clear-group-content group)
    ;;(sgf-svg-update-katago svg nil) ; clear the katago svg node
    (dotimes (i (length pv))
      (let* ((xy (nth i pv)) (x (car xy)) (y (cdr xy)))
        (when xy ; katago could have pass move in the variation, indicating the end of the game
          (sgf-svg-add-circle-xyr group x y sgf-svg-stone-size
                                  :gradient (format "%s" turn)
                                  :opacity 0.5)
          (sgf-svg-add-text group x y
                            (number-to-string (1+ i))
                            (sgf-svg-set-color turn)
                            :text-decoration "underline"
                            :font-size "0.5em")
          (setq turn (if (eq turn 'B) 'W 'B)))))))


(defun sgf-svg-update-annotations (svg game-state)
  "Add move annotations to the board.

For the move annotation, add circle ring of color to the stone on the board."
  (let ((board-2d (aref game-state 1))
        (curr-lnode (aref game-state 0))
        (annotated-xys (make-hash-table :test 'equal))
        (group (sgf-svg-group svg "annotations")))
    (sgf-svg-clear-group-content group)
    (while (not (sgf-root-p curr-lnode))
      (let* ((node (aref curr-lnode 1))
             (move (sgf-process-move node))
             (xy    (cdr move))
             (color (cond ((alist-get 'BM node) sgf-bm-color) ; good move
                          ((alist-get 'DO node) sgf-do-color) ; doubtful move
                          ((alist-get 'IT node) sgf-it-color) ; interesting move
                          ((alist-get 'TE node) sgf-te-color)))); tesuji move
        (if (and xy color ; not a pass and has annotation
                 (not (sgf-xy-is-empty-p xy board-2d)) ; xy is not empty on current board
                 (not (gethash xy annotated-xys)))
            (sgf-svg-add-circle-xyr group
                                    (car xy)
                                    (cdr xy)
                                    sgf-svg-stone-size
                                    :fill "none" :stroke color :stroke-width 2))
        (puthash xy t annotated-xys)
        ;; move to the previous node
        (setq curr-lnode (aref curr-lnode 0))))))


(defun sgf-svg-update-numbers (svg game-state)
  "Update move numbers on the board."
  (let* ((group (sgf-svg-group svg "numbers"))
         (board-2d (aref game-state 1))
         (curr-lnode (aref game-state 0))
         (mvnum (sgf-lnode-move-number curr-lnode))
         (numbered-xys (make-hash-table :test 'equal))
         (last-move-p t))
    (sgf-svg-update-status-mvnum svg mvnum)
    (sgf-svg-clear-group-content group)
    (while (not (sgf-root-p curr-lnode))
      (let* ((node (aref curr-lnode 1))
             (move (sgf-process-move node))
             (xy    (cdr move))
             (stone (car move))
             ;; first iteration (ie last move) is red
             (last-color (when last-move-p (setq last-move-p nil) "red")))
        (if (and xy (not (gethash xy numbered-xys)))
            (let* ((xy-state (sgf-board-get xy board-2d))
                   (actual-color (or last-color
                                     (sgf-svg-set-color (if (eq xy-state 'E)
                                                            ;; this is a removed, captured stone
                                                            (sgf-enemy-stone stone)
                                                          xy-state)))))
              ;; add move number only if it does not already have a number.
              (sgf-svg-add-text group (car xy) (cdr xy)
                                (number-to-string mvnum)
                                actual-color
                                :font-size "0.4em")
              (puthash xy t numbered-xys)))
        ;; move to the previous node
        (setq curr-lnode (aref curr-lnode 0))
        ;; decrement move number for the previous node, or
        ;; re-calculate we just passed thru a node with MN
        (setq mvnum (if (alist-get 'MN node)
                        (sgf-lnode-move-number curr-lnode)
                      (1- mvnum)))))))


(defun sgf-svg-add-text (svg x y text color &rest attributes)
  (apply #'svg-text svg text
         :x (* x sgf-svg-size) :y (* y sgf-svg-size)
         :fill color
         :dy ".25em" ; or
         ;; :baseline-shift "-30%"
         ;; TODO: librsvg issue: https://github.com/lovell/sharp/issues/1996
         ;; :alignment-baseline "central"
         attributes))


(defun sgf-svg-add-square (svg x y &rest attributes)
  (let ((r (* 0.2 sgf-svg-size)))
    (apply 'svg-rectangle svg
           (- (* x sgf-svg-size) r) (- (* y sgf-svg-size) r) (* 2 r) (* 2 r)
           attributes)))


(defun sgf-svg-add-triangle (svg x y &rest attributes)
  (let ((cx (* x sgf-svg-size))
        (cy (* y sgf-svg-size))
        (r (* 0.2 sgf-svg-size))
        (rt3 1.5))
    (apply 'svg-polygon svg
           (list (cons cx (+ cy (* r rt3 -0.55)))
                 (cons (+ cx r) (+ cy (* r rt3 0.45)))
                 (cons (- cx r) (+ cy (* r rt3 0.45))))
           attributes)))


(defun sgf-svg-add-circle-xyr (svg x y r &rest attributes)
  (apply 'svg-circle svg
         (* x sgf-svg-size)
         (* y sgf-svg-size)
         (* r sgf-svg-size)
         attributes))

(defun sgf-svg-add-circle (svg x y &rest attributes)
  (apply #'sgf-svg-add-circle-xyr svg x y 0.2 attributes))

(defun sgf-svg-add-cross (svg x y &rest attributes)
  (let ((cx (* x sgf-svg-size))
        (cy (* y sgf-svg-size))
        (r (* 0.2 sgf-svg-size)))
    (apply 'svg-path svg
           (list (list 'moveto (list (cons (- cx r) (- cy r))))
                 (list 'lineto (list (cons (+ cx r) (+ cy r))))
                 (list 'moveto (list (cons (+ cx r) (- cy r))))
                 (list 'lineto (list (cons (- cx r) (+ cy r)))))
           attributes)))


(defun sgf-svg-add-mark (type group x y color)
  "Add a mark to the marks group in a svg."
  (let* ((adders '((SQ . sgf-svg-add-square)
                   (CR . sgf-svg-add-circle)
                   (TR . sgf-svg-add-triangle)
                   (MA . sgf-svg-add-cross)))
         (adder (cdr (assoc type adders))))
    (funcall adder group x y :fill "none" :stroke color :stroke-width 2)))


(defun sgf-svg-toggle-visibility (group)
  "Toggle the visibility of the SVG-GROUP."
  (let ((visibility (dom-attr group 'visibility)))
    (dom-set-attribute group 'visibility
                       (if (or (null visibility) (equal visibility "visible"))
                           "hidden" "visible"))))


(defun sgf-svg-clear-group-content (group)
  "Remove all content under the svg GROUP.

Attributes are kept."
  ;; Keep the tag name and attributes, remove all children
  (if group (setcdr (cdr group) nil)))


(provide 'sgf-svg)
