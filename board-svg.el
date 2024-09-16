;;; board-svg.el --- svg visualization  -*- lexical-binding: t; -*-

(defcustom board-svg-interval 25
  "Default pixels for the size of grid cells.
It is a reference for all other element sizes."
  :type '(integer)
  :group 'board-svg)

(defcustom board-svg-margin 30
  "Default pixels for the margin of the board."
  :type '(integer)
  :group 'board-svg)

(defcustom board-svg-bar-height 27
  "Default pixels for the bar of the board."
  :type '(integer)
  :group 'board-svg)

(defcustom board-svg-padding 5
  "Default padding for the board. Used in buttons and other elements.")

(defcustom board-svg-font-family "Arial"
  "Default font family for the board."
  :type '(string)
  :group 'board-svg)


(defun board-svg-gradient (svg id type stops &rest args)
  "Instead of svg-gradient. svg-gradient not support additional
attributes(cx, cy, fx, fy, r, etc...)"
  (svg--def
   svg
   (apply
    'dom-node
    (if (eq type 'linear)
        'linearGradient
      'radialGradient)
    `((id . ,id)
      ,@(svg--arguments svg args))
    (mapcar
     (lambda (stop)
       (dom-node 'stop `((offset . ,(format "%s%%" (car stop)))
                         (stop-color . ,(cdr stop)))))
     stops))))

(defun board-svg-stone-gradient (svg color stops)
  (board-svg-gradient
   svg "black" 'radial '((0 . "#606060") (100 . "#000000"))
   :cx 0.5 :cy 0.5 :fx 0.7 :fy 0.3 :r 0.55)
  (board-svg-gradient
   svg "white" 'radial '((0 . "#ffffff") (80 . "#e0e0e0") (100 . "#b0b0b0"))
   :cx 0.5 :cy 0.5 :fx 0.7 :fy 0.3 :r 0.6)
  svg)

;; Create the SVG object
(defun board-svg-stone-gradient (svg color stops)
  "Create stone gradients"
  (let* ((defs (svg-node svg 'defs)) ; Create the <defs> node
         ;; Create the stone gradient
         (gradient (svg-node defs 'radialGradient :id color
                             :r "0.6" :fy "0.3" :fx "0.7" :cy "0.5" :cx "0.5")))
    (mapc (lambda (stop)
            (svg-node gradient 'stop :offset (car stop) :stop-color (cdr stop)))
          stops)))
;; (svg-node gradient-black 'stop :offset "0%" :stop-color "#606060")
;; (svg-node gradient-black 'stop :offset "100%" :stop-color "#000000")
;; (svg-node gradient-white 'stop :offset "0%" :stop-color "#ffffff")
;; (svg-node gradient-white 'stop :offset "100%" :stop-color "#b0b0b0")))

;; (let ((svg  (svg-create 800 800)))
;;   (board-svg-stone-gradient svg "black" '((0 . "#606060") (100 . "#000000")))
;;   (svg-print svg))


;; <rect width=\"34\" height=\"4\" x=\"0\" y=\"30\" id=\"status-turn\" fill=\"#f00\"></rect>
(defun board-svg-init (w h interval margin bar-height padding)
  (let* ((font-size (/ interval 2))
         (grid-w (* interval (1- w)))
         (grid-h (* interval (1- h)))
         (board-w (+ margin grid-w margin))
         (board-h (+ margin grid-h margin))
         (menu-bar-y (+ bar-height board-h))
         (line-width 0.5)
         (star-radius 3)
         (hot-grid-u-l (cons margin (+ bar-height margin)))
         (hot-grid-b-r (cons (+ margin (* (1- w) interval))
                             (+ bar-height margin (* (1- h) interval))))
         (hot-areas (list (list (cons 'rect (cons hot-grid-u-l hot-grid-b-r))
                                'hot-grid (list 'pointer 'hand))))
         svg statu-bar board grid menu-bar ; svg nodes
         idx)
    ;; Note that the order of svg elements matters
    (setq svg (svg-create board-w (+ bar-height board-h bar-height)
                          :font-family board-svg-font-family))
    ;; Stones' Gradient
    (board-svg-stone-gradient svg "B" '((0 . "#606060") (100 . "#000000")))
    (board-svg-stone-gradient svg "W" '((0 . "#ffffff") (100 . "#b0b0b0")))
    ;; Status Bar
    (setq status-bar (svg-node svg 'g :id "status-bar"))
    (svg-rectangle status-bar 0 0 board-w bar-height :fill "gray")
    (svg-circle status-bar (/ interval 2)
                (/ interval 2) (/ interval 3) :gradient "B")
    (svg-circle status-bar (- board-w (/ interval 2))
                (/ interval 2) (/ interval 3) :gradient "W")
    ;; show what play turn it is
    (svg-rectangle status-bar 0 (- bar-height padding)
                   interval padding
                   :id "status-b" :fill "gray")
    (svg-rectangle status-bar (- board-w interval) (- bar-height padding)
                   interval padding
                   :id "status-w" :fill "gray")
    ;; show move number
    (svg-text status-bar "0" :x (/ board-w 2) :y bar-height
              :id "status-n" :fill "white"
              :font-family board-svg-font-family :font-weight "bold"
              :text-anchor "middle" :dy "-0.5em")
    ;; show prisoner number
    (svg-text status-bar "0" :x (* interval 2) :y bar-height
              :id "status-pb" :fill "white"
              :font-weight "bold" :font-family board-svg-font-family
              :text-anchor "middle" :dy "-0.5em")
    (svg-text status-bar "0" :x (- board-w interval interval) :y bar-height
              :id "status-pw" :fill "white"
              :font-weight "bold" :font-family board-svg-font-family
              :text-anchor "middle" :dy "-0.5em")

    ;; Board Rect
    (setq board (svg-node svg 'g
                          :id "game-board"
                          :transform (format "translate(%s, %s)" 0 bar-height)
                          :fill "black"))
    (svg-rectangle board 0 0 board-w board-h :fill "#e3aa4e")
    ;; Board Grid
    (setq grid (svg-node board 'g
                         :id "game-grid"
                         :font-size font-size
                         :transform (format "translate(%s, %s)" margin margin)))
    ;; Grid Lines
    (dotimes (n w)
      ;; vertical lines
      (setq idx (format "%c" (if (< n (- ?I ?A)) (+ ?A n) (+ ?A n 1)))) ; skip char I
      (svg-text grid idx :class "grid-idx"
                :x (* interval n) :y (- font-size)
                :dominant-baseline "hanging" :text-anchor "middle")
      (svg-text grid idx :class "grid-idx"
                :x (* interval n) :y (* interval h)
                :text-anchor "middle")
      (svg-line grid (* interval n) 0 (* interval n) (* grid-h)
                :stroke "black" :stroke-width line-width))
    (dotimes (n h)
      ;; horizontal lines
      (setq idx (number-to-string (- h n)))
      (svg-text grid idx :class "grid-idx"
                :x (- font-size) :y (* interval n)
                :text-anchor "end" :dy ".25em")
      (svg-text grid idx :class "grid-idx"
                :x (+ grid-w font-size) :y (* interval n)
                :text-anchor "start" :dy ".25em")
      (svg-line grid 0 (* interval n) grid-w (* interval n)
                :stroke "black" :stroke-width line-width))

    ;; Stars
    (dolist (hoshi (sgf-board-hoshi w h))
      (svg-circle grid (* interval (car hoshi)) (* interval (cdr hoshi)) star-radius))

    ;; Layers: 3 types of information on board ;todo remove hard coding
    (svg-node grid 'g :id "stones")
    (svg-node grid 'g :id "mvnums")
    (svg-node grid 'g :id "next")
    (svg-node grid 'g :id "marks")

    ;; Menu Bar at bottom
    (setq menu-bar (svg-node svg 'g :id "menu-bar" :fill "gray"
                             :transform (format "translate(%s, %s)" 0 menu-bar-y)))
    ;; (svg-rectangle menu-bar 0 menu-bar-y board-w bar-height :fill "gray")
    (nconc hot-areas (board-svg-create-menu-buttons menu-bar menu-bar-y bar-height padding))

    (cons svg hot-areas)))


(defun board-svg-create-menu-buttons (menu-bar menu-bar-y bar-height padding)
  "Create all the buttons and return the hot areas for the buttons"
  (let ((btns '((hot-menu "Menu" "menu")
                (hot-first "|<" "Move to the beginning of the game")
                (hot-backward "<" "Move backward")
                (hot-forward ">" "Move forward")
                (hot-last ">|" "Move to the end of the game")
                (hot-del "Del" "Delete the current move")
                (hot-pass "Pass" "Pass the current move")))
        (x padding) (y padding)
        (height (- bar-height (* 2 padding))))

    (mapcar (lambda (btn)
              (let* ((hot-area-id (nth 0 btn))
                     (name        (nth 1 btn))
                     (tooltip     (nth 2 btn))
                     (width (+ (* 2 padding) (string-pixel-width name)))
                     hot-area)
                (svg-rectangle menu-bar x y width height :rx 3 :ry 3 :fill "#fff")
                (svg-text menu-bar name
                          :x (+ x (/ width 2)) :y height
                          :text-anchor "middle" :fill "#000")
                (setq hot-area (list (cons 'rect
                                           (cons (cons x
                                                       menu-bar-y)
                                                 (cons (+ x width)
                                                       (+ bar-height menu-bar-y))))
                                     hot-area-id (list 'pointer 'hand
                                                       'help-echo tooltip)))
                (setq x (+ x width padding))
                hot-area))
            btns)))


(defun board-svg-set-color (xy-state &optional same)
  "Set mark/text color according to background color of the intersections on board."
  ;; @todo optimize color
  (cond ((equal xy-state 'B) (if same "black" "white"))
        ((equal xy-state 'W) (if same "white" "black"))
        ((equal xy-state 'E) "white")
        (t "white")))


(defun board-svg-update-turn (svg stone)
  "Show the current turn on the status bar."
  (let ((status-w (car (dom-by-id svg "^status-w$")))
        (status-b (car (dom-by-id svg "^status-b$"))))
    (cond ((equal stone 'B)
           (dom-set-attribute status-w 'fill "#f00")
           (dom-set-attribute status-b 'fill "gray"))
          ((equal stone 'W)
           (dom-set-attribute status-w 'fill "gray")
           (dom-set-attribute status-b 'fill "#f00"))
          (t (error "Invalid stone color %s" stone)))))


(defun board-svg-update-mvnum (svg mvnum)
  "Show the current move number on the status bar."
  (let ((status-n (car (dom-by-id svg "^status-n$"))))
    (setcar (nthcdr 2 status-n) (number-to-string mvnum))))


(defun board-svg-update-prisoners (svg prisoners)
  "Show the current prisoners on the status bar."
  (let ((status-pb (car (dom-by-id svg "^status-pb$")))
        (status-pw (car (dom-by-id svg "^status-pw$")))
        (pb (car prisoners))
        (pw (cdr prisoners)))
    (setcar (nthcdr 2 status-pb) (number-to-string pb))
    (setcar (nthcdr 2 status-pw) (number-to-string pw))))


(defun board-svg-clear-node-content (node)
  "Remove all content under the SVG node."
  (if node
      ;; Keep the tag name and attributes, remove all children
      (setcdr (cdr node) nil)))


(defun board-svg-update-marks (svg interval node board-2d)
  "Process and update the marks on the board for a node.

It removes the old marks and adds the new marks."
  ;; make sure to remove old marks
  (let ((marks-group (board-svg-group-marks svg))
        type)
    (board-svg-clear-node-content marks-group)
    (dolist (prop node)
      (setq type (car prop))
      (if (member type '(SQ TR CR MA))
          (dolist (xy (cdr prop))
            (setq x (car xy) y (cdr xy)
                  xy-state (sgf-board-2d-get xy board-2d))
            (board-svg-add-mark type marks-group interval x y xy-state))))))


(defun board-svg-update-next (svg interval curr-lnode)
  "Update and show next move(s) on board svg."
  (let* ((next-lnodes (aref curr-lnode 2))
         (branch-count (length next-lnodes))
         (branch-index 0)
         (next-group (board-svg-group-next svg))
         text play color xy x y)
    (board-svg-clear-node-content next-group)
    (dolist (next-lnode next-lnodes)
      (if (= branch-count 1)
          (setq text "x")
        (setq text (string (+ ?a branch-index))))
      (setq play (sgf-process-play (aref next-lnode 1))
            stone (car play) xy (cdr play) x (car xy) y (cdr xy))
      (if (consp xy) ; xy is not nil, i.e. next move is not pass
          (board-svg-add-text next-group interval x y text (board-svg-set-color stone t)))
      (setq branch-index (1+ branch-index)))))


(defun board-svg-add-text (svg interval x y text color &optional attributes)
  (apply #'svg-text svg text
         :x (* x interval) :y (* y interval) :fill color
         :text-anchor "middle"
         :dy ".25em" ; or
         ;; :baseline-shift "-30%"
         ;; librsvg issue: https://github.com/lovell/sharp/issues/1996
         ;; :alignment-baseline "central"
         :font-family board-svg-font-family
         :font-weight "bold"
         attributes))

(defun board-svg-group-next (svg) (car (dom-by-id svg "next")))
(defun board-svg-group-marks (svg) (car (dom-by-id svg "marks")))
(defun board-svg-group-stones (svg) (car (dom-by-id svg "stones")))
(defun board-svg-group-mvnums (svg) (car (dom-by-id svg "mvnums")))

(defun board-svg-stone-id (x y) (format "stone-%s-%s" x y))
(defun board-svg-mvnum-id (x y) (format "mvnum-%s-%s" x y))

(defun board-svg-add-stone (svg interval x y color)
  (let ((cx (* x interval))
        (cy (* y interval))
        (r (* interval 0.48)))
    (svg-circle (board-svg-group-stones svg) cx cy r
                :id (board-svg-stone-id x y)
                :gradient color)))

;; todo  ((eq node current-node) "#f00")
(defun board-svg-add-mvnum (svg interval x y mvnum color)
    (board-svg-add-text
     (board-svg-group-mvnums svg)
     interval x y (number-to-string mvnum) color
     (list :id (board-svg-mvnum-id x y))))


(defun board-svg-add-square (svg interval x y &rest attributes)
  (let ((r (* 0.2 interval)))
    (apply 'svg-rectangle svg
           (- (* x interval) r) (- (* y interval) r) (* 2 r) (* 2 r)
           attributes)))


(defun board-svg-add-triangle (svg interval x y &rest attributes)
  (let ((cx (* x interval))
        (cy (* y interval))
        (r (* 0.2 interval))
        (rt3 1.5))
    (apply 'svg-polygon svg
           (list (cons cx (+ cy (* r rt3 -0.55)))
                 (cons (+ cx r) (+ cy (* r rt3 0.45)))
                 (cons (- cx r) (+ cy (* r rt3 0.45))))
           attributes)))


(defun board-svg-add-circle (svg interval x y &rest attributes)
    (apply 'svg-circle svg (* x interval) (* y interval) (* 0.2 interval) attributes))


(defun board-svg-add-cross (svg interval x y &rest attributes)
  (let ((cx (* x interval))
        (cy (* y interval))
        (r (* 0.2 interval)))
    (apply 'svg-path svg
              (list (list 'moveto (list (cons (- cx r) (- cy r))))
                    (list 'lineto (list (cons (+ cx r) (+ cy r))))
                    (list 'moveto (list (cons (+ cx r) (- cy r))))
                    (list 'lineto (list (cons (- cx r) (+ cy r)))))
              attributes)))


(defun board-svg-add-mark (type svg-group interval x y xy-state)
  "Add a mark to the marks group in a svg."
  (let* ((color (board-svg-set-color xy-state))
         (adders '((SQ . board-svg-add-square)
                   (CR . board-svg-add-circle)
                   (TR . board-svg-add-triangle)
                   (MA . board-svg-add-cross)))
         (adder (cdr (assoc type adders))))
    (funcall adder svg-group interval x y
             :fill "none" :stroke color :stroke-width 2)))



(defun igo-svg-last-move (svg game grid-interval)
  ;; todo
  (igo-svg-remove-last-move svg)

  (let* ((curr-node (igo-game-current-node game))
         (move (igo-node-move curr-node)))
    (if (igo-placement-p move)
        (let* ((board (igo-game-board game))
               (x (igo-board-pos-to-x board move))
               (y (igo-board-pos-to-y board move))
               (cx (* x grid-interval))
               (cy (* y grid-interval))
               (r (ceiling (* grid-interval 0.15)))
               ;;(color (igo-opposite-color (igo-game-turn game)))
               )
          (svg-rectangle
           ;;(igo-svg-overlays-group svg)
           (igo-svg-stones-group svg)
           (- cx r)
           (- cy r)
           (* 2 r)
           (* 2 r)
           :id "last-move"
           :fill (cond
                  ;;((igo-black-p color) "rgba(255,255,255,0.5)")
                  ;;((igo-white-p color) "rgba(0,0,0,0.5)")
                  (t "rgba(255,0,0,0.8)")))))))

(defun igo-svg-remove-last-move (svg)
  (dom-remove-node svg (car (dom-by-id svg "^last-move$"))))

;; Marker
