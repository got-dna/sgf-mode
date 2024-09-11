;; todo organzie code

(defun board-xy-to-pos (xy w)
  "Convert a xy point on a board of size WxH to a position of index in the 1-d vector.
XY is a cons cell of the form (X . Y)."
  (+ (car xy) (* (cdr xy) w)))

(defun board-pos-to-xy (ps w)
  "Convert a position index to a point on a board of size WxH.
Return a cons cell of the form (X . Y)."
  (cons (% pos w) (/ pos w)))


(defun board-pos-neighbors (pos w h)
  "Return a list of neighbors of POS on a board of size WxH."
  (list
   (if (/= (% pos w) 0)      (1- pos)) ;; left
   (if (/= (% pos w) (1- w)) (1+ pos)) ;; right
   (if (> pos w)             (- pos w)) ;; above
   (if (< (+ pos w) (* w h)) (+ pos w)))) ;; below


(defun board-validate-w/h (size)
  (if (not (and (integerp size) (> size 0) (<= size 52)))
      (error "Invalid argument board size: %s" size)))


(defun board-init-board (w h &optional positions turn prisoners ko-pos)
  "Create board object.

W and H are the width and height of the board, respectively.
POSITIONS is a vector of length W*H containing the positions of the board.
TURN is the color of the player to move.
PRISONERS is a cons cell containing the number of prisoners for each color.
KO-POS is the position of the ko point, or nil if there is no ko point."
  (board-validate-w/h w)
  (board-validate-w/h h)
  (if (null positions) (setq positions (make-vector (* w h) 'E))) ; empty
  (if (null turn) (setq turn 'B))                 ; black
  (if (null prisoners) (setq prisoners (cons 0 0)))
  (if (null ko-pos) (setq ko-pos igo-npos))
  (vector w h positions turn prisoners ko-pos))

;; [9 9 [empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty empty ...] black (0 . 0) -1]
;; prisnors (black . white)


;;
;; Node
;;

(defun igo-node (prev &optional move color)
  (vector
   prev ;;0:prev node
   (or move igo-nmove) ;;1:move (position)
   color ;;2:color
   nil ;;3:properties
   nil ;;4:next nodes
   nil ;;5:last-visited
   ))

(defun igo-board (w h &optional intersections turn prisoners ko-pos)
  (if (null w) (setq w igo-board-default-w)
    (igo-board-validate-w w))
  (if (null h) (setq h igo-board-default-h)
    (igo-board-validate-h h))
  (if (null intersections) (setq intersections (make-vector (* w h) 'empty))
    (igo-board-validate-intersections intersections w h))
  (if (null turn) (setq turn 'black)
    (igo-board-validate-turn turn))
  (if (null prisoners) (setq prisoners (cons 0 0))
    (igo-board-validate-prisoners prisoners))
  (if (null ko-pos) (setq ko-pos igo-npos)
    (igo-board-validate-ko-pos ko-pos))

  (vector w h intersections turn prisoners ko-pos))


;; game object: ref. igo-game
(defun igo-game (&optional w h game-tree)
  (let* ((root-node (or game-tree
                        (igo-node nil)))
         (board (igo-board w h)))
    ;; Apply root setup to board
    (if game-tree
        (igo-board-changes-apply (igo-node-get-setup-property game-tree) board))

    (vector
     nil ;;finished
     board ;;board
     nil ;;undo stack (black prison positions,  white prison position, current play, ko, turn . .)
     root-node ;;game tree
     root-node ;;current node
     )))

;; editor object: ref igo-editor
;; [#<overlay from 1 to 1 in foo.sgf> nil nil nil (nil) nil  ((0 . 1) . Unexpected end of SGF (expecting '(')) text-auto-recovery nil (:show-status-bar t :show-branches t :show-move-number nil :show-last-move t :editable t :allow-illegal-move nil :move-opposite-color nil :grid-interval nil) ((keymap-change igo-sgf-mode-on-keymap-change) (text-mode igo-sgf-mode-update-buffer-read-only) (graphical-mode igo-sgf-mode-update-buffer-read-only)) nil])
;; (editor (vector
;;                   ov ;;0:overlay
;;                   nil ;;1:game
;;                   nil ;;2:layout (includes board-view and iamge-scale)
;;                   nil ;;3:svg
;;                   (list nil) ;;4:image-input-map
;;                   nil ;;5:image
;;                   nil ;;6:buffer text last updated (see: update-buffer-text)
;;                   nil ;;7:last error
;;                   nil ;;8:text mode
;;                   nil ;;9:current mode
;;                   (list
;;                    :show-status-bar igo-editor-status-bar-visible
;;                    :show-branches t
;;                    :show-move-number nil
;;                    :show-last-move t
;;                    ;;:rotate180 nil
;;                    :editable t
;;                    :allow-illegal-move nil
;;                    :move-opposite-color nil
;;                    :grid-interval nil
;;                    ) ;;10:properties
;;                   (list
;;                    (cons 'keymap-change nil)
;;                    (cons 'text-mode nil)
;;                    (cons 'graphical-mode nil)
;;                    ) ;;11:hooks
;;                   nil ;;12:copied node
;;                   )))
