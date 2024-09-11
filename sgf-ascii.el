;;; board.el -*- lexical-binding: t -*-

(defvar black-piece "X")
(defvar white-piece "O")

(defun board-pos-to-str (board pos)
  ;; text property is used to svg conversion
  (let* ((size (board-size board))
         (hoshi-locations (cond ((= 19 size) '(3 9 15))
                                ((= 13 size) '(3 9))
                                ((= 9 size)  '(2 6))))
         (row-idx (car pos))
         (col-idx (cdr pos))
         (val (aref board (pos-to-idx pos size)))
         (str (cond ((equal val :W) white-piece)
                    ((equal val :B) black-piece)
                    ((and (stringp val) (= 1 (length val)) val))
                    (t  (if (and (member row-idx hoshi-locations)
                                 (member col-idx hoshi-locations))
                            "+"
                          ".")))))
    (put-text-property 0 1
                       :type
                       (cons (cond ;; item
                              ((string= str white-piece) :white)
                              ((string= str black-piece) :black)
                              ((string= str "+")         :hoshi)
                              ((string= str ".")         :background)
                              (t                         :background))
                             (cond ;; face
                              ((and (= 0 row-idx) (= 0 col-idx))                 :bl)
                              ((and (= 0 row-idx) (= (1- size) col-idx))         :br)
                              ((and (= (1- size) row-idx) (= 0 col-idx))         :tl)
                              ((and (= (1- size) row-idx) (= (1- size) col-idx)) :tr)
                              ((= 0 row-idx)                                     :b)
                              ((= (1- size) row-idx)                             :t)
                              ((= 0 col-idx)                                     :l)
                              ((= (1- size) col-idx)                             :r)
                              (t nil)))
                       str)
    (put-text-property 0 1 :pos pos str)
    str))


(defun board-row-to-str (board row)
  (let* ((size (board-size board))
         (row-idx (format "%2d" (1+ row)))
         (filler " "))
    (put-text-property 0 1 :type (cons :background nil) filler)
    (concat row-idx
            " "
            (mapconcat (lambda (i) (board-pos-to-str board (cons row i)))
                       (number-sequence 0 (1- size))
                       filler)
            " "
            row-idx)))


(defun board-to-string (board)
  (let ((column-indices (board-header board))
        (body (mapconcat (lambda (i) (board-row-to-str board i))
                         (number-sequence (1- (board-size board)) 0 -1) "\n")))
    (mapconcat #'identity '(column-indices body column-indices) "\n")))


(defun board-asscii (board)
  (interactive "r")
  (cl-flet ((ov (point face &optional back)
             (let ((ovly (make-overlay point (1+ point))))
               (overlay-put ovly 'go-pt point)
               (overlay-put ovly 'face (sym-cat 'go-board face))
               (when go-board-use-images
                 (overlay-put ovly 'display
                              (if (equal face 'filler)
                                  '(space :width (18))
                                (eval (sym-cat 'go-board 'image face back)))))
               (push ovly *go-board-overlays*)))
         (hide (point)
               (let ((ovly (make-overlay point (1+ point))))
                 (overlay-put ovly 'invisible t)
                 (push ovly *go-board-overlays*))))
    (let ((start (or start (point-min)))
          (end   (or end   (point-max))))
      (dolist (point (range start end))
        (if (get-text-property point :turn)
            (font-lock-prepend-text-property point (1+ point) 'face 'underline)
          (let ((back (case (cdr (get-text-property point :type))
                        (:tl 'top-left)
                        (:tr 'top-right)
                        (:bl 'bottom-left)
                        (:br 'bottom-right)
                        (:t  'top)
                        (:b  'bottom)
                        (:l  'left)
                        (:r  'right)
                        (:offboard 'offboard))))
            (case (car (get-text-property point :type))
              (:header       nil)
              (:filler       (ov point 'filler back))
              (:hoshi        (ov point 'hoshi))
              (:white        (ov point 'white back))
              (:black        (ov point 'black back))
              (:background  (if go-board-use-images
                                (hide point)
                              (ov point 'background)))
              (:background-1 (ov point 'background back)))))))))


;;; board tests
(ert-deftest go-empty-board-to-string-test ()
  (let ((board (make-vector (* 19 19) nil))
        (exp    (concat "    A B C D E F G H J K L M N O P Q R S T   \n"
                        " 19 . . . . . . . . . . . . . . . . . . . 19\n"
                        " 18 . . . . . . . . . . . . . . . . . . . 18\n"
                        " 17 . . . . . . . . . . . . . . . . . . . 17\n"
                        " 16 . . . + . . . . . + . . . . . + . . . 16\n"
                        " 15 . . . . . . . . . . . . . . . . . . . 15\n"
                        " 14 . . . . . . . . . . . . . . . . . . . 14\n"
                        " 13 . . . . . . . . . . . . . . . . . . . 13\n"
                        " 12 . . . . . . . . . . . . . . . . . . . 12\n"
                        " 11 . . . . . . . . . . . . . . . . . . . 11\n"
                        " 10 . . . + . . . . . + . . . . . + . . . 10\n"
                        "  9 . . . . . . . . . . . . . . . . . . .  9\n"
                        "  8 . . . . . . . . . . . . . . . . . . .  8\n"
                        "  7 . . . . . . . . . . . . . . . . . . .  7\n"
                        "  6 . . . . . . . . . . . . . . . . . . .  6\n"
                        "  5 . . . . . . . . . . . . . . . . . . .  5\n"
                        "  4 . . . + . . . . . + . . . . . + . . .  4\n"
                        "  3 . . . . . . . . . . . . . . . . . . .  3\n"
                        "  2 . . . . . . . . . . . . . . . . . . .  2\n"
                        "  1 . . . . . . . . . . . . . . . . . . .  1\n"
                        "    A B C D E F G H J K L M N O P Q R S T   "
                        )))
    (should (string= exp (substring-no-properties (board-to-string board))))))



(defun move-type (move)
  (cond
   ((member (car move) '(:B  :W))  :move)
   ((member (car move) '(:LB :LW)) :label)))

(defun other-color (color)
  (if (equal color :B) :W :B))


(defun alive-p (board piece &optional already)
  (let* ((val (aref board piece))
         (enemy (other-color val))
         (neighbors (remove-if (lambda (n) (member n already))
                               (neighbors board piece)))
         (neighbor-vals (mapcar (lambda (n) (aref board n)) neighbors))
         (friendly (delete nil (mapcar
                                (lambda (n) (when (equal (aref board n) val) n))
                                neighbors)))
         (already (cons piece already)))
    (or (some (lambda (v) (not (or (equal v enemy) ; touching open space
                                   (equal v val))))
              neighbor-vals)
        (some (lambda (n) (alive-p board n already)) ; touching alive dragon
              friendly))))

(defun remove-dead (board color)
  ;; must remove one color at a time for ko situations
  (let (cull)
    (dotimes (n (length board) board)
      (when (and (equal (aref board n) color) (not (alive-p board n)))
        (push n cull)))
    (cl-incf (go-player-get (other-color color) :prisoners) (length cull))
    (dolist (n cull cull) (setf (aref board n) nil))))


(defun board-apply-move (board move)
  (cl-flet ((bset (val data)
               (let ((data (if (listp (car data)) data (list data))))
                 (setf (aref board (pos-to-idx (aget data :pos)
                                                 (board-size board)))
                       (case val
                         (:B  :B)
                         (:W  :W)
                         (:LB (aget data :label))
                         (:LW (aget data :label))
                         (t nil))))))
    (case (move-type move)
      (:move
       (bset (car move) (cdr move))
       (let ((color (if (equal :B (car move)) :B :W)))
         (remove-dead board (other-color color))
         (remove-dead board color)))
      (:label
       (dolist (data (cdr move)) (bset (car move) data))))))


;;; Display mode
(defvar go-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") 'go-board-mouse-move)
    (define-key map (kbd "m") 'go-board-move)
    (define-key map (kbd "r") 'go-board-refresh)
    (define-key map (kbd "R") 'go-board-resign)
    (define-key map (kbd "u") 'go-board-undo)
    (define-key map (kbd "c") 'go-board-comment)
    (define-key map (kbd "l") 'go-board-level)
    (define-key map (kbd "p") 'go-board-pass)
    (define-key map (kbd "<right>") 'go-board-next)
    (define-key map (kbd "<left>")  'go-board-undo)
    (define-key map (kbd "q") 'go-board-quit)
    map)
  "Keymap for `go-board-mode'.")

(define-derived-mode go-board-mode nil "GO"
  "Major mode for viewing a GO board."
  (set (make-local-variable 'kill-buffer-query-functions)
       (add-to-list 'kill-buffer-query-functions 'go-board-safe-quit)))
