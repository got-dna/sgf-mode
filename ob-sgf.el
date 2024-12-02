;;; ob-sgf.el --- sgf block in org-mode -*- lexical-binding: t -*-

;; Author: Zech Xu <
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;; This file is not part of GNU Emacs


;;; Commentary:

;; Implement SGF code block in org-mode using template at
;; https://orgmode.org/worg/org-contrib/babel/languages/index.html#develop.
;; It defines new header args:

;; 1. :show-number - t or nil value to show move number on the game board or not.
;; 2. :show-mark - t or nil value to show marks on the board or not.
;; 3. :show-next - t or nil value to show hint mark(s) for next move on the board or not.
;; 4. :traverse-path - See `sgf-traverse-path' value to traverse to the game state.


;; It takes advantage of `C-c C-c' to display graphics of game board.
;; Use `C-u C-c C-c' to delete existing overlay and recreate the game.



;;; Code:
(require 'sgf-io)
(require 'sgf-mode)

(require 'ob)
;; (require 'ob-ref)
;; (require 'ob-comint)
;; (require 'ob-eval)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("SGF" . "sgf"))


;; optionally declare default header arguments for this language
;; set cache so that overlay is recreated only upon C-u C-c C-c.
(defvar org-babel-default-header-args:sgf '())


(defun org-src-block-value-begin-end ()
  "Return the beginning and end of the value (ie the code content) of the
current source block."
  (require 'org-element)
  (let* ((element (org-element-at-point))
         (end (org-element-end element))
         (post-affiliated (org-element-post-affiliated element))
         (post-blank (org-element-post-blank element))
         val-beg val-end)
    (save-excursion
      (goto-char post-affiliated)
      (forward-line 1)
      (sgf-parse-skip-ws)
      (setq val-beg (point))
      (goto-char end)
      (forward-line (- (1+ post-blank)))
      ;; `1-' to move to the end of the previous line
      (setq val-end (1- (point))))
    (cons val-beg val-end)))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:sgf (_body params)
  "Execute a block of SGF code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing SGF block to play game...")
  (let* ((game-plist (sgf-default-game-plist))
         (beg-end (org-src-block-value-begin-end))
         (processed-params (org-babel-process-params params)))
    ;; (message "params:\n%S" processed-params)
    (dolist (param processed-params)
      (let* ((key (car param))
             (val (cdr param)))
        (if (plist-member game-plist key)
            ;; val is str - convert it to elisp object with `read'
            (sgf-update-game-plist game-plist key (read val)))))
    ;; (message "game-plist:\n%S" game-plist)
    ;; (message "begin-end: %S" beg-end)
    ;; (sgf-setup-game-display (string= cache "no") (car beg-end) (cdr beg-end) game-plist)
    (sgf-toggle-game-display (car beg-end) (cdr beg-end) game-plist)))

(provide 'ob-sgf)

;;; ob-sgf.el ends here
