;;; sgf-org.el --- sgf block in org-mode -*- lexical-binding: t -*-

;; Author: Zech Xu <
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;; This file is not part of GNU Emacs


;;; Commentary:

;; Implement SGF code block in org-mode. It defines new header args. C-c C-c to display graphics.
;; Used template at https://orgmode.org/worg/org-contrib/babel/languages/index.html#develop


;;; Code:
(require 'sgf-mode)

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("SGF" . "sgf"))

;; create overlay object (call sgf-create-overlay)
;; retrieve header args and update game-plist on overlay

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:sgf '())

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:sgf' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:sgf (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-template nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-sgf-var-to-sgf (cdr pair))))
      vars "\n")
     "\n" body "\n")))


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
(defun org-babel-execute:sgf (body params)
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

    (sgf-start-the-game (car beg-end) (cdr beg-end) game-plist)

    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    ;;
    ;; for session based evaluation the functions defined in
    ;; `org-babel-comint' will probably be helpful.
    ;;
    ;; for external evaluation the functions defined in
    ;; `org-babel-eval' will probably be helpful.
    ;;
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)
    ))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:sgf (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-sgf-var-to-sgf (var)
  "Convert an elisp var into a string of sgf source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-sgf-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-sgf-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-sgf)

(provide 'sgf-org)
;;; sgf-org.el ends here
