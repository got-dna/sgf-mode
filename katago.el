(require 'sgf-util)

(defvar katago-exe "katago"
  "The path to the KataGo executable.")

(defvar katago-model
  "/opt/homebrew/Cellar/katago/1.15.3/share/katago/g170-b30c320x2-s4824661760-d1229536699.bin.gz"
  "The path to the KataGo model file.")

(defvar katago-analysis-config
  "/opt/homebrew/Cellar/katago/1.15.3/share/katago/configs/analysis_example.cfg"
  "The path to the KataGo analysis config file.")

(defvar katago-gtp-config
  "/opt/homebrew/Cellar/katago/1.15.3/share/katago/configs/gtp_example.cfg"
  "The path to the KataGo gtp configuration file.")

(defvar katago-log-buffer "*katago-log*"
  "The buffer name to log katago message.")

(defvar katago-out-buffer "*katago-out*"
  "The buffer name to log katago output message.")

(defvar katago-analysis-process nil)

(defvar katago-analysis-callback
  (lambda (turnNumber moves)
    (message "KataGo analysis: %s %s" turnNumber moves))
  "Callback function to handle processed katago analysis response.")

(defun katago-analysis-init (&optional exe model config)
  "Start a KataGo analysis process."
  (let* ((exe (or exe katago-exe))
         (model (or model katago-model))
         (config (or config katago-analysis-config))
         (command (list exe "analysis" "-model" model "-config" config))
         (log-buffer (get-buffer-create katago-log-buffer))
         (out-buffer (get-buffer-create katago-out-buffer))
         (result (make-hash-table)))
    (setq katago-analysis-process (make-process
                                   :name "katago-analysis"
                                   :buffer out-buffer
                                   :stderr log-buffer
                                   :command command
                                   :connection-type 'pipe
                                   :filter 'katago-analysis-filter)))
    (message "KataGo started with model %s and config %s." model config))


(defun katago-gtp-init (&optional exe model config)
  "Start a KataGo gtp process."
  (let* ((exe (or exe katago-exe))
         (model (or model katago-model))
         (config (or config katago-gtp-config))
         (command (list exe "gtp" "-model" model "-config" config))
         (log-buffer (get-buffer-create katago-log-buffer))
         (process (make-process
                   :name "katago-gtp"
                   :buffer log-buffer
                   :command command
                   :connection-type 'pipe
                   :filter 'katago-gtp-filter)))
    (message "KataGo started with model %s and config %s." model config)
    process))

(defun katago--pos-to-xy (pos)
  "Convert a POSITION in the KataGo format 'A1' to `(0 . 0)'.

The position is in the GTP format: https://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html#SECTION000311000000000000000"
  (let* ((x-pos (string-to-char (upcase (substring pos 0 1))))
         (x (- (if (> x-pos ?I) (1- x-pos) x-pos) ?A))
         (y (1- (string-to-number (substring pos 1)))))
    (cons x y)))


(defun katago-analysis-filter (process output)
  "Process filter for KataGo analysis output.

It parses the output from KataGo analysis and stores the result in the
process property."
  (with-current-buffer (process-buffer process)
    (insert output))
  (let ((json-object (json-read-from-string output)))
    ;; (message "KataGo analysis output: %s" output)
    (when json-object
      (let* ((res (katago-analysis-handle-response json-object))
             (turnNumber (car res))
             (moves (cdr res)))
        (funcall katago-analysis-callback turnNumber moves)))))


(defun katago-analysis-handle-response (response)
  "Handle a single response (a turnNumber) from KataGo analysis."
  (let ((id (cdr (assoc 'id response)))
        (turnNumber (cdr (assoc 'turnNumber response)))
        (moveInfos (cdr (assoc 'moveInfos response))))
      (cons turnNumber (mapcar 'katago-analysis-extract-move moveInfos))))

(defun katago-analysis-extract-move (move)
  (let* ((pos (cdr (assoc 'move move)))
         (xy (katago--pos-to-xy pos))
         (pv (mapcar 'katago--pos-to-xy (cdr (assoc 'pv move))))
         (winrate (cdr (assoc 'winrate move)))
         (score (cdr (assoc 'scoreLead move)))
         (visits (cdr (assoc 'visits move))))
    (cons xy (list :pv pv :winrate (* 100 winrate) :score score :visits visits))))
    ;; (insert (format "  %s: winrate %.2f%% score %.2f (%d visits) pv %s\n"
    ;;                 xy
    ;;                 (* 100 winrate)
    ;;                 score
    ;;                 visits pv))))

(defun katago-analysis-query (query callback)
  (setq katago-analysis-callback callback)
  (process-send-string katago-analysis-process (concat query "\n")))

(defun katago-gtp-filter (process output)
  "Filter function to handle output from KataGo."
  (message "KataGo gtp output: %s" output))


(defvar katago-gtp-history nil
  "History of GTP commands sent to KataGo.

Explicitly define this variable to initialize it with some default
values or to document its purpose. However, this is not required for the
functionality to work. Emacs will handle it automatically if you don't
define it.")

(defun katago-gtp-send-cmd (command)
  "Send a GTP COMMAND to KataGo.
If the command has a dedicated wrapper function, call that instead."
  (interactive
   (list (completing-read
          "Enter KataGo GTP command: "
          '("boardsize"
            "clear_board"
            "showboard"
            "version"
            "genmove"
            "play"
            "kata-analyze"
            "stop"
            "quit")
          nil nil nil 'katago-gtp-history)))
  (cond
   ((string= command "quit") (katago-gtp-quit))
   ((string= command "kata-analyze") (katago-gtp-analyze))
   ((string= command "play") (katago-gtp-play))
   ((string= command "genmove") (katago-gtp-genmove))
   (t (when (process-live-p katago-process)
        (process-send-string katago-process (concat command "\n"))
        (message "Sent command: %s" command)))))


(defun katago-gtp-genmove (&optional color)
  "Ask KataGo to generate a move for the given COLOR (black or white).
If called interactively, prompt the user for the color."
  (interactive)
  (katago-gtp-send-cmd
   (format "genmove %s"
           (completing-read "Enter stone color (black/white): "
                            '("black" "white")))))

(defun katago-gtp-play (&optional move)
  "Play a MOVE in the format 'color position' (e.g., 'black A1').
If called interactively, prompt the user for the move with completion."
  (interactive)
  (katago-gtp-send-cmd
   (format "play %s"
           (let ((color (completing-read "Enter color (black/white): "
                                         '("black" "white")))
                 (position (read-string "Enter position (e.g., A1): ")))
             (format "%s %s" color position)))))


(defun katago-gtp-analyze (&optional visits)
  "Run KataGo analysis with a specified number of VISITS.
If called interactively, prompt the user for the number of visits."
  (interactive "P")
  ;; Default to 100 visits if not provided
  (unless visits (setq visits 100))
  (katago-gtp-send-cmd (format "kata-analyze %d" visits))
  (run-with-timer 2 nil (lambda () (katago-gtp-send-cmd "stop"))))


(defun katago-gtp-parse-analysis (output)
  "Parse the analysis OUTPUT from KataGo."
  (setq katago-analysis nil)
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (while (re-search-forward "info move \\(\\w+\\) visits \\(\\d+\\) winrate \\(\\d+\\.\\d+\\)% scoreLead \\([-+]?\\d+\\.\\d+\\)" nil t)
      (let ((move (match-string 1))
            (visits (string-to-number (match-string 2)))
            (winrate (string-to-number (match-string 3)))
            (scoreLead (string-to-number (match-string 4))))
        (push (list :move move :visits visits :winrate (* 100 winrate) :scoreLead scoreLead)
              katago-analysis))))
  (setq katago-analysis (reverse katago-analysis)) ; Reverse to maintain order
  (message "Parsed analysis: %S" katago-analysis))


(defun katago-process-quit (&optional process)
  "Quit the katago gtp or analysis process."
  (interactive)
  (let ((process (or process katago-analysis-process)))
    (when (process-live-p process)
      ;; (process-send-string process "quit\n")
      (delete-process process)
      (message "KataGo process killed."))))


(provide 'katago)
