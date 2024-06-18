;;; reapl-mode.el --- Talking to Reaper using Fennel -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 pierre baille
;;
;; Author: pbaille
;; Maintainer: pbaille
;; Created: June 04, 2024
;; Modified: June 04, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/pierrebaille/reapl-mode
;; Package-Requires: ((emacs "27.1") (fennel-mode "0.9.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Talking to the Reaper DAW using Fennel.
;;
;;; Code:

(require 'comint)
(require 'fennel-mode)
(require 'json)
(require 'company)

(defvar reapl-mode-map (make-sparse-keymap))

(define-derived-mode reapl-mode
  fennel-mode
  "reapl mode"
  :keymap reapl-mode-map
  ; (add-to-list 'completion-at-point-functions 'reapl-mode_complete)
  (add-to-list 'eldoc-documentation-functions 'reapl-mode_eldoc)
  (add-to-list 'company-backends 'reapl-mode_complete))

;; connection
;; --------------------------------------------

(defvar reapl-mode_repl-port 9999)
(defvar reapl-mode_repl-proc nil)
(defvar reapl-mode_action-port 9998)
(defvar reapl-mode_action-proc nil)

(defun reapl-mode_connect-to-reaper ()
  "Connect to the remote reaper server."
  (setq reapl-mode_repl-proc
        (make-network-process
         :name "*reapl*"
         :host "localhost"
         :service reapl-mode_repl-port
         :type 'datagram
         :family 'ipv4))

  (setq reapl-mode_action-proc
    (make-network-process
     :name "*reapl-action*"
     :host "localhost"
     :service reapl-mode_action-port
     :type 'datagram
     :family 'ipv4)))

;; evaluation
;; --------------------------------------------

(defvar reapl-mode_history nil)
(defvar reapl-mode_receive-port 9997)
(defvar reapl-mode_receive-proc nil)
(defvar reapl-mode_completions (list))

(progn
  (defun reapl-mode_prettify-json-string (json-str)
    "Prettify the given JSON-STR."
    (let ((json-data (json-parse-string json-str)))
      (with-temp-buffer
        (insert (json-encode json-data))
        (json-pretty-print (point-min) (point-max))
        (buffer-string))))

  (defun reapl-mode_highlight-str (mode str)
    "Highlight STR with syntax highlighting from given MODE."
    (with-temp-buffer
      (insert str)
      (funcall mode)
      (font-lock-ensure)
      (buffer-string)))

  (defun reapl-mode_insert-line (buffer)
    "Insert a line in BUFFER."
    ;; this expression was initially used to compute width
    '(window-width (get-buffer-window buffer))
    (let* ((start (point))
           (end (progn (insert (make-string 80 ?\s)) (point)))) ; insert a string of spaces
      (put-text-property start end 'face `(:underline ,(face-foreground 'font-lock-comment-face)))))

  (defun reapl-mode_count-indentation (line)
    "Count leading spaces in a LINE."
    (if (string-match "^\\s-+" line)
        (length (match-string 0 line))
      0))

  (defun reapl-mode_remove-leading-spaces (str n)
    "Remove the first N space characters from the beginning of STR."
    (let ((result str)
          (count n))
      (while (and (> count 0) (string-prefix-p " " result))
        (setq result (substring result 1))
        (setq count (1- count)))
      result))

  (defun reapl-mode_formatted-doc (doc-str)
    "Parse docstring (DOC-STR) into `(form . docstring)`."
    (let* ((splitted (split-string doc-str "\n"))
           (form (car splitted))
           (lines (cdr splitted))
           (line-len (length lines)))
      (cons (reapl-mode_highlight-str 'fennel-mode form)
            (propertize (cond ((eq line-len 0) "")
                              ((eq line-len 1) (car lines))
                              ;; if multi-line docstring, we are fixing potential indentation issues
                              (t (let* ((first-line (car lines))
                                        (second-line (cadr lines))
                                        (first-indentation (reapl-mode_count-indentation first-line))
                                        (second-indentation (reapl-mode_count-indentation second-line))
                                        (indent-difference (- second-indentation first-indentation))
                                        (trimmed-lines (mapcar (lambda (s) (reapl-mode_remove-leading-spaces s indent-difference)) (cdr lines)))
                                        (doc-txt (mapconcat 'identity (cons first-line trimmed-lines) "\n")))
                                   doc-txt)))
                        'face 'font-lock-function-name-face ))))

  (defun reapl-mode_output-first-value (msg)
    "Extract the first value from the reapl server response message."
    (let ((output (plist-get msg :data)))
      (if-let* ((values (plist-get output :values)))
          (car values)
        (plist-get output :value))))

  (defun reapl-mode_insert-json-output (msg)
    "Insert the output value of the reapl server response (MSG) as highlighted JSON."
    (insert (reapl-mode_highlight-str
             'json-mode (reapl-mode_prettify-json-string (json-encode (plist-get msg :data))))))

  (defun reapl-mode_insert-reapl-command (msg)
    "Insert a reapl server command request."
    (insert (reapl-mode_highlight-str
             'fennel-mode (format "%s" (concat (symbol-name (plist-get msg :op)) " " (plist-get msg :data))))))

  (defun reapl-mode_insert-formatted-doc-output (doc-str)
    "Insert formatted docstring (DOC-STR) into current buffer."
    (let ((formatted (reapl-mode_formatted-doc doc-str)))
      (insert (car formatted))
      (insert "\n\n")
      (insert (cdr formatted))))

  (defun reapl-mode_wrap-insertion-fn (req f)
    (lambda (res)
      (let* ((buffer (process-buffer reapl-mode_receive-proc)))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "\n\n")
          ;; insert input expression
          (reapl-mode_insert-reapl-command req)
          ;; input/output separator
          (insert (propertize "\n\n=>\n\n" 'face 'font-lock-comment-face))
          ;; insert return
          (funcall f res)

          (insert "\n")
          ;; insert a line at the end
          (reapl-mode_insert-line buffer)))))

  (defun reapl-mode_make-default-request-handler (req)
    (reapl-mode_wrap-insertion-fn req #'reapl-mode_insert-json-output))

  (defun reapl-mode_make-doc-request-handler (req)
    (reapl-mode_wrap-insertion-fn req
                                  (lambda (res)
                                    (reapl-mode_insert-formatted-doc-output (reapl-mode_output-first-value res)))))

  (defun reapl-mode_insert-evaluation-result (msg)
      (let* ((output (plist-get msg :data))
             (err (plist-get output :error))
             (value-str (plist-get output :value-str))
             (value (plist-get output :value)))
        (cond (err
               (insert (propertize (concat (plist-get err :type) " error:\n\n"
                                           (plist-get err :message))
                                   'face 'font-lock-warning-face)))
              (value-str
               (insert (reapl-mode_highlight-str 'reapl-mode value-str)))
              (value
               (insert (reapl-mode_highlight-str
                        'json-mode (reapl-mode_prettify-json-string (json-encode value))))))))

  (defun reapl-mode_make-evaluation-request-handler (req)
    (reapl-mode_wrap-insertion-fn req #'reapl-mode_insert-evaluation-result)))

(defvar reapl-mode_callbacks ())

(defun reapl-mode_on-receive (msg)
  "Handle the received messages MSG."
  (setq reapl-mode_history
        (cons msg reapl-mode_history))
  (let* ((id (plist-get msg :id))
         (callback (alist-get id reapl-mode_callbacks
                              nil nil #'equal)))
    ;(print "handling ")
    ;(print msg)
    (funcall callback msg)
    (setq reapl-mode_callbacks
          (cl-delete id reapl-mode_callbacks
                     :key #'car
                     :test #'equal))
    (if-let* ((buf (process-buffer reapl-mode_receive-proc))
              (win (get-buffer-window buf)))
        (with-selected-window win
          (goto-char (point-max))
          (recenter -1)))))

(defun reapl-mode_mk-proc-filter (handler)
  "Build a process filter function given an HANDLER plist."
  (let* ((default-state (list :id nil :data ""))
         (state default-state))
    (lambda (_ string)
      (let ((json-object-type 'plist)
            (json-array-type 'list))
        (let* ((json-message (json-read-from-string string))
               (id (plist-get json-message :id))
               (op (plist-get json-message :op))
               (data (plist-get json-message :data)))
          (when id
            (unless (equal (plist-get state :id) id)
              (setq state default-state))
            (cond ((and data (not op))
                   (setq state
                         (list :id id
                               :data (concat (plist-get state :data) data))))
                  (op
                   (let* ((msg (if (equal (plist-get state :id) id)
                                   (plist-put json-message
                                              :data (json-read-from-string
                                                     (plist-get state :data)))
                                 json-message))
                          (ret (funcall handler msg)))
                     (setq state default-state)
                     ret))
                  (t
                   (setq state default-state)
                   (error (format "Bad format msg: %s" json-message))))))))))

(defun reapl-mode_start-evaluation-proc ()
  "Start the completion process."
  (setq reapl-mode_receive-proc
        (make-network-process
         :name "reapl-evaluation-proc"
         :buffer "*reapl-evaluation*"
         :host 'local
         :service reapl-mode_receive-port
         :server t
         :family 'ipv4
         :type 'datagram))
  (set-process-filter reapl-mode_receive-proc
                      (reapl-mode_mk-proc-filter #'reapl-mode_on-receive)))

;; completions
;; --------------------------------------------

(defun reapl-mode_completion-details (item)
  "Find details about ITEM completion."
  (let* ((types (plist-get reapl-mode_completions :types))
         (type (plist-get types (intern (format ":%s" item)))))
    (list :type type
          :kind (cond ((eq type "function") 'function)
                      ((eq type "table") 'module)
                      (t (or type 'variable))))))

(defun reapl-mode_complete (command &optional arg &rest ignored)
  "Company backend for reapl-mode. COMMAND, ARG, IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'reapl-mode_complete))
    (prefix (and (eq major-mode 'reapl-mode)
                 (reapl-mode_thing-at-point)))
    (candidates
     (cons :async
           (lambda (callback)
             (reapl-mode_send-message :complete arg
                                      (lambda (msg)
                                        '(print `(company-async ,msg))
                                        (setq reapl-mode_completions (plist-get msg :data))
                                        (funcall callback (plist-get reapl-mode_completions :completions)))))))
    (annotation (plist-get (reapl-mode_completion-details arg)
                           :type))
    (kind (plist-get (reapl-mode_completion-details arg)
                     :kind))))

(defun reapl-mode_request-doc-for-completion-candidate ()
  "Request doc current company candidate."
  (interactive)
  (when (eq major-mode 'reapl-mode)
    (let ((candidate (nth company-selection company-candidates)))
      (reapl-mode_request-documentation candidate))))

(defun reapl-mode_eldoc (callback)
  "Build a eldoc hook for reapl-mode wrapping eldoc provided CALLBACK."
  (when (eq major-mode 'reapl-mode)
    (when-let ((sym (thing-at-point 'symbol t)))
      (reapl-mode_send-message :doc sym
                               (lambda (msg)
                                 (funcall callback
                                          (car (reapl-mode_formatted-doc
                                                (or (reapl-mode_output-first-value msg)
                                                    "")))))))))

;; API
;; --------------------------------------------

(defun reapl-mode_repl-quit ()
  "Connect to the reaper server."
  (interactive)
  (when (process-live-p reapl-mode_repl-proc)
    (delete-process reapl-mode_repl-proc))
  (when (process-live-p reapl-mode_receive-proc)
    (let ((buffer (process-buffer reapl-mode_receive-proc)))
      (delete-process reapl-mode_receive-proc)
      (mapc #'delete-window (get-buffer-window-list buffer))
      (kill-buffer buffer))))

(defun reapl-mode_connect ()
  "Connect to the reaper server."
  (interactive)
  (reapl-mode_repl-quit)
  (reapl-mode_connect-to-reaper)
  (reapl-mode_start-evaluation-proc))

(defun reapl-mode_repl ()
  "Connect to the reaper server."
  (interactive)
  (unless (process-live-p reapl-mode_repl-proc)
    (reapl-mode_connect-to-reaper))
  (unless (process-live-p reapl-mode_receive-proc)
    (reapl-mode_start-evaluation-proc))
  (display-buffer (get-buffer "*reapl-evaluation*")))


(defun reapl-mode_send-message (op data callback)
  "Send the message {id: 0, op: OP, data: DATA to the remote reaper server.
CALLBACK will be called with the response.."
  (let ((id (current-time-string)))
    (if callback
        (setq reapl-mode_callbacks
              (cons (cons id (or callback
                                 (reapl-mode_make-default-request-handler (list :op op :data data))))
                    reapl-mode_callbacks)))
    (when reapl-mode_repl-proc
      (process-send-string reapl-mode_repl-proc
                           (json-encode-plist
                            (list :id id :op op :data data))))))

(defun reapl-mode_request-evaluation (s)
  "Request the evaluation of string S to the remote reaper server."
  (reapl-mode_send-message :eval s
                           (reapl-mode_make-evaluation-request-handler (list :op :eval :data s))))

(defun reapl-mode_request-completion (s)
  "Request the completion of strng S to the remote reaper server."
  (reapl-mode_send-message :complete s
                           (lambda (msg) (setq reapl-mode_completions (plist-get msg :data)))))

(defun reapl-mode_request-documentation (s)
  "Request the documentation for S to the remote reaper server.
the SILENT optional arg is staying that the result should not be printed."
  (reapl-mode_send-message :doc s
                           (reapl-mode_make-doc-request-handler (list :op :doc :data s))))

(defun reapl-mode_thing-at-point ()
  "Return the thing at point.
This can be either a symbol, or sexp, in that order of preference."
  (or (thing-at-point 'symbol t)
      (thing-at-point 'sexp t)))

(defun reapl-mode_send-thing-at-point ()
  "Send the thing at point to the Reapl REPL."
  (interactive)
  (reapl-mode_request-evaluation (reapl-mode_thing-at-point)))

(defun reapl-mode_send-buffer ()
  "Send the entire buffer content to the Reapl REPL."
  (interactive)
  (reapl-mode_request-evaluation (buffer-substring-no-properties (point-min) (point-max))))

(defun reapl-mode_complete-symbol-at-point ()
  "Ask for completions for the symbol at point to the reaper REPL."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (when sym (reapl-mode_request-completion sym))))

(defun reapl-mode_doc-symbol-at-point ()
  "Ask for documentation for the symbol at point to the reaper REPL."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (when sym (reapl-mode_request-documentation sym))))

(add-to-list 'auto-mode-alist '("\\.reapl\\'" . reapl-mode))

(defun reapl-mode_send-action (str)
  "Send an action STR request to the server."
  (process-send-string reapl-mode_action-proc str))

(provide 'reapl-mode)
;;; reapl-mode.el ends here
