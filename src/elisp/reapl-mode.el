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

;;; Code:

(defvar reapl-mode-map (make-sparse-keymap))

(define-derived-mode reapl-mode
  fennel-mode
  "reapl mode"
  :keymap reapl-mode-map
  (add-to-list 'completion-at-point-functions 'reapl-mode_complete))

;; connection
;; --------------------------------------------

(defvar reapl-mode_server-port 9999)
(defvar reapl-mode_send-proc nil)

(defun reapl-mode_connect-to-reaper ()
  "Connect to the remote reaper server."
  (setq reapl-mode_send-proc
        (make-network-process
         :name "*reapl*"
         :host "localhost"
         :service reapl-mode_server-port
         :type 'datagram
         :family 'ipv4)))

;; evaluation
;; --------------------------------------------

(defvar reapl-mode_history nil)
(defvar reapl-mode_receive-port 9997)
(defvar reapl-mode_receive-proc nil)
(defvar reapl-mode_completions (list))

(defun reapl-mode_prettify-json-string (json-str)
  "Prettify the given JSON-STR."
  (let ((json-data (json-parse-string json-str)))
   (with-temp-buffer
     (insert (json-encode json-data))
     (json-pretty-print (point-min) (point-max))
     (buffer-string))))

(defun reapl-mode_insert-with-highlighting (mode str)
  "Insert STR into the current buffer with syntax highlighting from given MODE."
  (let ((formatted (with-temp-buffer
                     (insert str)
                     (funcall mode)
                     (font-lock-ensure)
                     (buffer-string))))
    (insert formatted)))

(defun reapl-mode_insert-line (buffer)
  "Insert a line in BUFFER."
  (let* ((start (point))
         (end (progn (insert (make-string (window-width (get-buffer-window buffer)) ?\s)) (point)))) ; insert a string of spaces
    (put-text-property start end 'face `(:underline ,(face-foreground 'font-lock-comment-face)))))

(defun reapl-mode_print-evaluation-result (msg)
  "Handle the result of an eval operation. MSG is an alist decoded from json."
  (let ((buffer (process-buffer reapl-mode_receive-proc)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n\n")
      ;; insert input expression
      (reapl-mode_insert-with-highlighting
       'fennel-mode (alist-get 'expression msg))
      (insert (propertize "\n\n=>\n\n" 'face 'font-lock-comment-face))
      ;; insert return
      (reapl-mode_insert-with-highlighting
       'json-mode (reapl-mode_prettify-json-string (json-encode (alist-get 'output msg))))
      (insert "\n")
      ;; insert a line at the end
      (reapl-mode_insert-line buffer))))

(defun reapl-mode_print-command-result (msg)
  "Handle the result of a command. MSG is an alist decoded from json."
  (let* ((buffer (process-buffer reapl-mode_receive-proc)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n\n")
      ;; insert input expression
      (reapl-mode_insert-with-highlighting
       'fennel-mode (format "%s" (list 'reapl:cmd (alist-get 'op msg) (alist-get 'arg msg))))
      (insert (propertize "\n\n=>\n\n" 'face 'font-lock-comment-face))
      ;; insert return
      (reapl-mode_insert-with-highlighting
       'json-mode (reapl-mode_prettify-json-string (json-encode (alist-get 'output msg))))
      (insert "\n")
      ;; insert a line at the end
      (reapl-mode_insert-line buffer))))

(defun reapl-mode_on-receive (_ msg)
  "Handle the received messages MSG."
  (let* ((json-object-type 'alist)
         (json-msg (json-read-from-string msg))
         (op (alist-get 'op json-msg)))
    (setq reapl-mode_history
          (cons json-msg reapl-mode_history))
    (cond ((string-equal op "eval") (reapl-mode_print-evaluation-result json-msg))
          ((string-equal op "complete") (setq reapl-mode_completions json-msg))
          (t (reapl-mode_print-command-result json-msg)))))

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
         :type 'datagram
         :filter #'reapl-mode_on-receive
         :sentinel (lambda (_ _event)))))

;; completions
;; --------------------------------------------

(defun reapl-mode_completion-details (item)
  "Find details about ITEM completion."
  (let ((type (alist-get (intern item) (alist-get 'types reapl-mode_completions))))
    (list :type type
          :kind (cond ((eq type "function") 'function)
                      ((eq type "table") 'module)
                      (t 'variable)))))

(defun reapl-mode_complete ()
  "Completion at point function."
  (interactive)
  (reapl-mode_complete-symbol-at-point)
  (sit-for 0.1)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when reapl-mode_completions
      (list (car bounds)
            (cdr bounds)
            (append (alist-get 'completions reapl-mode_completions)
                    nil)
            :annotation-function
            (lambda (item)
              (plist-get (reapl-mode_completion-details item)
                         :type))
            :company-kind
            (lambda (item)
              (plist-get (reapl-mode_completion-details item)
                         :kind))))))

;; API
;; --------------------------------------------

(defun reapl-mode_repl-quit ()
  "Connect to the reaper server."
  (interactive)
  (when reapl-mode_send-proc
    (delete-process reapl-mode_send-proc))
  (when reapl-mode_receive-proc
    (let ((buffer (process-buffer reapl-mode_receive-proc)))
      (delete-process reapl-mode_receive-proc)
      (kill-buffer buffer))))

(defun reapl-mode_connect ()
  "Connect to the reaper server."
  (interactive)
  (reapl-mode_repl-quit)
  (reapl-mode_connect-to-reaper)
  (reapl-mode_start-evaluation-proc)
  (display-buffer (get-buffer "*reapl-evaluation*")))

(defun reapl-mode_repl ()
  "Connect to the reaper server."
  (interactive)
  (when (not reapl-mode_send-proc)
    (reapl-mode_connect-to-reaper))
  (when (not reapl-mode_receive-proc)
    (reapl-mode_start-evaluation-proc))
  (display-buffer (get-buffer "*reapl-evaluation*")))


(defun reapl-mode_send-message (op arg)
  "Send the message {op: OP, arg: ARG} to the remote reaper server."
  (process-send-string reapl-mode_send-proc
                       (json-encode-plist
                        (list :op op :arg arg))))

(defun reapl-mode_request-evaluation (s)
  "Request the evaluation of string S to the remote reaper server."
  (reapl-mode_send-message :eval s))

(defun reapl-mode_request-completion (s)
  "Request the completion of strng S to the remote reaper server."
  (reapl-mode_send-message :complete s))

(defun reapl-mode_request-documentation (s)
  "Request the documentation for S to the remote reaper server."
  (reapl-mode_send-message :doc s))

(defun reapl-mode_thing-at-point ()
  "Return the thing at point.
This can be either a word, symbol, or sexp, in that order of preference."
  (or (thing-at-point 'word t)
      (thing-at-point 'symbol t)
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

(provide 'reapl-mode)
;;; reapl-mode.el ends here
