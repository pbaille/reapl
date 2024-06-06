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
  :keymap reapl-mode-map)

;; connection
;; --------------------------------------------

(defvar reapl-mode_server-port 9999)
(defvar reapl-mode_reaper-connection nil)

(defun reapl-mode_connect-to-reaper ()
  "Connect to the remote reaper server."
  (setq reapl-mode_reaper-connection
        (make-network-process
         :name "*reapl*"
         :host "localhost"
         :service 9999
         :type 'datagram
         :family 'ipv4)))

;; evaluation
;; --------------------------------------------

(defvar reapl-mode_evaluation-history nil)
(defvar reapl-mode_evaluation-port 9997)
(defvar reapl-mode_evaluation-proc nil)

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

(defun reapl-mode_on-received-evaluation (_ msg)
  "Handle the result of reapl evaluation. ( MSG )."
  (let* ((json-object-type 'alist)
         (json-alist (json-read-from-string msg))
         (output (json-encode  (alist-get 'output json-alist)))
         (buffer (process-buffer reapl-mode_evaluation-proc)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n\n")
      (reapl-mode_insert-with-highlighting 'fennel-mode (alist-get 'expression json-alist "pouet"))
      (insert (propertize "\n\n=>\n\n" 'face 'font-lock-comment-face))
      (reapl-mode_insert-with-highlighting 'json-mode (reapl-mode_prettify-json-string output))
      (insert "\n")
      (let* ((start (point))
             (end (progn (insert (make-string (window-width (get-buffer-window buffer)) ?\s)) (point)))) ; insert a string of spaces
        (put-text-property start end 'face `(:underline ,(face-foreground 'font-lock-comment-face)))))
    (setq reapl-mode_evaluation-history
          (cons json-alist reapl-mode_evaluation-history))))

(defun reapl-mode_start-evaluation-proc ()
  "Start the completion process."
  (setq reapl-mode_evaluation-proc
        (make-network-process
         :name "reapl-evaluation-proc"
         :buffer "*reapl-evaluation*"
         :host 'local
         :service reapl-mode_evaluation-port
         :server t
         :family 'ipv4
         :type 'datagram
         :filter #'reapl-mode_on-received-evaluation
         :sentinel (lambda (_ _event)))))

;; completions
;; --------------------------------------------

(defvar reapl-mode_completions (list))
(defvar reapl-mode_completion-port 9996)
(defvar reapl-mode_completion-proc nil)

(defun reapl-mode_on-received-completions (_ msg)
  "Read the completions ( MSG ) sent by reapl server."
  (setq reapl-mode_completions
        (append (alist-get 'completions (json-read-from-string msg))
                nil))
  (make-local-variable 'company-backend)
  (setq company-backend #'reapl-mode_company-backend)
  (company-manual-begin))

(defun reapl-mode_start-completion-proc ()
  "Start the completion process."
  (setq reapl-mode_completion-proc
        (make-network-process
         :name "reapl-mode_completion-proc"
         :buffer "*reapl-completion*"
         :host 'local
         :service reapl-mode_completion-port
         :server t
         :family 'ipv4
         :type 'datagram
         :filter #'reapl-mode_on-received-completions
         :sentinel (lambda (_ _event)))))

(defun reapl-mode_company-backend (command &optional arg &rest ignored)
  "Reapl mode company backend. COMMAND &optional ARG &rest IGNORED."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'reapl-mode_company-backend))
    (prefix (and (eq major-mode 'reapl-mode) ;; Replace with required mode
                 (thing-at-point 'symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      reapl-mode_completions))))

(defun reapl-mode_company-hook ()
  "Company hook for reapl-mode."
  (company-mode 1)
  (add-to-list 'company-backends 'reapl-mode_company-backend))

(add-hook 'reapl-mode-hook 'reapl-mode_company-hook)

;; API
;; --------------------------------------------

(defun reapl-mode_repl-quit ()
  "Connect to the reaper server."
  (interactive)
  (when reapl-mode_reaper-connection
    (let ((buffer (process-buffer reapl-mode_reaper-connection)))
      (delete-process reapl-mode_reaper-connection)
      (kill-buffer buffer)))
  (when reapl-mode_evaluation-proc
    (let ((buffer (process-buffer reapl-mode_evaluation-proc)))
      (delete-process reapl-mode_evaluation-proc)
      (kill-buffer buffer)))
  (when reapl-mode_completion-proc
    (let ((buffer (process-buffer reapl-mode_completion-proc)))
      (delete-process reapl-mode_completion-proc)
      (kill-buffer buffer))))

(defun reapl-mode_connect ()
  "Connect to the reaper server."
  (interactive)
  (reapl-mode_repl-quit)
  (reapl-mode_connect-to-reaper)
  (reapl-mode_start-evaluation-proc)
  (reapl-mode_start-completion-proc)
  (display-buffer (get-buffer "*reapl-evaluation*")))

(defun reapl-mode_repl ()
  "Connect to the reaper server."
  (interactive)
  (when (not reapl-mode_reaper-connection)
    (reapl-mode_connect-to-reaper))
  (when (not reapl-mode_evaluation-proc)
    (reapl-mode_start-evaluation-proc))
  (when (not reapl-mode_completion-proc)
    (reapl-mode_start-completion-proc))
  (display-buffer (get-buffer "*reapl-evaluation*")))

(defun reapl-mode_request-evaluation (s)
  "Send the strng S to the remote reaper server."
  (process-send-string reapl-mode_reaper-connection (concat "{\"eval\": \"" s "\"}")))

(defun reapl-mode_request-completion (s)
  "Send the strng S to the remote reaper server."
  (process-send-string reapl-mode_reaper-connection (concat "{\"complete\": \"" s "\"}")))

(defun reapl-mode_thing-at-point ()
  "Return the thing at point. This can be either a word, symbol, or sexp, in that order of preference."
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

(provide 'reapl-mode)
;;; reapl-mode.el ends here
