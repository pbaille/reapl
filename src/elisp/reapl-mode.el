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
;; Package-Requires: ((emacs "26.1") (fennel-mode "0.9.1") (bencode "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Talking to the Reaper DAW using Fennel.
;;
;;; Code:

(require 'bencode)
(require 'comint)
(require 'fennel-mode)

;;; Code:

(defun reapl-mode_thing-at-point ()
  "Return the thing at point. This can be either a word, symbol, or sexp, in that order of preference."
  (or (thing-at-point 'word t)
      (thing-at-point 'symbol t)
      (thing-at-point 'sexp t)))

;;; socket

(defun reapl-mode_mk-udp-socket (ip port)
  "Create a UDP network process with given IP and PORT."
  (make-network-process :name "udp-socket" :type 'datagram :family 'ipv4 :host ip :service port))

(defun reapl-mode_udp-socket-send-str (socket str)
  "Send STR as a UTF-8 string to the given SOCKET."
  (process-send-string socket (encode-coding-string str 'utf-8)))

(defun reapl-mode_udp-send-str (ip port str)
  "Create a UDP socket at given IP and PORT, then send STR and close socket."
  (let ((sk (reapl-mode_mk-udp-socket ip port)))
    (reapl-mode_udp-socket-send-str sk str)
    (delete-process sk)))

;;; socket-repl

(defvar reapl-mode_reaper-socket-repl-host "127.0.0.1")
(defvar reapl-mode_reaper-socket-repl-port 9999)

(defun reapl-mode_send-to-reaper-socket-repl (s)
  "Send string S to the UDP socket at defined host and port variables."
  (reapl-mode_udp-send-str reapl-mode_reaper-socket-repl-host
                           reapl-mode_reaper-socket-repl-port
                           s))


(defun reapl-mode_send-thing-at-point-to-reaper-socket-repl ()
  "Send the thing at point to the Reaper socket REPL in encoded form."
  (interactive)
  (let ((s (reapl-mode_thing-at-point)))
    (reapl-mode_send-to-reaper-socket-repl
     (bencode-encode `(:code ,s)))))

;;; reapl

(defvar reapl-program)

(defun reapl-mode_repl ()
  "Launch or bring up to front the Reapl REPL process in a comint buffer."
  (interactive)
  (if (not reapl-program)
      (error "Please set reapl-program variable in order to be able to launch the repl"))
  (with-current-buffer (or (get-buffer "*reapl*")
                           (make-comint "reapl" reapl-program))
    (display-buffer (current-buffer))))

(defun reapl-mode_repl-quit ()
  "Quit the repl."
  (interactive)
  (let ((b (get-buffer "*reapl*")))
    (when b
      (kill-process (get-buffer-process b))
      (kill-buffer b))))

(defun reapl-mode_restart-repl ()
  "Restart the repl."
  (interactive)
  (reapl-mode_repl-quit)
  (reapl-mode_repl))

(defun reapl-mode_send-string (s)
  "Send string S to the Reapl REPL."
  (comint-send-string (get-buffer-process "*reapl*") (concat "{\"eval\": \"" s "\"}\C-d"))
  (with-current-buffer (get-buffer "*reapl*")
    (comint-send-eof)))

(defun reapl-mode_get-completions (s)
  "Ask some completions for S to the Reapl REPL."
  (comint-send-string (get-buffer-process "*reapl*") (concat "{\"complete\": \"" s "\"}\C-d"))
  (with-current-buffer (get-buffer "*reapl*")
    (comint-send-eof)))

(defun reapl-mode_send-thing-at-point ()
  "Send the thing at point to the Reapl REPL."
  (interactive)
  (reapl-mode_send-string (reapl-mode_thing-at-point)))

(defun reapl-mode_complete-symbol-at-point ()
  "Ask for completions for the symbol at point to the reaper REPL."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (when sym (reapl-mode_get-completions sym))))

(defun reapl-mode_send-buffer ()
  "Send the entire buffer content to the Reapl REPL."
  (interactive)
  (reapl-mode_send-string (buffer-substring-no-properties (point-min) (point-max))))

(defvar reapl-mode_last-comint-output)

(defun reapl-mode_comint-output-filter (x)
  "A =comint-output-filter-function= that records the last *reapl* output X."
  (when (string-equal "*reapl*" (buffer-name (current-buffer)))
    (print "1")
    (let ((s (substring-no-properties x)))
      (when (string-prefix-p "{" s)
        (print 2)
        (setq reapl-mode_last-comint-output (json-read-from-string s))))))

(setq comint-output-filter-functions
      (cons #'reapl-mode_comint-output-filter comint-output-filter-functions))

;(setq comint-output-filter-functions nil)

;;; reaper-mode

(defvar reapl-mode-map (make-sparse-keymap))

(define-derived-mode reapl-mode
  fennel-mode
  "reapl mode"
  :keymap reapl-mode-map)

(require 'company)
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
      (append (alist-get 'completions reapl-mode_last-comint-output)
              nil)))))

(defun reapl-mode_company-hook ()
  "Company hook for reapl-mode."
  (print "run reapl hook")
  (company-mode 1)
  (add-to-list 'company-backends 'reapl-mode_company-backend))

(add-hook 'reapl-mode-hook 'reapl-mode_company-hook)

(provide 'reapl-mode)
;;; reapl-mode.el ends here
