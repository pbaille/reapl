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
  "Return the thing at point, regardless of its type."
  (or (thing-at-point 'word t)
      (thing-at-point 'symbol t)
      (thing-at-point 'sexp t)))

(progn :socket

       (defun reapl-mode_mk-udp-socket (ip port)
         (make-network-process :name "udp-socket" :type 'datagram :family 'ipv4 :host ip :service port))

       (defun reapl-mode_udp-socket-send-str (socket str)
         (process-send-string socket (encode-coding-string str 'utf-8)))

       (defun reapl-mode_udp-send-str (ip port str)
         (let ((sk (reapl-mode_mk-udp-socket ip port)))
           (reapl-mode_udp-socket-send-str sk str)
           (delete-process sk)))

       '(progn "try"
               (reapl-mode_udp-send-str "127.0.0.1" 9999 (bencode-encode '(:code "(+ 1 2)")))))

(progn :socket-repl

        (defvar reapl-mode_reaper-socket-repl-host "127.0.0.1")
        (defvar reapl-mode_reaper-socket-repl-port 9999)

        (defun reapl-mode_send-to-reaper-socket-repl (s)
          (reapl-mode_udp-send-str reapl-mode_reaper-socket-repl-host
                                   reapl-mode_reaper-socket-repl-port
                                   s))


        (defun reapl-mode_send-thing-at-point-to-reaper-socket-repl ()
          (interactive)
          (let ((s (reapl-mode_thing-at-point)))
            (reapl-mode_send-to-reaper-socket-repl
             (bencode-encode `(:code ,s)))))

        '(progn :try
                ((fn [a b] (+ a b)) 3 4)))

(progn :reapl

       (defvar reapl-program (expand-file-name "../../scripts/reapl.py" default-directory))

       (defun reapl-mode_repl ()
         (interactive)
         (with-current-buffer (or (get-buffer "*reapl*")
                                  (make-comint "reapl" reapl-program))
           (display-buffer (current-buffer))))

       (defun reapl-mode_send-string (s)
         (comint-send-string (get-buffer-process "*reapl*") (concat s "\C-d"))
         (with-current-buffer (get-buffer "*reapl*")
           (comint-send-eof)))

       (defun reapl-mode_send-thing-at-point ()
         (interactive)
         (reapl-mode_send-string (reapl-mode_thing-at-point)))

       (defun reapl-mode_send-buffer ()
         (interactive)
         (reapl-mode_send-string (buffer-substring-no-properties (point-min) (point-max)))))

(progn :reaper-mode

       (defvar reapl-mode-map (make-sparse-keymap))

       (define-derived-mode reapl-mode
         fennel-mode
         "reaper mode"
         :keymap reapl-mode-map))

(provide 'reapl-mode)
;;; reapl-mode.el ends here
