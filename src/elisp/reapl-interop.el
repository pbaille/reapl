;;; reapl-interop.el -*- lexical-binding: t; -*-

(require 'bencode)

(defun pb/thing-at-point ()
  "Return the thing at point, regardless of its type."
  (or (thing-at-point 'word t)
      (thing-at-point 'symbol t)
      (thing-at-point 'sexp t)))

(progn :socket

       (defun pb/mk-udp-socket (ip port)
         (make-network-process :name "udp-socket" :type 'datagram :family 'ipv4 :host ip :service port))

       (defun pb/udp-socket-send-str (socket str)
         (process-send-string socket (encode-coding-string str 'utf-8)))

       (defun pb/udp-send-str (ip port str)
         (let ((sk (pb/mk-udp-socket ip port)))
           (pb/udp-socket-send-str sk str)
           (delete-process sk)))

       '(progn "try"
               (pb/udp-send-str "127.0.0.1" 9999 (bencode-encode '(:code "(+ 1 2)")))))

(progn :socket-repl

        (setq pb/reaper-socket-repl-host "127.0.0.1")
        (setq pb/reaper-socket-repl-port 9999)

        (defun pb/send-to-reaper-socket-repl (s)
          (pb/udp-send-str pb/reaper-socket-repl-host
                           pb/reaper-socket-repl-port
                           s))

        (defun pb/send-fnl-s-expression-to-reaper-socket-repl ()
          (interactive)
          (let ((s (pb/thing-at-point)))
            (pb/send-to-reaper-socket-repl
             (bencode-encode `(:code ,s)))))

        '(progn :try
                ((fn [a b] (+ a b)) 3 4)))

(provide 'reapl-interop)