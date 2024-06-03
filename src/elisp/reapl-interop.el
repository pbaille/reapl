;;; reapl-interop.el -*- lexical-binding: t; -*-

(require 'bencode)
(require 'comint)

;;; Code:

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

(progn :reapl

       (defvar reapl-program "/Users/pierrebaille/Code/WIP/reapl/scripts/proxy-repl.py")

       (defun pb/reaper-repl ()
         (interactive)
         (with-current-buffer (or (get-buffer "*reapl*")
                                  (make-comint "reapl" reapl-program))
           (+popup/buffer)))

       (defun pb/reapl-send-string (s)
         (comint-send-string (get-buffer-process "*reapl*") (concat s "\C-d"))
         (with-current-buffer (get-buffer "*reapl*")
           (comint-send-eof)))

       (defun pb/reapl-send-thing-at-point ()
         (interactive)
         (pb/reapl-send-string (pb/thing-at-point)))

       (defun pb/reapl-send-buffer ()
         (interactive)
         (pb/reapl-send-string (buffer-substring-no-properties (point-min) (point-max)))))

(progn :reaper-mode

       (defvar reaper-mode-map (make-sparse-keymap))

       (define-derived-mode reaper-mode
         fennel-mode
         "reaper mode"
         :keymap reaper-mode-map)

       (map! :localleader
             (:map reaper-mode-map
                   "r" #'pb/reaper-repl
                   ))

       (require 'symex-interface)
       (defun pb/reapl-no-op ()
         nil)

       (symex-interface-add
        'reaper-mode
        (list :eval #'pb/reapl-send-thing-at-point
              :eval-definition (lambda () (symex-goto-lowest) (pb/reapl-send-thing-at-point))
              :eval-pretty #'pb/reapl-send-thing-at-point
              :eval-thunk #'pb/reapl-no-op
              :eval-print #'pb/reapl-no-op
              :describe-symbol #'pb/reapl-no-op
              :repl #'pb/reaper-repl
              :run #'pb/reapl-send-buffer)))

(provide 'reapl-interop)
;;; reapl-interop.el ends here
