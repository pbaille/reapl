;;; pb-udp.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'json)

(defvar pb-udp_default-state (list :id nil :data ""))

(defun pb-udp_mk-proc-filter (handler)
  "Build a process filter function given an HANDLER plist."
  (let ((state pb-udp_default-state))
    (lambda (_ string)
      ;(print "ping")
      (let ((json-object-type 'plist)
            (json-array-type 'list))
        (let* ((json-message (json-read-from-string string))
               (id (plist-get json-message :id))
               (op (plist-get json-message :op))
               (data (plist-get json-message :data)))
          '(print "-ping:\n")
          '(print (list :id id :op op :data data))
          (when id
            (unless (equal (plist-get state :id) id)
              (setq state pb-udp_default-state)
              '(print "--- reset 1"))
            (cond ((and data (not op))
                   '(print "--- concat")
                   (setq state
                         (list :id id
                               :data (concat (plist-get state :data) data))))
                  (op
                   (progn '(when (equal (plist-get state :id) id)
                             (print "will dump --------------------------------------")
                             (with-temp-file "/Users/pierrebaille/Desktop/last-chunked-res.json"
                               (insert (plist-get state :data))))
                          (let* ((msg (if (equal (plist-get state :id) id)
                                          (let ((encoded (json-read-from-string
                                                          (plist-get state :data))))
                                            (print `((chunk-data-str ,(plist-get state :data))
                                                     (decoded ,(json-read-from-string (plist-get state :data)))
                                                     ))
                                            '(print `(len ,(length (plist-put json-message :data
                                                               (json-read-from-string
                                                                (plist-get state :data))))))
                                            (plist-put json-message :data encoded))
                                        json-message))
                                 (ret (funcall handler msg)))
                            (setq state pb-udp_default-state)
                            '(print "--- reset 2")
                            ret)))
                  (t
                   (setq state pb-udp_default-state)
                   (print "SHOULD NOT BE THERE")
                   (error "Bad format msg")))))))))

(defun pb-udp_start-listening (host port handler)
  "Start to listen HOST PORT, handling incoming messages with HANDLER."
  (let ((proc (make-network-process
               :name "my-udp-proc"
               :buffer "*my-udp-proc*"
               :host host
               :service port
               :server t
               :family 'ipv4
               :type 'datagram)))
    (set-process-filter proc (pb-udp_mk-proc-filter handler))
    proc))

'(:tries

  (delete-process pb-udp_receive-proc)

  (defvar pb-udp_receive-proc
    (pb-udp_start-listening 'local
                            "8088"
                            (lambda (opts)
                              (let ((op (plist-get opts :op))
                                    (data (plist-get opts :data)))
                                (cond ((equal op "print") (print data))
                                      (t (error "Unknown op")))))))

  (delete-process pb-udp_send-proc)

  (defvar pb-udp_send-proc
    (make-network-process
     :name "*pb-udp_out*"
     :host 'local
     :service "8088"
     :type 'datagram
     :family 'ipv4))

  (process-send-string pb-udp_send-proc
                       (json-encode-plist '(:id 1 :op :print :data "yo")))
  (process-send-string pb-udp_send-proc
                       (json-encode-plist '(:id 2 :data "yo")))
  (process-send-string pb-udp_send-proc
                       (json-encode-plist '(:id 2 :op :print))))

(provide 'pb-udp)
;;; pb-udp.el ends here.
