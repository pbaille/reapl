;;; reapl-hydra.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'hydrata)
(require 'reapl-mode)

(defun reapl-hydra_init ()
  "Init the reaper hydra."
  (reapl-mode_send-message
   :eval "(actions.get-binding-tree)"
   (lambda (res)
     (let* ((data (plist-get res :data))
            (tree (plist-get data :value))
            (children (pb-walk_pre tree
                                   (lambda (x)
                                     (if-let ((fn (plist-get x :fn)))
                                         (plist-put x :fn `(lambda ()
                                                             (interactive)
                                                             (reapl-mode_send-action ,fn)))
                                       x)))))
       (eval (hydrata_compile
              :reaper
              (list :options (list :foreign-keys 'warn)
                    :doc "reaper root"
                    :children children)))))))

(provide 'reapl-hydra)
;;; reapl-hydra.el ends here
