;;; reapl-symex.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 pierre baille
;;
;; Author: pierre baille <pierrebaille@MBP2.local>
;; Maintainer: pierre baille <pierrebaille@MBP2.local>
;; Created: June 04, 2024
;; Modified: June 04, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/pierrebaille/reapl-symex
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'symex-interface)
(require 'reapl-mode)

(defun pb/reapl-no-op ()
  nil)

(symex-interface-add
 'reapl-mode
 (list :eval #'pb/reapl-send-thing-at-point
       :eval-definition (lambda () (symex-goto-lowest) (pb/reapl-send-thing-at-point))
       :eval-pretty #'pb/reapl-send-thing-at-point
       :eval-thunk #'pb/reapl-no-op
       :eval-print #'pb/reapl-no-op
       :describe-symbol #'pb/reapl-no-op
       :repl #'pb/reaper-repl
       :run #'pb/reapl-send-buffer))



(provide 'reapl-symex)
;;; reapl-symex.el ends here
