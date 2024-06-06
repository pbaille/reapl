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
(require 'symex-traversals)
(require 'reapl-mode)

(defun reapl-symex_no-op ()
  "Do nothing."
  nil)

(symex-interface-extend
 (list 'reapl-mode)
 (list :eval #'reapl-mode_send-thing-at-point
       :eval-definition (lambda () (symex-goto-lowest) (pb/reapl-send-thing-at-point))
       :eval-pretty #'reapl-mode_send-thing-at-point
       :eval-thunk #'reapl-symex_no-op
       :eval-print #'reapl-symex_no-op
       :describe-symbol #'reapl-symex_no-op
       :repl #'reapl-mode_repl
       :run #'reapl-mode_send-buffer))

(provide 'reapl-symex)
;;; reapl-symex.el ends here
