;;; reapl-bindings.el --- Doom bindings for reapl-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 pierre baille
;;
;; Author: pbaille
;; Maintainer: pbaille
;; Created: June 04, 2024
;; Modified: June 04, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/pierrebaille/reapl-bindings
;; Package-Requires: ((emacs "24.3") (reapl-mode "0.0.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Doom bindings for reapl mode
;;
;;; Code:

(require 'reapl-mode)

(map! :localleader
      (:map reapl-mode-map
            "'" #'reapl-mode_connect
            "r" #'reapl-mode_repl
            "q" #'reapl-mode_repl-quit
            "e b" #'reapl-mode_send-buffer
            "c" #'reapl-mode_complete-symbol-at-point))

(map! (:map reapl-mode-map
       :i "TAB" #'reapl-mode_complete-symbol-at-point)
      (:map company-active-map
            "C-h" #'reapl-mode_request-doc-for-completion-candidate))

(provide 'reapl-bindings)
;;; reapl-bindings.el ends here
