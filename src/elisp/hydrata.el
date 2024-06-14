;;; hydrata.el --- Specifies hydras using data instead of macros -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 pierre baille
;;
;; Author: pierre baille <pierrebaille@MBP2.local>
;; Maintainer: pierre baille <pierrebaille@MBP2.local>
;; Created: June 14, 2024
;; Modified: June 14, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/pierrebaille/hydrata
;; Package-Requires: ((emacs "25.1") (hydra "0.15.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;  Specifies hydras using data instead of macros
;;
;;; Code:

(require 'hydra)

(defun hydrata_leaf? (spec)
  "Test if SPEC is a leaf"
  (not (alist-get 'children spec)))

(defun hydrata_position->sym (at)
  "Turn the path AT into a symbol."
  (intern (mapconcat #'identity (reverse (mapcar #'symbol-name at)) "/")))

(defun hydrata_position->body-sym (at)
  "Turn the path AT into its corresponding hydra body symbol."
  (intern (concat (symbol-name (hydrata_position->sym at)) "/body")))

(defun hydrata_plist-merge (&rest plists)
  "Merge property lists PLISTS together.
If same key is in multiple plists, the value in the last plist is used."
  (let ((result '()))
    (dolist (plist plists result)
      (while plist
        (setq result (plist-put result (car plist) (cadr plist)))
        (setq plist (cddr plist))))))

(defun hydrata_unnest (parent spec)
  "Preformat SPEC with the help of its PARENT spec."
  (let* ((name (car spec))
         (opts (cdr spec))
         (children (alist-get 'children opts))
         (at (cons name (alist-get 'at parent)))
         (options (hydrata_plist-merge (alist-get 'options parent (list)) (alist-get 'options opts (list)))))
    (if children
        (let ((self `((at . ,at)
                      (options . ,options)
                      (doc . ,(alist-get 'doc opts ""))
                      (children . ,(mapcar (lambda (c)
                                             (let* ((name (car c))
                                                    (spec (cdr c))
                                                    (leaf? (hydrata_leaf? spec))
                                                    (options (if leaf?
                                                                 (alist-get 'options c (list))
                                                               (hydrata_plist-merge (alist-get 'options c (list))
                                                                                    (list :exit t :column "Nav")))))
                                               `(,name . ((key . ,(alist-get 'key spec))
                                                          (options . ,options)
                                                          (fn . (function ,(if leaf?
                                                                               (alist-get 'fn spec)
                                                                             (hydrata_position->body-sym (cons name at)))))))))
                                           children)))))
          (cons self (seq-mapcat (lambda (c) (hydrata_unnest self c)) children))))))

(defun hydrata_compile1 (spec)
  "Compile one hydrata SPEC."
  (let ((children (alist-get 'children spec))
        (at (alist-get 'at spec)))
    `(defhydra ,(hydrata_position->sym at)
       ,(alist-get 'options spec)
       ,(alist-get 'doc spec)
       ("<escape>" ,(if (cdr at)
                        (function ,(hydrata_position->body-sym (cdr at)))
                      nil)
        ,(concat "back to " (if (cdr at)
                                (symbol-name (cadr at))
                              "emacs"))
        :exit t :column "Nav")
       ("q" nil "quit" :exit t :column "Nav")
       ,@(mapcar (lambda (c)
                   (list* (alist-get 'key c)
                          (alist-get 'fn c)
                          (symbol-name (car c))
                          (hydrata_plist-merge (list :column "Act") (alist-get 'options c (list)))))
                 children))))

(defun hydrata_compile (spec)
  "Compile SPEC into hydra forms"
  (cons 'progn
        (mapcar #'hydrata_compile1 (hydrata_unnest '((at . ())) spec))))

'(:comment
  (defvar hydrata_sample
    '(root . ((options . (:foreign-keys warn))
              (doc . "root:\n\n")
              (children . ((child1 . ((key . "a") (fn . some-fn)))
                           (child2 . ((key . "b") (fn . some-other-fn)))
                           (child3 . ((key . "c")
                                      (children . ((subchild1 . ((key . "a") (fn . some-sub-fn)))
                                                   (subchild2 . ((key . "b") (fn . some-other-sub-fn))))))))))))
  (pp (hydrata_unnest () hydrata_sample))
  (eval (hydrata_compile hydrata_sample)))

(provide 'hydrata)
;;; hydrata.el ends here
