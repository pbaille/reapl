;;; hydrata.el --- utils -*- lexical-binding: t; -*-

;; Author: Pierre Baille
;; URL: https://github.com/pbaille
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Utils.

;;; Code:

(require 'hydra)

(defun hydrata_leaf? (spec)
  "Test if SPEC is a leaf."
  (not (plist-get spec :children)))

(defun keyword-name (keyword)
  "Convert KEYWORD to a string without the leading colon."
  (substring (symbol-name keyword) 1))

(defun hydrata_position->sym (at)
  "Turn the path AT into a symbol."
  (intern (mapconcat #'identity (reverse (mapcar #'keyword-name at)) "/")))

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

(defun km_map-values (f pl)
  "Apply F to each value in plist PL."
  (cl-loop for (k v) on pl by #'cddr
           collect k
           and collect (funcall f k v)))

(defun km_to-alist (plist)
  "Convert PLIST to alist."
  (let (alist)
    (while plist
      (setq alist (cons (cons (car plist) (cadr plist)) alist))
      (setq plist (cddr plist)))
    (reverse alist)))

(defun hydrata_unnest (parent spec)
  "Preformat SPEC with the help of its PARENT spec."
  (let* ((name (car spec))
         (opts (cdr spec))
         (children (plist-get opts :children))
         (at (cons name (plist-get parent :at)))
         (options (hydrata_plist-merge (or (plist-get parent :options) (list))
                                       (or (plist-get opts :options) (list)))))
    (if children
        (let ((self `(:at ,at
                      :options ,options
                      :doc ,(or (plist-get :doc opts)
                                "")
                      :children ,(km_map-values (lambda (name spec)
                                                  (let* ((leaf? (hydrata_leaf? spec))
                                                         (options (if leaf?
                                                                      (or (plist-get spec :options)
                                                                          (list))
                                                                    (hydrata_plist-merge (or (plist-get spec :options)
                                                                                             (list))
                                                                                         (list :exit t :column "Nav")))))
                                                    `(:key ,(plist-get spec :key)
                                                      :options ,options
                                                      :fn ,(if leaf?
                                                               (plist-get spec :fn)
                                                             (list 'function (hydrata_position->body-sym (cons name at)))))))
                                                children))))
          (cons self (seq-mapcat (lambda (c) (hydrata_unnest self c))
                                 (km_to-alist children)))))))

(defun hydrata_compile1 (spec)
  "Compile one hydrata SPEC."
  (let ((children (plist-get spec :children))
        (at (plist-get spec :at)))
    `(defhydra ,(hydrata_position->sym at)
       ,(plist-get spec :options)
       ,(plist-get spec :doc)
       ("<escape>" ,(if (cdr at)
                        `(function ,(hydrata_position->body-sym (cdr at)))
                      nil)
        ,(concat "back to " (if (cdr at)
                                (symbol-name (cadr at))
                              "emacs"))
        :exit t :column "Nav")
       ("q" nil "quit" :exit t :column "Nav")
       ,@(mapcar (lambda (c)
                   (let ((name (car c))
                         (spec (cdr c)))
                     (list* (plist-get spec :key)
                            (plist-get spec :fn)
                            (keyword-name name)
                            (hydrata_plist-merge (list :column "Act") (or (plist-get spec :options)
                                                                          (list))))))
                 (km_to-alist children)))))

(defun hydrata_compile (name spec)
  "Compile SPEC into hydra forms with NAME prefix."
  (cons 'progn
        (mapcar #'hydrata_compile1 (hydrata_unnest '(:at ()) (cons name spec)))))

'(:comment
  (defvar hydrata_sample
    '(:options (:foreign-keys warn)
      :doc "root"
      :children (:child1 (:key "a" :fn some-fn)
                 :child2 (:key "b" :fn some-other-fn)
                 :child3 (:key "c"
                          :children (:subchild1 (:key "a" :fn some-sub-fn)
                                     :subchild2 (:key "b" :fn some-other-sub-fn))))))
  (hydrata_unnest () (cons :root hydrata_sample))
  (eval (hydrata_compile :root hydrata_sample)))

(provide 'hydrata)
;;; hydrata.el ends here
