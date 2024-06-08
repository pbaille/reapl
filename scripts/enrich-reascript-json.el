

(require 'json)

(defun add-form-key (data)
  "Add form key to each object in the JSON array DATA."
  (mapcar (lambda (obj)
            (let* ((namespace (alist-get 'namespace obj))
                   (name (alist-get 'name obj))
                   (params (alist-get 'params obj))
                   (returns (alist-get 'returns obj))
                   (param-names (mapcar (lambda (param)
                                          (concat (if (alist-get 'optional param) "?" "")
                                                  (alist-get 'name param)))
                                        params))
                   (return-type (mapcar (lambda (ret) (alist-get 'type ret)) returns))
                   (form-string (format "(%s.%s %s) :: %s"
                                        namespace
                                        name
                                        (mapconcat 'identity param-names " ")
                                        return-type))
                   (param-types-string (apply 'concat (mapcar (lambda (param)
                                                                (concat (alist-get 'name param)
                                                                        " :: "
                                                                        (alist-get 'type param)
                                                                        (if (alist-get 'optional param) " (optional)" "")
                                                                        "\n"))
                                                              params))))
              (append (list (cons 'form form-string)
                            (cons 'doc (concat form-string "\n" param-types-string (alist-get 'description obj))))
                      obj)))
          data))

(defun read-json-file (file)
  "Read JSON data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (json-read)))

(defun write-json-file (file data)
  "Write DATA as JSON to FILE."
  (with-temp-buffer
    (insert (json-encode data))
    (json-pretty-print-buffer)
    (write-file file)))

(let ((input-file "~/Code/WIP/reapl/reascript.json")
      (output-file "~/Code/WIP/reapl/reascript-with-form.json"))
  (let ((data (read-json-file input-file)))
    (let ((modified-data (add-form-key data)))
      (write-json-file output-file modified-data))))

(message "The form keys have been added.")
