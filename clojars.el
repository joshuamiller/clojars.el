(require 'request)
(require 'json)
(require 'ido)

(defun jar-name (result)
  (let ((group-name (cdr (assoc 'group_name result)))
        (jar-name (cdr (assoc 'jar_name result))))
    (if (string= group-name jar-name)
        jar-name
      (format "%s/%s" group-name jar-name))))

(defun format-dependency (result)
  (let ((version (cdr (assoc 'version result)))
        (name (jar-name result)))
    (format "[%s %S]" name version)))

(defun jar-result (result)
  (cons (format-dependency result) (jar-name result)))

(defun clojars-search (query)
  "Finds a Clojars dependency, and copies selected result to kill ring"
  (interactive "sSearch Clojars: ")
  (message "Loading...")
  (request
   "http://clojars.org/search"
   :params `(("q" . ,query) ("format" . "json"))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (let ((results (cdr (assoc 'results data))))
                 (kill-new (ido-completing-read "Results: " (mapcar 'jar-result results))))))))

(provide 'clojars-search)
