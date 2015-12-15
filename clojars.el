;;; clojars.el --- clojars.org search interface

;; Filename: clojars.el
;; Description: clojars.org search interface
;; Author: Joshua Miller <josh@joshmiller.io>
;; License: GPLv3
;; Created: 2014-12-15 17:21:05
;; Version: 1.0.1
;; Package-Version: 20151215.101
;; URL: https://github.com/joshuamiller/clojars.el
;; Keywords: docs, help, tools
;; Package-Requires: ((request "0.1.0") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; M-x clojars - to search clojars.org for a Clojure library

;;; Code:

(require 'request)
(require 'json)
(require 'cl-lib)

(defconst clojars-search-endpoint "https://clojars.org/search"
  "Clojars search endpoint")

(defun clojars-jar-name (result)
  (let ((group-name (cdr (assoc 'group_name result)))
        (jar-name (cdr (assoc 'jar_name result))))
    (if (string= group-name jar-name)
        jar-name
      (format "%s/%s" group-name jar-name))))

(defun clojars-format-dependency (result)
  (let ((version (cdr (assoc 'version result)))
        (name (clojars-jar-name result)))
    (format "[%s %S]" name version)))

(defun clojars-jar-result (result)
  (cons (clojars-format-dependency result) (clojars-jar-name result)))

;;;###autoload
(defun clojars (query)
  "Finds a Clojure library from clojars.org, and copies selected
   result to kill ring"
  (interactive "sSearch Clojars: ")
  (message "Loading...")
  (request
   clojars-search-endpoint
   :params `(("q" . ,query) ("format" . "json"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (let ((results (cdr (assoc 'results data))))
                 (kill-new (completing-read "Results: " (mapcar 'clojars-jar-result results))))))))

(provide 'clojars)

;;; clojars.el ends here
