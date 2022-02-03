;;; sgd-lookup.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Yu Huo
;;
;; Author: Yu Huo <https://github.com/niwaka-ame>
;; Maintainer: Yu Huo <yhuo@tuta.io>
;; Created: February 01, 2022
;; Modified: February 01, 2022
;; Version: 0.0.2
;; Keywords: comm
;; Homepage: https://github.com/niwaka-ame/sgd-lookup.el
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  An Emacs API wrapper for Saccharomyces Genome Database (SGD).
;;
;;; Code:

(require 'json)

(defvar sgd-lookup-base-url "https://www.yeastgenome.org")

(defun sgd-lookup--get-and-parse-json (gene)
  "Get and parse json info from SGD for a specific GENE."
  (with-current-buffer (url-retrieve-synchronously
                        (concat
                         sgd-lookup-base-url
                         "/backend/locus/"
                         gene)
                        'silent
                        'inhibit-cookies
                        5)
    (json-read)))


(defun sgd-lookup--get-field (gene field)
  "Return the FIELD of info of a GENE in SGD."
  (let ((content-array (sgd-lookup--get-and-parse-json gene)))
    (cdr (seq-find
          #'(lambda (ele) (string= (car ele) field))
          content-array))))

(defun sgd-lookup--pop-buffer (string)
  "Show the SGD content as STRING in a pop-up buffer."
  (let ((sgd-buffer-name "*sgd-info*"))
    (get-buffer-create sgd-buffer-name)
    (with-current-buffer sgd-buffer-name
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert string)
      (org-mode))
    (display-buffer sgd-buffer-name
                    '(display-buffer-at-bottom . ((window-height . 0.2))))))

(defun sgd-lookup-description ()
  "Look up description of a gene on SGD in a posframe."
  (interactive)
  (let* ((gene (thing-at-point 'word))
         (name-desc (sgd-lookup--get-field gene "name_description"))
         (desc (sgd-lookup--get-field gene "description")))
    (sgd-lookup--pop-buffer (concat name-desc "\n" desc))))

(defun sgd-lookup-gene-homepage ()
  "Look up a gene on SGD via default browser."
  (interactive)
  (let* ((link (sgd-lookup--get-field (thing-at-point 'word) "link"))
         (url (concat sgd-lookup-base-url link)))
    (browse-url url)))

(defun sgd-lookup-gene-info ()
  "Look up gene info via internal and external links on SGD."
  (interactive)
  (let* ((urls-array (sgd-lookup--get-field (thing-at-point 'word) "urls"))
         (category-list (delete-dups (mapcar
                                      #'(lambda (ele)
                                          (cdr (assoc 'category ele)))
                                      urls-array)))
         (category-choice (completing-read "category: " category-list nil t))
         (filtered-urls-array (cl-remove-if-not
                               #'(lambda (ele)
                                   (string= category-choice
                                            (cdr (assoc 'category ele))))
                               urls-array))
         (display-name-list (mapcar
                             #'(lambda (ele)
                                 (cdr (assoc 'display_name ele)))
                             filtered-urls-array))
         (display-name-choice (completing-read "display name: " display-name-list nil t))
         (found-choice (cl-find
                        display-name-choice
                        filtered-urls-array
                        :key
                        #'(lambda (ele)
                            (cdr (assoc 'display_name ele)))
                        :test
                        #'string=))
         (link (cdr (assoc 'link found-choice))))
    (if (string-match-p "^http" link)
        (browse-url link)
      (browse-url (concat sgd-lookup-base-url link)))))

(provide 'sgd-lookup)
;;; sgd-lookup.el ends here
