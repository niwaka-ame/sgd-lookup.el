;;; sgd-lookup.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Yu Huo
;;
;; Author: Yu Huo <https://github.com/niwaka-ame>
;; Maintainer: Yu Huo <yhuo@tuta.io>
;; Created: February 01, 2022
;; Modified: February 20, 2022
;; Version: 0.1.4
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
(require 'cl-lib)

(defconst sgd-lookup-base-url "https://www.yeastgenome.org")
(defconst sgd-lookup-action-alist
  '(("description" . sgd-lookup-description)
    ("name description" . sgd-lookup-name-description)
    ("summary paragraph" . sgd-lookup-paragraph)
    ("phenotype" . sgd-lookup-phenotype)
    ("visit in browser" . sgd-lookup-gene-homepage)
    ("more URLs" . sgd-lookup-gene-info)))

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
    (condition-case nil
        (json-read)
      (error (error "Failed to fetch data. Please try again. Possible cause: connection overtime or unknown gene name!")))))


(defun sgd-lookup--get-field (gene field)
  "Return the FIELD of info of a GENE in SGD."
  (let* ((content-array (sgd-lookup--get-and-parse-json gene))
         (field-content (cdr (seq-find
                              #'(lambda (ele) (string= (car ele) field))
                              content-array))))
    (or field-content (error "Connection is successful, but no relevant information is found on SGD!"))))

(defun sgd-lookup--pop-window (string &optional height)
  "Show the SGD content as STRING in a pop-up window with HEIGHT."
  (let ((sgd-buffer-name "*sgd-info*")
        (height (or height 0.2)))
    (get-buffer-create sgd-buffer-name)
    (with-current-buffer sgd-buffer-name
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert string)
      (org-mode))
    (display-buffer sgd-buffer-name
                    `(display-buffer-at-bottom . ((window-height . ,height))))))

(defun sgd-lookup-description (gene)
  "Look up description of a GENE on SGD in a pop-up window at the bottom of current frame."
  (let ((desc (sgd-lookup--get-field gene "description")))
    (sgd-lookup--pop-window (concat gene "\n" desc))))

(defun sgd-lookup-name-description (gene)
  "Look up name description of a GENE on SGD in a pop-up window."
  (let ((name-desc (sgd-lookup--get-field gene "name_description")))
    (sgd-lookup--pop-window (concat gene "\n" name-desc))))

(defun sgd-lookup-phenotype (gene)
  "Look up phenotype of a GENE on SGD in a pop-up window at the bottom of current frame."
  (let* ((phenotype (sgd-lookup--get-field gene "phenotype_overview"))
         (paragraph (cdr (assoc "paragraph" phenotype 'string=))))
    (if paragraph
        (sgd-lookup--pop-window (concat gene "\n" paragraph))
      (error "Connection is successful, but no relevant information is found on SGD!"))))

(defun sgd-lookup-paragraph (gene)
  "Look up paragraph of a GENE on SGD in a pop-up window at the bottom of current frame."
  (let* ((paragraph (cdr (assoc 'text (sgd-lookup--get-field gene "paragraph"))))
         (string (with-temp-buffer
                   (insert paragraph)
                   (goto-char (point-min))
                   ; remove the annoying HTML fragments
                   (while (re-search-forward "<.*?>" nil t)
                     (replace-match "" 'fixedcase nil))
                   (buffer-string))))
    (sgd-lookup--pop-window (concat gene "\n" string) 0.3)))

(defun sgd-lookup-gene-homepage (gene)
  "Look up a GENE on SGD via default browser."
  (let* ((link (sgd-lookup--get-field gene "link"))
         (url (concat sgd-lookup-base-url link)))
    (browse-url url)))

(defun sgd-lookup-gene-info (gene)
  "Look up GENE info via internal and external links on SGD."
  (let* ((urls-array (sgd-lookup--get-field gene "urls"))
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

(defun sgd-lookup--action-menu (gene)
  "Prompt for function to use to look up GENE info."
  (let* ((func-list (mapcar #'car sgd-lookup-action-alist))
         (choice (completing-read "Action: " func-list nil t)))
    (funcall (cdr (assoc choice sgd-lookup-action-alist)) gene)))

(defun sgd-lookup (gene)
  "Prompt for a gene name and look up info on SGD."
  (interactive "sInput gene name: ")
  (sgd-lookup--action-menu gene))

(defun sgd-lookup-at-point ()
  "Detect gene name at point and look up info on SGD."
  (interactive)
  (let ((gene (thing-at-point 'word)))
    (sgd-lookup--action-menu gene)))

(provide 'sgd-lookup)
;;; sgd-lookup.el ends here
