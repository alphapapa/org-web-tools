;;; org-web-tools-archive-search.el --- Search Web archives made with org-web-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;;; Requirements


;;;; Variables



(defgroup org-web-tools-archive-search nil
  "Options for org-web-tools-archive-search."
  :group 'org-web-tools-archive)

(defcustom org-web-tools-archive-directory
  (expand-file-name (concat org-directory "/" org-attach-directory))
  "Default directory to search."
  :type 'directory)

;;;; Commands


;;;; Functions

(cl-defun org-web-tools-archive-search--search (content-regexp
                                                &key filename-regexp (dir org-web-tools-archive-directory)
                                                (case-fold-search t))
  "Return list of Org IDs containing archived results for REGEXP in DIR.
Searches Zip and Tar archives in DIR."
  (let* ((extension-regexp (rx "." (or "zip" (seq "tar." (or "gz" "xz"))) eos))
         (regexp (if filename-regexp
                     (rx-to-string `(seq (regexp ,filename-regexp) (1+ anything) (regexp ,extension-regexp)))
                   extension-regexp))
         (files (directory-files-recursively dir regexp))
         (matching-files (--select (org-web-tools-archive-search--archive-match-p it content-regexp)
                                   files))
         (ids (--map (org-web-tools-archive-search--filename-to-id dir it)
                     matching-files)))
    (cl-loop for id in ids
             for marker = (org-id-find id 'marker)
             when marker
             collect (cons (org-with-point-at marker
                             (substring-no-properties (org-get-heading t t)))
                           id))))

(cl-defun org-web-tools-archive-search--archive-match-p (file regexp &key case-fold-search)
  "Return non-nil if FILE's contents match REGEXP."
  (let* ((extract-command (pcase file
                            ((rx ".zip" eos)
                             ;; Zip archive
                             (concat "unzip -c"))
                            ((rx ".tar.")
                             ;; Tar archive
                             (concat "tar --to-stdout -xf"))))
         (full-command (concat extract-command (shell-quote-argument file)
                               " | grep "
                               (when case-fold-search
                                 "-i ")
                               (shell-quote-argument regexp))))
    (with-temp-buffer
      (zerop (call-process-shell-command full-command nil (current-buffer))))))

(defun org-web-tools-archive-search--filename-to-id (dir filename)
  "Return Org ID for FILENAME in DIR."
  (let ((id-part (replace-regexp-in-string (rx-to-string `(seq bos ,dir)) "" filename)))
    (when (string-match (rx bos (optional "/")
                            (group (1+ (not (any "/"))))
                            "/"
                            (group (1+ (not (any "/")))))
                        id-part)
      (concat (match-string 1 id-part)
              (match-string 2 id-part)))))

;;;; Footer

(provide 'org-web-tools-archive-search)

;;; org-web-tools-archive-search.el ends here
