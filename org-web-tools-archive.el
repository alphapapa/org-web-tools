;;; org-web-tools-archive.el --- Tools for archive.is  -*- lexical-binding: t -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-web-tools

;;; Commentary:

;; This file contains code for retrieving archived content from archive.is.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; TODO: Add new org link type "attachment:" that can link to entry attachments.

;;;; Requirements

(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)

(require 'org-attach)

(require 'dash)
(require 'esxml-query)
(require 'request)

;;;; Variables

(defvar org-web-tools-archive-hostname "archive.is"
  "archive.is seems to work.  Sometimes the server redirects to
archive.fo.  And archive.today is what it shows at the top of the
page.  So who knows.")

(defvar org-web-tools-archive-debug-level nil
  "See `request-log-level'.")

(defcustom org-web-tools-attach-url-archive-retry 15
  "Retry attaching archives that aren't yet available."
  :type '(choice (integer :tag "Retry asynchronously after N seconds")
                 (const :tag "Don't retry, just give an error" nil))
  :group 'org-web-tools)

(defcustom org-web-tools-attach-url-archive-async-attempts 2
  "Number of times to try to attach archives asynchronously."
  :type 'integer
  :group 'org-web-tools)

;;;; Commands

(declare-function archive-find-type "arc-mode")
(declare-function org-web-tools--read-url "org-web-tools")

;;;###autoload
(defun org-web-tools-attach-url-archive (url)
  "Download Zip archive of page at URL and attach with `org-attach'.
This downloads what archive.is returns as the latest archive of
the page.

Return value is undefined.  For calling from Lisp code, see
`org-web-tools-attach-url-archive--1'."
  (interactive (list (org-web-tools--read-url)))
  (if-let* ((size (org-web-tools-attach-url-archive--1 url)))
      (message "Attached %s archive of %s" size url)
    (pcase-exhaustive org-web-tools-attach-url-archive-retry
      ((pred integerp) (if (org-web-tools--attach-url-archive-async :url url
                                                                    :id (org-id-get nil 'create))
                           (message "Archive not yet available.  Retrying in %s seconds"
                                    org-web-tools-attach-url-archive-retry)
                         (error "Archive not available, and unable to retry (this shouldn't happen; please report this bug)")))
      ('nil (error "Archive not yet available.  Retry in a few seconds.")))))

;;;###autoload
(defun org-web-tools-view-archive ()
  "Open Zip file archive of web page.
Extracts to a temp directory and opens with
`browse-url-default-browser'.  Note: the extracted files are left
on-disk in the temp directory."
  (interactive)
  (unless (executable-find "unzip")
    (error "Can't find unzip command"))
  (let* ((attach-dir (org-attach-dir t))
	 (files (org-attach-file-list attach-dir))
	 (file (if (= (length files) 1)
		   (car files)
		 (completing-read "Open attachment: "
				  (mapcar #'list files) nil t)))
         (path (expand-file-name file attach-dir))
         (temp-dir (make-temp-file "org-web-tools-view-archive-" 'dir))
         (args (list path "-d" temp-dir)))
    (unless (= 0 (apply #'call-process (executable-find "unzip")
                        nil nil nil args))
      (error "Unzipping failed"))
    (browse-url-default-browser (concat "file://" temp-dir "/index.html"))
    (message "Files extracted to: %s" temp-dir)))

;;;; Functions

(defun org-web-tools-attach-url-archive--1 (url)
  "Return size in bytes if archive of URL is downloaded and attached at point successfully.
Returns nil if unsuccessful."
  ;; Rather than forcing `org-attach' to load when this package is loaded, we'll just load it here,
  ;; because `org-attach-attach' is not autoloaded.  Same for `arc-mode' and `archive-find-type'.
  (require 'org-attach)
  (require 'arc-mode)
  (when-let* ((archive-url (org-web-tools-archive--url-archive-url url))
              (temp-dir (make-temp-file "org-attach-download-link-" 'dir))
              (encoded-url (url-hexify-string url))
              (basename (concat encoded-url "--" (file-name-nondirectory (directory-file-name archive-url))))
              (local-path (expand-file-name basename temp-dir)))
    (unwind-protect
        (progn
          (url-copy-file archive-url local-path 'ok-if-exists 'keep-time)
          (pcase (ignore-errors
                   (with-temp-buffer
                     (insert-file-contents-literally local-path)
                     (archive-find-type)))
            ('zip (prog1
                      (file-size-human-readable (nth 7 (file-attributes local-path)))
                    (org-attach-attach local-path nil 'mv)))
            (_ nil)))
      (delete-directory temp-dir 'recursive))))

(cl-defun org-web-tools--attach-url-archive-async (&key url id (attempts 0))
  "Return a timer that adds an archive attachment of URL to entry with ID."
  (let ((fn (lambda ()
              (if-let* ((size (org-with-point-at (or (org-id-find id 'marker)
                                                     (error "Can't find entry %s to attach archive of %s at" id url))
                                (org-web-tools-attach-url-archive--1 url))))
                  (message "Attached %s archive of %s" size url)
                (if (>= (cl-incf attempts) org-web-tools-attach-url-archive-async-attempts)
                    (if-let* ((marker (org-id-find id 'marker)))
                        (progn
                          (pop-to-buffer (marker-buffer marker))
                          (goto-char marker)
                          (error "Failed to attach archive of %s asynchronously.  Try again manually" url))
                      (error "Failed to attach archive of %s asynchronously, and couldn't find entry %s" url id)))
                (org-web-tools--attach-url-archive-async :url url :id id :attempts attempts)
                (message "Archive of %s not yet available.  Retrying in %s seconds (%s attempts)"
                         url org-web-tools-attach-url-archive-retry attempts)))))
    (run-at-time org-web-tools-attach-url-archive-retry nil fn)))

(defun org-web-tools-archive--url-archive-url (url)
  "Return URL to Zip archive of URL."
  (when-let* ((id (org-web-tools-archive--url-id url)))
    (concat "http://" org-web-tools-archive-hostname "/download/" id ".zip")))

(defun org-web-tools-archive--url-id (url)
  "Return ID of most recent archive of URL."
  (let* ((submitid (org-web-tools-archive--submitid))
         (submit-url (concat "https://" org-web-tools-archive-hostname "/submit/"))
         (data (list (cons "anyway" 1)
                     (cons "submitid" submitid)
                     (cons "url" url)))
         (response (org-web-tools-archive--request submit-url
                     :type "POST"
                     :data data
                     :timeout 10
                     :sync t))
         (refresh (request-response-header response "Refresh")))
    (when (string-match (rx "url=http" (optional "s") "://"
                            (1+ (not (any "/"))) "/"   ; hostname
                            (group (1+ anything)))     ; ID
                        refresh)
      (match-string 1 refresh))))

(defun org-web-tools-archive--submitid ()
  "Return new submitid string.
Raises an error if unable to get it."
  (let* ((url (concat "https://" org-web-tools-archive-hostname "/"))
         (parser (lambda ()
                   (-let* ((tree (libxml-parse-html-region (point) (point-max)))
                           ((_element . (attrs)) (esxml-query "input[name=submitid]" tree)))
                     (alist-get 'value attrs))))
         (response (org-web-tools-archive--request url
                     :sync t
                     :parser parser
                     :success (cl-function
                               (lambda (&key data &allow-other-keys)
                                 data)))))
    (or (request-response-data response)
        (error "Unable to get submitid"))))

(defun org-web-tools-archive--request (&rest args)
  "Wrapper for `request'."
  (declare (indent defun))
  ;; When using the curl backend with "POST", `request' always returns before
  ;; the request actually completes.  So we use the `url-retrieve' backend,
  ;; which seems to work correctly.
  (let ((request-log-level org-web-tools-archive-debug-level)
        (request-backend 'url-retrieve))
    (apply #'request args)))

;;;; Footer

(provide 'org-web-tools-archive)

;;; org-web-tools-archive.el ends here

;; Local Variables:
;; fill-column: 80
;; End:
