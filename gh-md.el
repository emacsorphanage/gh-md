;;; gh-md.el --- Render markdown using the github api  -*- lexical-binding: t; -*-

;; Copyright © 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/gh-md.el
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24") (pkg-info "0.4"))

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:
;; [![Travis build status](https://travis-ci.org/emacs-pe/gh-md.el.png?branch=master)](https://travis-ci.org/emacs-pe/gh-md.el)

;; Render markdown using the [Github API](https://developer.github.com/v3/markdown/).

;; Usage:
;;
;; After install `gh-md.el' you can use the functions
;; `gh-md-render-region' and `gh-md-render-buffer' to generate a
;; preview of the markdown content of a buffer.
;;
;; ![gh-md.el screenshot](screenshot.png)

;;; Code:


(require 'json)
(require 'shr)
(require 'url)

(declare-function pkg-info-version-info "pkg-info" (library))

(defgroup gh-md nil
  "Render markdown using the github api."
  :prefix "gh-md-"
  :group 'applications)

(defcustom gh-md-use-gfm nil
  "Render using Github Flavored Markdown (GFM) by default ."
  :type 'boolean
  :group 'gh-md)

(defcustom gh-md-css-path "http://sindresorhus.com/github-markdown-css/github-markdown.css"
  "Path to css used output."
  :type 'string
  :group 'gh-md)

(defcustom gh-md-extra-header ""
  "Extra header used when converting to html."
  :type 'string
  :group 'gh-md)

(defvar gh-md-apiurl "https://api.github.com/markdown")
(defvar gh-md-buffer-name "*gh-md*")


(defun gh-md--json-payload (begin end &optional mode context)
  "Construct a json payload for the github markdown api."
  (let* ((text (buffer-substring-no-properties begin end))
         (mode (if gh-md-use-gfm "gfm" (or mode "markdown"))))
    (json-encode `((text . ,text)
                   (mode . ,mode)
                   (context . ,context)))))

(defun gh-md--generate-html (content &optional title)
  (mapconcat #'identity
             `("<!doctype html>"
               "<html>"
               "<head>"
               "<meta charset=\"utf-8\">"
               "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, minimal-ui\">"
               ,(and gh-md-css-path (format "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\"%s\">" gh-md-css-path))
               ,(and gh-md-extra-header gh-md-extra-header)
               "<style>"
               "body {"
               "  min-width: 200px;"
               "  max-width: 790px;"
               "  margin: 0 auto;"
               "  padding: 30px;}"
               "</style>"
               ,(and title (format "<title>%s</title>" title))
               "<body>"
               "<article class=\"markdown-body\">"
               ,content
               "</article>"
               "</body>"
               "</html>")
             "\n"))

(defun gh-md--export-file-name (&optional buffer)
  "Generate a export file name from BUFFER."
  (concat (file-name-sans-extension (or (buffer-file-name buffer)
                                        (buffer-name buffer)))
          ".html"))

;;;###autoload
(defalias 'gh-md-render-region #'gh-md-convert-region)

;;;###autoload
(defun gh-md-convert-region (begin end &optional export)
  "Show a preview the markdown from a region from BEGIN to END.

EXPORT writes a file."
  (interactive "r")
  (let ((url-request-method "POST")
        (url-request-data (gh-md--json-payload begin end))
        (url-request-extra-headers `(("User-Agent" . ,(format "gh-md.el/%s" (pkg-info-version-info 'gh-md)))))
        (title (format "Markdown Preview (%s)" (buffer-name)))
        (output-buffer (if export
                           (find-file-noselect (read-from-minibuffer "Export to file: " (gh-md--export-file-name)))
                         (get-buffer-create gh-md-buffer-name))))
    (url-retrieve gh-md-apiurl
                  (lambda (&rest _)
                    (let* ((response (with-current-buffer (current-buffer)
                                       (goto-char (point-min))
                                       (re-search-forward "^$" nil t)
                                       (buffer-substring (1+ (point)) (point-max))))
                           (content (decode-coding-string response 'utf-8))
                           (html (gh-md--generate-html content title)))
                      (with-current-buffer output-buffer
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (insert html)
                          (cond
                           (export
                            (save-buffer))
                           (t
                            (shr-render-region (point-min) (point-max))
                            (when (require 'eww nil 'noerror)
                              (eww-mode))
                            (goto-char (point-min))
                            (display-buffer (current-buffer)))))))))))

;;;###autoload
(defun gh-md-render-buffer (&optional buffer)
  "Render the markdown content from BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (gh-md-convert-region (point-min) (point-max))))

;;;###autoload
(defun gh-md-export-region (begin end)
  "Export to a file the markdown content from region BEGIN to END."
  (interactive "r")
  (gh-md-convert-region begin end t))

;;;###autoload
(defun gh-md-export-buffer (&optional buffer)
  "Export to a file the markdown content from BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (gh-md-convert-region (point-min) (point-max) t)))

(provide 'gh-md)

;;; gh-md.el ends here
