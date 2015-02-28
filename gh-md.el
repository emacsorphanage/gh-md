;;; gh-md.el --- Render markdown using the github api  -*- lexical-binding: t; -*-

;; Copyright © 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/gh-md.el
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (markdown-mode "2.0"))

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

;;; Code:


(eval-when-compile (require 'cl))

(require 'json)
(require 'shr)
(require 'url)

(require 'markdown-mode)


(defgroup gh-md nil
  "Render markdown using the github api."
  :prefix "gh-md-"
  :group 'applications)

(defcustom gh-md-use-gfm nil
  "Render using Github Flavored Markdown (GFM) by default ."
  :type 'boolean
  :group 'gh-md)

(defvar gh-md-apiurl "https://api.github.com/markdown")
(defvar gh-md-apiurl-raw "https://api.github.com/markdown/raw")
(defvar gh-md-buffer-name "*gh-md*")


(defun gh-md-construct-json-payload (begin end &optional mode context)
  "Construct a json payload for the github markdown api."
  (let* ((text (buffer-substring-no-properties begin end))
         (mode (if gh-md-use-gfm "gfm" (or mode "markdown")))
         (context (and (string= mode "gfm") context)))
    (json-encode `((text . ,text)
                   (mode . ,mode)
                   (context . ,context)))))

;;;###autoload
(defun gh-md-render-region (begin end &optional title)
  "Show a preview the markdown from a region from BEGIN to END."
  (interactive "r")
  (let ((url-request-method "POST")
        (url-request-data (gh-md-construct-json-payload begin end))
        (title (or title (format "Markdown Preview (%s)" (buffer-name)))))
    (url-retrieve gh-md-apiurl
                  (lambda (&rest _)
                    (let ((response (with-current-buffer (current-buffer)
                                      (goto-char (point-min))
                                      (re-search-forward "^$" nil t)
                                      (buffer-substring-no-properties (point) (point-max)))))
                      (with-current-buffer (get-buffer-create gh-md-buffer-name)
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (insert response)
                          (goto-char (point-min))
                          (unless (markdown-output-standalone-p)
                            (markdown-add-xhtml-header-and-footer title))
                          (shr-render-region (point-min) (point-max))
                          (when (require 'eww nil 'noerror)
                            (eww-mode)))
                        (display-buffer (current-buffer))))))))

;;;###autoload
(defun gh-md-render-buffer (&optional buffer title)
  "Render the markdown contents from BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (gh-md-render-region (point-min) (point-max) title)))

(provide 'gh-md)

;;; gh-md.el ends here
