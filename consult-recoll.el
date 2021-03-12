;;; consult-recoll.el --- Recoll searches using consult  -*- lexical-binding: t; -*-

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Maintainer: Jose A Ortega Ruiz
;; Keywords: docs, convenience
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (consult "0.5"))
;; Homepage: https://codeberg.org/jao/consult-recoll

;; Copyright (C) 2021  Jose A Ortega Ruiz

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

;; A simple consult-recoll function to perform simple queries over
;; your Recoll (https://www.lesbonscomptes.com/recoll/) index.

;;; Code:

(require 'consult)

(defgroup consult-recoll nil
  "Options for consult recoll."
  :group 'consult)

(defcustom consult-recoll-open-fn #'find-file
  "Function used to open candidate URL.
It receives a single argument, the full path to the file to open."
  :type 'function)

(defface consult-recoll-url-face '((t :inherit default))
  "Face used to display URLs of candidates.")

(defface consult-recoll-title-face '((t :inherit italic))
  "Face used to display titles of candidates.")

(defvar consult-recoll-history nil "History for `consult-recoll'.")

(defvar consult-recoll--command
  "recollq -a -F \"url title\" ARG"
  "Command used to perform queries.")

(defun consult-recoll--transformer (str)
  "Decode STR, as returned by recollq."
  (unless (string-match-p "^\\(Recoll query:\\|[0-9]+ results\\| *$\\)" str)
    (let* ((cmps (split-string str " " t))
           (url+title (seq-map #'base64-decode-string cmps))
           (url (car url+title))
           (title (or (cadr url+title) (file-name-base url)))
           (url (if (string-prefix-p "file://" url) (substring url 7) url)))
      (format "%s (%s)"
              (propertize title 'face 'consult-recoll-title-face)
              (propertize url 'face 'consult-recoll-url-face)))))

(defun consult-recoll--open (candidate)
  "Open file of corresponding completion CANDIDATE."
  (when (string-match ".+ (\\(.+\\))$" (or candidate ""))
    (funcall consult-recoll-open-fn (match-string 1 candidate))))

(defun consult-recoll--search (&optional initial)
  "Perform an asynchronous recoll search via `consult--read'.
If given, use INITIAL as the starting point of the query."
  (consult--read (consult--async-command consult-recoll--command
                   (consult--async-filter (lambda (x) (not (null x))))
                   (consult--async-map #'consult-recoll--transformer))
                 :prompt "Recoll search: "
                 :require-match t
                 :initial (concat consult-async-default-split initial)
                 :history 'consult-recoll-history
                 :category 'recoll-result))

;;;###autoload
(defun consult-recoll ()
  "Consult recoll's local index."
  (interactive)
  (consult-recoll--open (consult-recoll--search)))

(provide 'consult-recoll)
;;; consult-recoll.el ends here
