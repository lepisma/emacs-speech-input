;;; esi-utils.el --- General utilities for esi -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; General utilities for esi
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 's)
(require 'f)
(require 'cl-lib)

(defun ensure-dir (dir)
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun esi-utils--array-format-line (it)
  "Take a number or vector and return a string representing IT as
a single line."
  (cl-typecase it
    (vector (s-join " " (mapcar #'number-to-string it)))
    (t (number-to-string it))))

(defun esi-utils--array-parse-line (line)
  "Parse given LINE in the stored array format and return either
a vector or number."
  (let ((splits (s-split " " (s-trim line))))
    (if (> (length splits) 1)
        (cl-map 'vector #'string-to-number splits)
      (string-to-number (car splits)))))

(defun esi-utils-save-array (array filepath)
  "Save provided ARRAY (vector or 2d) in numpy style format."
  (let ((text (s-join "\n" (mapcar #'esi-utils--array-format-line array))))
    (f-write-text text 'utf-8 filepath)))

(defun esi-utils-load-array (filepath)
  "Load array from the given filepath."
  (let ((text (s-trim (f-read-text filepath 'utf-8))))
    (cl-map 'vector #'esi-utils--array-parse-line (s-split "\n" text))))

(provide 'esi-utils)

;;; esi-utils.el ends here
