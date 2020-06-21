;;; esi-search.el --- Voice search stuff -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Voice search stuff
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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'helm)
(require 'cl-lib)

(defcustom esi-search-scoring-fn 'helm-score-candidate-for-pattern
  "Scoring function to match a pattern with candidate string.

A scoring function takes two arguments, a string pattern and a
candidate (also string). The current default does character level
bi-gram matching which might not be very suitable for voice use
cases and we might want more phonemic matching as fallback."
  :type 'symbol)

(defun esi-search-sort (text candidates)
  "Sort CANDIDATES in order according to given TEXT and `esi-search-scoring-fn'.

Each item in CANDIDATES is a pair of string representation to be
matched on and the item itself. A list similar to candidate is
returned with string representation replaced with match score."
  (let ((with-scores (mapcar (lambda (candidate) (cons (funcall esi-search-scoring-fn text (car candidate)) (cdr candidate))) candidates)))
    (cl-sort with-scores #'> :key #'car)))

(defun esi-search-filter (scored-candidates)
  "Filter out spurious matches from SCORED-CANDIDATES.

SCORED-CANDIDATES is a list return from `esi-search-sort' fn."
  (let ((best-score (caar scored-candidates)))
    (unless (zerop best-score)
      ;; NOTE: Divide by 2 heuristic might or might not work in a general case.
      ;;       Depends on how annoying the spurious matches are for you.
      (cl-remove-if (lambda (candidate) (< (car candidate) (/ best-score 2.0))) scored-candidates))))

(provide 'esi-search)

;;; esi-search.el ends here
