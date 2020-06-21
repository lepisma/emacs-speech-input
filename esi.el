;;; esi.el --- Emacs Speech Input -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.5
;; Package-Requires: ((emacs "26") (dash "2.17.0") (dash-functional "2.17.0") (f "0.20.0") (helm "3.6.2") (s "1.12.0"))
;; Keywords: tools
;; URL: https://github.com/lepisma/emacs-speech-input

;;; Commentary:

;; Emacs Speech Input
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

(require 'dash)
(require 'dash-functional)
(require 'helm)
(require 'esi-record)
(require 'esi-kaldi)

;;;###autoload
(defun esi-insert-text (transcriber)
  "Insert transcription at point, selecting among ASR
alternatives."
  (interactive (list #'esi-kaldi-transcribe))
  (let ((texts  (->> (funcall transcriber (esi-record))
                   (alist-get 'results)
                   car
                   (alist-get 'alternatives)
                   (mapcar (-cut alist-get 'transcript <>)))))
    (helm :sources (helm-build-sync-source "alternatives"
                     :candidates texts
                     :action `(("Insert" . ,#'insert)))
          :buffer "*helm esi*"
          :prompt "Insert : ")))

(provide 'esi)

;;; esi.el ends here
