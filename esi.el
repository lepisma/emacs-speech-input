;;; esi.el --- Emacs Speech Input -*- lexical-binding: t; -*-

;; Copyright (c) 2019-2023 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.2.1
;; Package-Requires: ((emacs "27") (dash "2.19.1") (f "0.20.0") (s "1.13.0"))
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
(require 'esi-llm)
(require 'esi-transcribe)

;;;###autoload
(defun esi-assist (&optional arg)
  "Trigger LLM assistant. If `arg' is present, prompt for text
input in minibuffer, otherwise start recording voice input."
  (interactive "P")
  (unless (esi-llm-running-p)
    (esi-llm-initialize))
  (if arg
      (esi-llm-write (read-from-minibuffer "esi-assist: "))
    (esi-transcribe #'esi-llm-write)))

(provide 'esi)

;;; esi.el ends here
