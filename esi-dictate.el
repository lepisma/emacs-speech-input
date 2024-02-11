;;; esi-dictate.el --- Dictation Mode -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Dictation Mode
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

(require 'json)

(defcustom esi-dictate-dg-api-key nil
  "API Key for deepgram.")

(defvar esi-dictate--dg-process nil
  "Process holding the deepgram script")

(defface esi-dictate-intermittent-face
  '((t (:inherit company-preview)))
  "Face for transcription that's intermittent and could change
later.")

(define-minor-mode esi-dictate-mode
  "Toggle esi-dictate mode."
  :init-value nil)

(defun esi-dictate--clear-process ()
  (when esi-dictate--dg-process
    (delete-process esi-dictate--dg-process)
    (setq esi-dictate--dg-process nil)))

(defun esi-dictate-insert (transcription-item)
  "Insert transcription object in the current buffer preserving the
semantics of intermittent results."
  (let ((id (alist-get 'start transcription-item))
        (text (alist-get 'transcript (aref (alist-get 'alternatives (alist-get 'channel transcription-item)) 0)))
        (prev-item (get-text-property (- (point) 1) 'esi-dictate-transcription-item)))
    ;; If previous item and current are the same utterance, delete the previous
    ;; item and then insert new one.
    (when (and prev-item (= id (alist-get 'start prev-item)))
      (delete-region (get-text-property (- (point) 1) 'esi-dictate-start) (point)))
    (let ((start (point)))
      (insert text " ")
      (when (eq :false (alist-get 'is_final transcription-item))
        (overlay-put (make-overlay start (point)) 'face 'esi-dictate-intermittent-face))
      (put-text-property start (point) 'esi-dictate-transcription-item transcription-item)
      (put-text-property start (point) 'esi-dictate-start start))))

(defun esi-dictate-filter-fn (process string)
  (let ((existing (or (process-get process 'accumulated-output) "")))
    (setq existing (concat existing string))
    (while (string-match "\n" existing)
      (let ((line (substring existing 0 (match-beginning 0)))
            (rest (substring existing (match-end 0))))
        (setq existing rest)
        (when (string-prefix-p "Output: " line)
          (let ((json-string (substring line (length "Output: "))))
            (esi-dictate-insert (json-parse-string json-string :object-type 'alist))))))
    (process-put process 'accumulated-output existing)))

(defun esi-dictate-start ()
  "Start the real-time transcription process to start inserting text
in current buffer."
  (interactive)
  (esi-dictate--clear-process)
  (setq esi-dictate--dg-process
        (let ((process-environment (cons (format "DG_API_KEY=%s" esi-dictate-dg-api-key) process-environment)))
          (make-process :name "esi-dictate-dg"
                        :buffer "*esi-dictate-dg*"
                        :command (list "dg.py")
                        :filter #'esi-dictate-filter-fn))))

(defun esi-dictate-stop ()
  (interactive)
  (esi-dictate--clear-process))

(provide 'esi-dictate)

;;; esi-dictate.el ends here
