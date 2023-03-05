;;; esi-transcribe.el --- Transcribing functions -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Abhinav Tushar

;;; Commentary:

;; Transcribing functions
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

(require 'whisper)

(defvar esi-transcribe-callback nil
  "Callback function to trigger after transcription is done.")

(defun whisper--transcribe-audio ()
  "Start audio transcribing process in the background.

This is a patched function with a sentinel that doesn't write "
  (message "[-] Transcribing/Translating audio")
  (setq whisper--transcribing-process
        (make-process
         :name "whisper-transcribing"
         :command (whisper--transcribe-command whisper--temp-file)
         :connection-type nil
         :buffer (get-buffer-create whisper--stdout-buffer-name)
         :stderr (get-buffer-create whisper--stderr-buffer-name)
         :coding 'utf-8
         :sentinel (lambda (_process event)
                     (unwind-protect
                         (let ((whisper--stdout-buffer (get-buffer whisper--stdout-buffer-name)))
                           (with-current-buffer whisper--stdout-buffer
                             (when (string-equal "finished\n" event)
                               (goto-char (point-min))
                               (when (search-forward-regexp "." nil t)
                                 (delete-region (point-min) (point))
                                 (with-current-buffer (marker-buffer whisper--marker)
                                   (goto-char whisper--marker)
                                   (let ((transcription (with-current-buffer whisper--stdout-buffer (buffer-string))))
                                     (if (null esi-transcribe-callback)
                                         (insert transcription)
                                       (funcall esi-transcribe-callback transcription)
                                       (setf esi-transcribe-callback nil)))
                                   (goto-char whisper--marker))))))
                       (set-marker whisper--marker nil)
                       (setq whisper--point-buffer nil)
                       (kill-buffer whisper--stdout-buffer-name)
                       (kill-buffer whisper--stderr-buffer-name)
                       (message nil))))))

(defun esi-transcribe (callback)
  "Record, transcribe, and call `callback' on the text."
  (interactive)
  (whisper-run)
  (read-string "Press RET when done speaking ")
  (setf esi-transcribe-callback callback)
  (whisper-run))

(provide 'esi-transcribe)

;;; esi-transcribe.el ends here
