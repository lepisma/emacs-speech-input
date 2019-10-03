;;; esi.el --- Emacs Speech Input -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
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
(require 'esi-kaldi)

(defcustom esi--arecord-args (list "-f" "S16_LE" "-r" "8000" "-c" "1" "-d" "600")
  "Arguments to send to arecord while recording. We put a max
duration limit so that an accident doesn't throw us out of memory.")

(defvar esi--arecord-proc nil
  "Variable holding the process used for recording.")

(defun esi-start-recording ()
  "Start recording audio."
  (let* ((tmp-file (make-temp-file "esi-raw-audio"))
         (args (append esi--arecord-args (list ">" (shell-quote-argument tmp-file)))))
    (setq esi--arecord-proc (start-process-shell-command "arecord" nil (string-join (cons "arecord" args) " ")))
    (process-put esi--arecord-proc 'output-file tmp-file)))

(defun esi-stop-recording ()
  "Stop recording and save wav output in OUTPUT-FILE."
  ;; NOTE: arecord takes kill (almost) gracefully but leaves the recording time
  ;;       wrong, so we fix it manually using sox
  (kill-process esi--arecord-proc)
  (let ((tmp-file (process-get esi--arecord-proc 'output-file))
        (processed-file (concat (make-temp-file "esi-audio") ".wav")))
    (call-process "sox" nil nil nil "--ignore-length" tmp-file processed-file)
    (setq esi--arecord-proc nil)
    (delete-file tmp-file)
    processed-file))

(defun esi-record ()
  "Ask for audio from user and return saved file path."
  (esi-start-recording)
  (read-string "Press RET when done speaking ")
  (esi-stop-recording))

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
