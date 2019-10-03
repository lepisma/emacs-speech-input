;;; esi-kaldi.el --- Kaldi based input mechanism -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Kaldi based input mechanism following kaldi-serve's protobuf.
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

;; HACK: We are using various command line tools to stitch things together for
;;       now. This will change once the outer API becomes more clear to me.

(defcustom esi-kaldi-serve-proto nil
  "Path to the protobuf file describing kaldi-serve service.")

(defcustom esi-kaldi-serve-config `((config . ((max_alternatives . 10)
                                               (model . "general")
                                               (language_code . "en"))))
  "Extra config to be passed in grpc requests.")

(defun esi-kaldi-encoder-wav (wavfile)
  "Encoder wavfile to base64 format so that it could be passed to evans."
  (with-temp-buffer
    (insert-file-contents-literally wavfile)
    (base64-encode-region (point-min) (point-max) t)
    (buffer-string)))

(defun esi-kaldi-transcribe (wavfile)
  "Transcribe provided file and return everything else."
  (let* ((args (format "--package kaldi_serve --service KaldiServe %s --call Recognize --port 5016" esi-kaldi-serve-proto))
         (audio-data (esi-kaldi-encoder-wav wavfile))
         (data-json (shell-quote-argument (json-encode `((audio . ((content . ,audio-data))) ,@esi-kaldi-serve-config)))))
    (json-parse-string (shell-command-to-string (format "echo %s | evans %s" data-json args))
                       :object-type 'alist
                       :array-type 'list)))

(provide 'esi-kaldi)

;;; esi-kaldi.el ends here
