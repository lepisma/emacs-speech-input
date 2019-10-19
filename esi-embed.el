;;; esi-embed.el --- Speech embedding stuff -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Speech embedding stuff
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

(require 'esi-utils)
(require 'esi-record)
(require 'esi-core)
(require 'f)

(defcustom esi-embed-enroll-dir (f-join user-emacs-directory "esi/enroll")
  "Directory to keep enrollment data in.")

(defvar esi-embed--model nil
  "User pointer to the loaded model")

(defun esi-embed-load-model (&optional model-path)
  (let ((model-path (f-full (or model-path "./resources/embedding-model.pt"))))
    (unless (file-exists-p model-path)
      (error "Model file not found at %s" model-path))
    (unless esi-embed--model
      (setq esi-embed--model (esi-core--load-embed-model model-path)))))

(defun esi-embed-enroll-user (name)
  "Enroll a user of given NAME using a supplied recording."
  (interactive "sName: ")
  (esi-embed-load-model)
  (let* ((sample-rate 16000)
         (wav (esi-record sample-rate))
         (vector (esi-core--embed-model-run esi-embed--model (esi-core-wav-to-samples wav) sample-rate))
         (ns-dir (f-join esi-embed-enroll-dir name)))
    (unless (file-directory-p ns-dir)
      (make-directory ns-dir t))
    (esi-utils-save-array vector (f-join ns-dir (format-time-string "%Y%m%d%H%M%S.vec")))
    (message "Saved embedding. Total embeddings for %s: %d" name (length (directory-files ns-dir nil "vec$")))))

(provide 'esi-embed)

;;; esi-embed.el ends here
