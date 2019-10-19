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

(require 'cl-lib)
(require 'esi-utils)
(require 'esi-record)
(require 'esi-core)
(require 'f)

(defcustom esi-embed-enroll-dir (f-join user-emacs-directory "esi/enroll")
  "Directory to keep enrollment data in.")

(defvar esi-embed--model nil
  "User pointer to the loaded model")

(defun esi-embed-similarity (vec-a vec-b)
  "Return similarity for given vectors. Assume that the vectors
have same size.

NOTE: We assume that vectors are coming from the embedding model
      and so are already divided by L2 norm."
  (apply #'+ (cl-mapcar #'* vec-a vec-b)))

(defun esi-embed-load-model (&optional model-path)
  (let ((model-path (f-full (or model-path "./resources/embedding-model.pt"))))
    (unless (file-exists-p model-path)
      (error "Model file not found at %s" model-path))
    (unless esi-embed--model
      (setq esi-embed--model (esi-core--load-embed-model model-path)))))\

(defun esi-embed--embed-utterance (&optional sample-rate)
  "Record an utterance and return embedding for it."
  (esi-embed-load-model)
  (let* ((sample-rate (or sample-rate 16000))
         (wav (esi-record sample-rate)))
    (esi-core--embed-model-run esi-embed--model (esi-core-wav-to-samples wav) sample-rate)))

(defun esi-embed-enrolled-users ()
  "Return a list of users enrolled in the system."
  (ensure-dir esi-embed-enroll-dir)
  (mapcar #'file-name-base (f-directories esi-embed-enroll-dir)))

(defun esi-embed-saved-embeddings (name)
  "Return list of vectors saved under given NAME."
  (mapcar #'esi-utils-load-array (directory-files (f-join esi-embed-enroll-dir name) t "vec$")))

(defun esi-embed-enroll-user (name)
  "Enroll a user of given NAME using a supplied recording."
  (interactive "sName: ")
  (let ((ns-dir (f-join esi-embed-enroll-dir name)))
    (ensure-dir ns-dir)
    (esi-utils-save-array (esi-embed--embed-utterance) (f-join ns-dir (format-time-string "%Y%m%d%H%M%S.vec")))
    (message "Saved embedding. Total embeddings for %s: %d" name (length (esi-embed-saved-embeddings name)))))

(defun esi-embed-verify-user (name)
  "Record and check whether the given utterance comes from NAME."
  (interactive (list (completing-read "Available names: " (esi-embed-enrolled-users) nil t)))
  (let ((saved-vectors (esi-embed-saved-embeddings name))
        (vector (esi-embed--embed-utterance)))
    (message "Match similarity score: %s" (apply #'max (mapcar (lambda (v) (esi-embed-similarity vector v)) saved-vectors)))))

(provide 'esi-embed)

;;; esi-embed.el ends here
