;;; esi-llm.el --- LLM connections -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Abhinav Tushar

;;; Commentary:

;; LLM connections
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

(require 'org)

(defvar esi-llm--process nil
  "Running process for chatgpt.py script.")

(defvar esi-llm--buffer nil
  "Buffer for tracking LLM interactions.")

(defcustom esi-llm-buffer-name "*esi-llm*"
  "Name for the LLM interaction buffer."
  :type 'string)

(defcustom esi-llm-api-key nil
  "API key for using the LLM. This is injected in the call to
process script."
  :type 'string)

(defun esi-llm--md-to-org (text)
  "Assuming the LLM returns output in markdown (code blocks usually
are like this), convert text to org-mode for better preview."
  (with-temp-buffer
    (insert text)
    (shell-command-on-region (point-min) (point-max) "pandoc -f markdown -t org" t t)
    (buffer-string)))

(defun esi-llm--filter (proc string)
  "Filter function for writing the LLM process output. This is
supposed to put the output after `esi-llm-write' finishes writing
the prompt metadata."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (process-mark proc))
      (setq buffer-read-only nil)
      (insert (esi-llm--md-to-org (string-trim string)))
      (setq buffer-read-only t)
      (set-marker (process-mark proc) (point)))))

(defun esi-llm--sentinel (proc event)
  "Sentinel function for better handling exceptions from LLM script."
  (message (format "Process %s sent event '%s'" proc event)))

(defun esi-llm-running-p ()
  "Tell if the LLM system is running."
  (and (get-buffer esi-llm-buffer-name)
       esi-llm--process
       (process-live-p esi-llm--process)))

(defun esi-llm-initialize ()
  "Start the LLM process."
  (let ((process-environment (cl-copy-list process-environment)))
    (setenv "OPENAI_API_KEY" esi-llm-api-key)
    (setq esi-llm--buffer (get-buffer-create esi-llm-buffer-name))
    (save-excursion
      (with-current-buffer esi-llm--buffer (esi-llm-mode)))
    (when esi-llm--process
      (when (process-live-p esi-llm--process)
        (kill-process esi-llm--process))
      (setq esi-llm--process nil))
    (setq esi-llm--process
          (make-process :name "esi-llm"
                        :buffer esi-llm--buffer
                        :command '("chatgpt.py")
                        :filter #'esi-llm--filter
                        :sentinel #'esi-llm--sentinel))))

(defun esi-llm-write (text)
  "Write `text' to the process `esi-llm--process'."
  (let ((text (string-trim text)))
    (with-current-buffer (process-buffer esi-llm--process)
      (save-excursion
        (goto-char (point-max))
        (setq buffer-read-only nil)
        (insert "\n* ")
        (org-insert-time-stamp (current-time) t t)
        (insert "\n")
        (insert "#+begin_quote\n" text "\n#+end_quote\n\n")
        (setq buffer-read-only t)
        (set-marker (process-mark esi-llm--process) (point)))
      (process-send-string esi-llm--process (concat text "\n")))))

(defun esi-llm-read ()
  "Read text from `esi-llm--process' and return it.

Nothing happens here right now since we just keep the process buffer open.")

;;;###autoload;
(define-derived-mode esi-llm-mode org-mode
  "ESI-LLM"
  "Major mode for tracking interactions with an LLM."
  (setq buffer-read-only t))

(provide 'esi-llm)

;;; esi-llm.el ends here
