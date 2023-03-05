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

(defcustom esi-llm-api-key nil
  "API key for using the LLM. This is injected in the call to
process script."
  :type 'string)

(defun esi-lm--md-to-org (text)
  (with-temp-buffer
    (insert text)
    (shell-command-on-region (point-min) (point-max) "pandoc -f markdown -t org" t t)
    (buffer-string)))

(defun esi-llm--filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert (esi-lm--md-to-org (string-trim string)))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun esi-llm-initialize ()
  "Start the LLM process."
  (let ((process-environment (cl-copy-list process-environment)))
    (setenv "OPENAI_API_KEY" esi-llm-api-key)
    (setq esi-llm--buffer (get-buffer-create "*esi-llm*"))
    (save-excursion
      (with-current-buffer esi-llm--buffer
        (esi-llm-mode)))
    (setq esi-llm--process
          (make-process :name "esi-llm"
                        :buffer esi-llm--buffer
                        :command '("chatgpt.py")
                        :filter #'esi-llm--filter))))

(defun esi-llm-write (text)
  "Write `text' to the process `esi-llm--process'."
  (with-current-buffer (process-buffer esi-llm--process)
    (save-excursion
      (goto-char (point-max))
      (org-insert-heading)
      (org-insert-time-stamp (current-time) t t)
      (insert "\n")
      (insert "#+begin_quote\n" text "\n#+end_quote\n\n")
      (set-marker (process-mark esi-llm--process) (point)))
    (process-send-string esi-llm--process (concat (string-trim text) "\n"))))

(defun esi-llm-read ()
  "Read text from `esi-llm--process' and return it.

Nothing happens here right now since we just keep the process buffer open.")

;;;###autoload;
(define-derived-mode esi-llm-mode org-mode
  "ESI-LLM"
  "Major mode for tracking interactions with an LLM.")

(provide 'esi-llm)

;;; esi-llm.el ends here
