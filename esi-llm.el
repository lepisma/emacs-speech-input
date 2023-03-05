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

(defvar *esi-llm-process* nil
  "Running process for chatgpt.py script.")

(defcustom esi-llm-api-key nil
  "API key for using the LLM. This is injected in the call to
process script."
  :type 'string)

(defun esi-llm-initialize ()
  "Start the LLM process."
  (let ((process-environment (cl-copy-list process-environment)))
    (setenv "OPENAI_API_KEY" esi-llm-api-key)
    (setq *esi-llm-process*
          (make-process :name "esi-llm"
                        :buffer "*esi-llm*"
                        :command '("chatgpt.py")))))

(defun esi-llm-write (text)
  "Write `text' to the process `*esi-llm-process*'."
  (process-send-string *esi-llm-process* (concat text "\n")))

(defun esi-llm-read ()
  "Read text from `*esi-llm-process*' and return it.

Nothing happens here right now since we just keep the process buffer open.")

(provide 'esi-llm)

;;; esi-llm.el ends here
