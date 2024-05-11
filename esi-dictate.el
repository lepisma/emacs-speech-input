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
(require 'llm)
(require 'llm-openai)


(defcustom esi-dictate-dg-api-key nil
  "API Key for Deepgram.")

(defcustom esi-dictate-openai-key (getenv "OPENAI_API_KEY")
  "API Key for OpenAI")

(defvar esi-dictate--dg-process nil
  "Process holding the deepgram script")

(defvar esi-dictate--llm-provider nil
  "Variable holding the LLM provider.")

(defvar esi-dictate--command-mode-p nil
  "Tell whether current utterance has to be taken as command. If
value is `nil', take utterance as actual utterance. If it's a
timestamp, any utterance from that time onwards is taken as a ...")

;; + When i continue pressing hotkey
;;   + keydown -> set the variable with timestamp, keep inserting in command mode.
;;   + keyup -> set variable back, process via llm, and make edits.
;; + When i press hotkey
;;   + keydown -> set the variable with timestamp, keep inserting in command mode.
;;   + wait for speech final -> ...
;; + Model based
;;   + send every utterance to llm for command detection -> it model says commmand
;;     complete, color it command and act.

(defvar esi-dictate--llm-examples (list (cons "I want to write something that's difficult to transcribe and then try correcting that. Write my name as abcd.\nInstruction: No separate the letters with . please."
                                              "I want to write something that's difficult to transcribe and then try correcting that. Write my name as a.b.c.d.")
                                        (cons "hi neumo, what are you doing?\nInstruction: it's n e m o"
                                              "hi nemo, what are you doing?"))
  "Example inputs and outputs for LLM few-shot learning.")

(defface esi-dictate-intermittent-face
  '((t (:inherit company-preview)))
  "Face for transcription that's intermittent and could change
later.")

(defface esi-dictate-command-face
  '((t (:inherit company-preview-common)))
  "Face for transcription that's to be used as correction or
suggestion instructions, also called commands.")

(defvar esi-dictate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'esi-dictate-stop)
    map)
  "Keymap for `esi-dictate-mode'.")

(define-minor-mode esi-dictate-mode
  "Toggle esi-dictate mode."
  :init-value nil
  :keymap esi-dictate-mode-map)

(defun esi-dictate-make-edits (content &optional command)
  "Give `command' to the LLM for making edits to the `content' and
return new content."
  (let ((prompt (make-llm-chat-prompt :context "You are a dictation assistant, you will be given transcript by the user and instruction to correct it. You have to return a corrected transcript without changing case of the text unless explicitly asked."
                                      :examples esi-dictate--llm-examples)))
    (llm-chat-prompt-append-response prompt (if command (concat content "\nInstruction: " command) content))
    (llm-chat esi-dictate--llm-provider prompt)))

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
                        :filter #'esi-dictate-filter-fn)))
  (setq esi-dictate--llm-provider
        (make-llm-openai :key esi-dictate-openai-key :chat-model "gpt-4-turbo")
        llm-warn-on-nonfree nil)
  (esi-dictate-mode)
  (message "Started dictation mode."))

(defun esi-dictate-stop ()
  (interactive)
  (esi-dictate--clear-process)
  (esi-dictate-mode -1)
  (message "Stopped dictation mode."))

(provide 'esi-dictate)

;;; esi-dictate.el ends here
