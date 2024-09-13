;;; esi-dictate.el --- Dictation with Real-Time Editing -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29") (llm "0.17.2"))
;; Keywords: speech
;; URL: https://github.com/lepisma/emacs-speech-input

;;; Commentary:

;; Dictation with Real-Time Editing
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
  "API Key for Deepgram."
  :type 'string)

(defcustom esi-dictate-llm-provider nil
  "LLM provider to use for corrections")

(defcustom esi-dictate-speech-final-hook nil
  "Hook to keep functions that run once the speech utterance is
finalized from the ASR."
  :type ':hook)

(defcustom esi-dictate-cursor "⤳"
  "Symbol for voice cursor."
  :type 'string)

(defvar esi-dictate--dg-process nil
  "Process holding the deepgram script")

(defcustom esi-dictate-llm-prompt "You are a dictation assistant, you will be given transcript by the user with speech disfluencies, minor mistakes, and edits and you have to return a corrected transcript. The user might give you their stream of consciousness and you have to ensure that you correctly identify a request to edit and don't misfire. You don't have to generate any new information, just ensure fixes in spoken transcripts and edits as asked."
  "System prompt for the LLM editor."
  :type 'string)

(defcustom esi-dictate-fix-examples (list (cons "I wan to write about umm something related to food. My name is name is Abhinav"
                                                "I want to write about umm something related to food. My name is Abhinav.")
                                          (cons "Okay we will start. Let's write something about chairs. No not chairs, make it tables."
                                                "Let's write something about tables.")
                                          (cons "I want to write something that's difficult to transcribe and then try correcting that. Write my name as abcd. No separate the letters with . please"
                                                "I want to write something that's difficult to transcribe and then try correcting that. Write my name as a.b.c.d.")
                                          (cons "hi easy, what are you doing? It's e s i."
                                                "hi esi, what are you doing?"))
  "Example inputs and outputs for few shot learning of auto
edits. Change this to impact the behaviour of dictation
intelligence."
  :type '(repeat (cons string string)))

(defface esi-dictate-intermittent-face
  '((t (:inherit font-lock-comment-face)))
  "Face for transcription that's intermittent from ASR and could
change later.")

(defface esi-dictate-context-face
  '((t (:inherit link)))
  "Face for marking region of text that's under consideration as
voice context (used in context-overlay). This is the region
that's send to the LLM for edits.")

(defface esi-dictate-cursor-face
  '((t (:inherit default)))
  "Face to use for the `esi-dictate-cursor'.")

(defvar esi-dictate-mode-map
  (make-sparse-keymap)
  "Keymap for `esi-dictate-mode'.")

(defvar-local esi-dictate-context-overlay nil
  "Overlay that specifies the region to be used as context
for corrections. The end position of this is also the position
where insertion happens from the ASR transcription. We also use
this to track position of the context.")

(define-minor-mode esi-dictate-mode
  "Toggle esi-dictate mode."
  :init-value nil
  :keymap esi-dictate-mode-map)

(defun esi-dictate--fix (content)
  "Perform general fixes to given `content' assuming it's coming
from dictation with speech disfluencies and other artifacts."
  (let ((prompt (make-llm-chat-prompt :context esi-dictate-llm-prompt
                                      :examples esi-dictate-fix-examples)))
    (llm-chat-prompt-append-response prompt content)
    (llm-chat esi-dictate-llm-provider prompt)))

(defun esi-dictate-fix-context ()
  "Fix the context using the general transcription fixing
instructions."
  (interactive)
  (let* ((beg-pos (overlay-start esi-dictate-context-overlay))
         (end-pos (overlay-end esi-dictate-context-overlay))
         (content (buffer-substring-no-properties beg-pos end-pos))
         (edited (esi-dictate--fix content))
         (past-point (point)))
    (delete-region beg-pos end-pos)
    (goto-char beg-pos)
    (insert edited)
    ;; We replicate save-excursion manually since we do full deletion and
    ;; replacement.
    (let ((current-point (point)))
      (if (<= past-point beg-pos)
          (goto-char past-point)
        (if (<= past-point end-pos)
            (goto-char (min past-point current-point))
          (goto-char (+ current-point (- past-point end-pos)))))
      ;; Recover the overlay
      (move-overlay esi-dictate-context-overlay beg-pos current-point))))

(defun esi-dictate--clear-process ()
  (when esi-dictate--dg-process
    (delete-process esi-dictate--dg-process)
    (setq esi-dictate--dg-process nil)))

(defun esi-dictate-make-context-overlay ()
  "Make and return new context overlay."
  (let ((overlay (if (region-active-p)
                     (make-overlay (region-beginning) (region-end) nil nil t)
                   (make-overlay (point) (point) nil nil t))))
    (overlay-put overlay 'face 'esi-dictate-context-face)
    (overlay-put overlay 'after-string (propertize esi-dictate-cursor 'face 'esi-dictate-cursor-face))
    overlay))

(defun esi-dictate-clear-context-overlay ()
  (when esi-dictate-context-overlay
    (delete-overlay esi-dictate-context-overlay))
  (setq esi-dictate-context-overlay nil))

(defun esi-dictate-insert (transcription-item)
  "Insert transcription object in the current buffer preserving the
semantics of intermittent results."
  (let* ((id (alist-get 'start transcription-item))
         (text (alist-get 'transcript (aref (alist-get 'alternatives (alist-get 'channel transcription-item)) 0)))
         (prev-item (get-text-property (- (overlay-end esi-dictate-context-overlay) 1) 'esi-dictate-transcription-item)))
    ;; If previous item and current are the same utterance, delete the previous
    ;; item and then insert new one. This handles intermittent results from the
    ;; ASR.
    (when (and prev-item (= id (alist-get 'start prev-item)))
      (delete-region (get-text-property (- (overlay-end esi-dictate-context-overlay) 1) 'esi-dictate-start) (overlay-end esi-dictate-context-overlay)))

    (let ((insertion-pos (overlay-end esi-dictate-context-overlay)))
      (save-excursion
        (goto-char insertion-pos)
        (insert text " "))
      (when (eq :false (alist-get 'is_final transcription-item))
        (overlay-put (make-overlay insertion-pos (overlay-end esi-dictate-context-overlay)) 'face 'esi-dictate-intermittent-face))

      ;; Saving properties which will be read later to handle intermittent
      ;; results.
      (put-text-property insertion-pos (overlay-end esi-dictate-context-overlay) 'esi-dictate-transcription-item transcription-item)
      (put-text-property insertion-pos (overlay-end esi-dictate-context-overlay) 'esi-dictate-start insertion-pos)

      ;; This is utterance end according to the ASR. In this case, we run a
      ;; few hooks.
      (when (not (eq :false (alist-get 'speech_final transcription-item)))
        (run-hooks 'esi-dictate-speech-final-hook)))))

(defun esi-dictate-filter-fn (process string)
  "Filter function to read the output from python script that
 interacts with Deeepgram."
  (let ((existing (or (process-get process 'accumulated-output) "")))
    (setq existing (concat existing string))
    (while (string-match "\n" existing)
      (let ((line (substring existing 0 (match-beginning 0)))
            (rest (substring existing (match-end 0))))
        (setq existing rest)
        (cond ((string-prefix-p "Output: " line)
               (let ((json-string (substring line (length "Output: "))))
                 (esi-dictate-insert (json-parse-string json-string :object-type 'alist))))
              ((string-prefix-p "Press Enter to stop recording" line)
               (message "[esi] Dictation mode ready to use.")
               (esi-dictate-move-here)))))
    (process-put process 'accumulated-output existing)))

(defun esi-dictate-move-here ()
  "Move the voice cursor to the current point or, if active, the
current region."
  (interactive)
  (esi-dictate-clear-context-overlay)
  (setq esi-dictate-context-overlay (esi-dictate-make-context-overlay)))

;;;###autoload
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
  (esi-dictate-mode)
  (message "[esi] Starting dictation mode ..."))

(defun esi-dictate-stop ()
  (interactive)
  (esi-dictate--clear-process)
  (esi-dictate-mode -1)
  (esi-dictate-clear-context-overlay)
  (message "[esi] Stopped dictation mode."))

(provide 'esi-dictate)

;;; esi-dictate.el ends here
