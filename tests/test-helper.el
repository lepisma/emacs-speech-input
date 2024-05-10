;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'seq)

(defun approx-equal (x y &optional rtol atol)
  (<= (abs (- x y)) (+ (* (or rtol 1e-5) (abs y)) (or atol 1e-8))))

(defun vector-approx-equal (a b &optional rtol atol)
  (and (= (length a)
          (length b)
          (apply #'+ (cl-mapcar (lambda (a b) (if (approx-equal a b rtol atol) 1 0)) a b)))))

(defun matrix-approx-equal (a b &optional rtol atol)
  (and (= (length a)
          (length b)
          (apply #'+ (cl-mapcar (lambda (a b) (if (vector-approx-equal a b rtol atol) 1 0)) a b)))))

(defun ffprobe (filepath &rest args)
  "Run ffprobe result for first stream on FILEPATH and return
sexp output. ARGS is a list of format option strings."
  (let ((file-arg (shell-quote-argument filepath))
        (format-args (mapconcat #'shell-quote-argument args " "))
        (json-object-type 'alist)
        (json-array-type 'list))
    (--> (format "ffprobe -v quiet -print_format json -show_streams %s %s" format-args file-arg)
       (shell-command-to-string it)
       (json-read-from-string it)
       (alist-get 'streams it)
       (car it))))

(defun markdown-remove-comments ()
  "Remove markdown style comments from the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "<!--" nil t)
    (let ((start (match-beginning 0)))
      (when (re-search-forward "-->" nil t)
        (delete-region start (match-end 0))))))

(defun dictate-break-input (text)
  "Break input text in content and command pair, if the separator is
present in `text'."
  (let ((splits (mapcar #'string-trim (string-split text "||"))))
    (if (= (length splits) 2)
        (cons (car splits) (cadr splits))
      splits)))

(defun dictate-break-case (text)
  "Break test cases in input output along with content and command
pair if the separator is there in input."
  (let ((splits (mapcar #'string-trim (string-split (string-trim text) "\n\n"))))
    (cons (dictate-break-input (car splits)) (cdr splits))))

(defun dictate-parse-cases (filepath)
  "Parse test cases from a markdown file and return a list."
  (with-temp-buffer
    (insert-file-contents filepath)
    (markdown-remove-comments)
    (goto-char (point-min))
    (let ((cases)
          (cursor (point)))
      (while (not (eobp))
        (if (re-search-forward "^-----$" nil t)
            (let ((case-text (buffer-substring-no-properties cursor (match-beginning 0)))
                  (end-point (match-end 0)))
              (push (dictate-break-case case-text) cases)
              (setq cursor end-point))
          (goto-char (point-max))))
      (reverse cases))))
