(require 'cl-lib)
(require 'dash)
(require 'json)

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
