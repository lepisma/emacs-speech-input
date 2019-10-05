(require 'esi-core)
(require 'cl-extra)
(require 'cl-lib)
(require 'f)
(require 's)

(describe "Version"
  (it "is correct"
    (expect (esi-core-version) :to-equal "0.0.1")))

(defun numerical-list-from-file (filepath)
  (mapcar #'string-to-number (s-split "\n" (f-read-text filepath))))

(defun numerical-approx-equal (x y &optional fuzz-factor)
  (or (= x y)
      (< (/ (abs (- x y))
            (max (abs x) (abs y)))
         (or fuzz-factor 1.0e-6))))

(defun numerical-list-approx-equal (a b &optional fuzz-factor)
  (and (= (length a)
          (length b)
          (apply #'+ (cl-mapcar (lambda (a b) (if (numerical-approx-equal a b) 1 0)) a b)))))

(describe "Sample reading"
  (it "is correct"
    (let ((samples (esi-core-wav-to-samples (with-temp-buffer
                                              (insert-file-contents-literally "tests/resources/hello.wav")
                                              (buffer-string))))
          (true-samples (numerical-list-from-file "tests/resources/hello.samples")))
      (expect (numerical-list-approx-equal samples true-samples) :to-be t))))
