(require 'esi-core)
(require 'cl-extra)
(require 'cl-lib)
(require 'f)
(require 's)

(describe "Version"
  (it "is correct"
    (expect (esi-core-version) :to-equal "0.0.1")))

(defun matrix-from-file (filepath)
  "Read a numpy.savetxt style matrix from given file."
  (cl-map 'vector (lambda (line) (cl-map 'vector #'string-to-number (s-split " " line))) (s-split "\n" (f-read-text filepath))))

(defun vector-from-file (filepath)
  "Read a newline separated vector from given file."
  (cl-map 'vector #'string-to-number (s-split "\n" (f-read-text filepath))))

(defun approx-equal (x y &optional fuzz-factor)
  (or (= x y)
      (< (/ (abs (- x y))
            (max (abs x) (abs y)))
         (or fuzz-factor 1.0e-6))))

(defun vector-approx-equal (a b &optional fuzz-factor)
  (and (= (length a)
          (length b)
          (apply #'+ (cl-mapcar (lambda (a b) (if (approx-equal a b) 1 0)) a b)))))

(defun matrix-approx-equal (a b &optional fuzz-factor)
  (and (= (length a)
          (length b)
          (apply #'+ (cl-mapcar (lambda (a b) (if (vector-approx-equal a b) 1 0)) a b)))))

(describe "Sample reading"
  (it "is correct"
    (let ((samples (esi-core-wav-to-samples (with-temp-buffer
                                              (insert-file-contents-literally "tests/resources/hello.wav")
                                              (buffer-string))))
          (true-samples (vector-from-file "tests/resources/hello.samples")))
      (expect (vector-approx-equal true-samples samples) :to-be t))))

(describe "Spectrogram"
  (it "is correct"
    (let ((samples (vector-from-file "tests/resources/hello.samples"))
          (true-spectrogram (matrix-from-file "tests/resources/hello.spectrogram")))
      (expect (matrix-approx-equal true-spectrogram (esi-core-samples-to-spectrogram samples 2048 512 2))))))
