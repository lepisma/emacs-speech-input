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
  (cl-map 'vector (lambda (line) (cl-map 'vector #'string-to-number (s-split " " line))) (s-split "\n" (s-trim (f-read-text filepath)))))

(defun vector-from-file (filepath)
  "Read a newline separated vector (also numpy.savetxt style) from given file."
  (cl-map 'vector #'string-to-number (s-split "\n" (s-trim (f-read-text filepath)))))

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

(defun fft-real (vector)
  (cl-map 'vector (lambda (elem) (aref elem 0)) vector))

(defun fft-imag (vector)
  (cl-map 'vector (lambda (elem) (aref elem 1)) vector))

(defun stft-real (matrix)
  (cl-map 'vector #'fft-real matrix))

(defun stft-imag (matrix)
  (cl-map 'vector #'fft-imag matrix))

(describe "Sample reading"
  (it "is correct"
    (let ((samples (esi-core-wav-to-samples (with-temp-buffer
                                              (insert-file-contents-literally "tests/resources/hello.wav")
                                              (buffer-string))))
          (true-samples (vector-from-file "tests/resources/hello.samples")))
      (expect (vector-approx-equal true-samples samples) :to-be t))))

(describe "FFT"
  (it "has correct real values"
    (let* ((samples (vector-from-file "tests/resources/hello.samples"))
           (fft (esi-core--rfft samples))
           (true-real-values (vector-from-file "tests/resources/hello.fft.real")))
      (expect (vector-approx-equal true-real-values (fft-real fft)))))
  (it "has correct imaginary values"
    (let* ((samples (vector-from-file "tests/resources/hello.samples"))
           (fft (esi-core--rfft samples))
           (true-imag-values (vector-from-file "tests/resources/hello.fft.imag")))
      (expect (vector-approx-equal true-imag-values (fft-imag fft))))))

(describe "STFT"
  (it "has correct real values"
    (let* ((samples (vector-from-file "tests/resources/hello.samples"))
           (stft (esi-core--stft samples 2048 512))
           (true-real-values (matrix-from-file "tests/resources/hello.stft.real")))
      (expect (matrix-approx-equal true-real-values (stft-real stft)))))
  (it "has correct imaginary values"
    (let* ((samples (vector-from-file "tests/resources/hello.samples"))
           (stft (esi-core--stft samples 2048 512))
           (true-imag-values (matrix-from-file "tests/resources/hello.stft.imag")))
      (expect (matrix-approx-equal true-imag-values (stft-imag stft))))))

(describe "Spectrogram"
  (it "is correct"
    (let ((samples (vector-from-file "tests/resources/hello.samples"))
          (true-spectrogram (matrix-from-file "tests/resources/hello.spectrogram")))
      (expect (matrix-approx-equal true-spectrogram (esi-core--spectrogram samples 2048 512 2))))))

(describe "Mel filterbank"
  (it "is correct"
    (let ((true-melfb (matrix-from-file "tests/resources/mel.fb"))
          (melfb (esi-core--mel-filter 8000 2048 12)))
      (expect (matrix-approx-equal melfb true-melfb)))))
