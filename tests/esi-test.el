(require 'esi-core)
(require 'esi-utils)
(require 'cl-extra)
(require 'cl-lib)
(require 'f)
(require 's)

(describe "Version"
  (it "is correct"
    (expect (esi-core-version) :to-equal "0.0.2")))

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
          (true-samples (esi-utils-load-array "tests/resources/hello.samples")))
      (expect (vector-approx-equal true-samples samples) :to-be t))))

(describe "FFT"
  (it "has correct real values"
    (let* ((samples (esi-utils-load-array "tests/resources/hello.samples"))
           (fft (esi-core--rfft samples))
           (true-real-values (esi-utils-load-array "tests/resources/hello.fft.real")))
      (expect (vector-approx-equal true-real-values (fft-real fft)))))
  (it "has correct imaginary values"
    (let* ((samples (esi-utils-load-array "tests/resources/hello.samples"))
           (fft (esi-core--rfft samples))
           (true-imag-values (esi-utils-load-array "tests/resources/hello.fft.imag")))
      (expect (vector-approx-equal true-imag-values (fft-imag fft))))))

(describe "STFT"
  (it "has correct real values"
    (let* ((samples (esi-utils-load-array "tests/resources/hello.samples"))
           (stft (esi-core--stft samples 2048 512))
           (true-real-values (esi-utils-load-array "tests/resources/hello.stft.real")))
      (expect (matrix-approx-equal true-real-values (stft-real stft)))))
  (it "has correct imaginary values"
    (let* ((samples (esi-utils-load-array "tests/resources/hello.samples"))
           (stft (esi-core--stft samples 2048 512))
           (true-imag-values (esi-utils-load-array "tests/resources/hello.stft.imag")))
      (expect (matrix-approx-equal true-imag-values (stft-imag stft))))))

(describe "Spectrogram"
  (it "is correct"
    (let ((samples (esi-utils-load-array "tests/resources/hello.samples"))
          (true-spectrogram (esi-utils-load-array "tests/resources/hello.spectrogram")))
      (expect (matrix-approx-equal true-spectrogram (esi-core--spectrogram samples 2048 512 2))))))

(describe "Mel filterbank"
  (it "is correct"
    (let ((true-melfb (esi-utils-load-array "tests/resources/mel.fb"))
          (melfb (esi-core--mel-filter 8000 2048 12)))
      (expect (matrix-approx-equal melfb true-melfb)))))

(describe "Mel spectrogram"
  (it "is correct"
    (let ((samples (esi-utils-load-array "tests/resources/hello.samples"))
          (true-spectrogram (esi-utils-load-array "tests/resources/hello.mel.spectrogram")))
      (expect (matrix-approx-equal true-spectrogram (esi-core--mel-spectrogram samples 8000 2048 512 40))))))
