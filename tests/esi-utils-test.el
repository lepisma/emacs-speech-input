(require 'esi-utils)

(describe "Vector"
  (it "parses properly"
    (expect (vector-approx-equal (esi-utils--array-parse-line "1 2 3.4 0.2 \n") [1.0 2.0 3.4 0.2]) :to-be t))
  (it "formats properly"
    (expect (string= "1.0 2.0 3.4 0.2" (esi-utils--array-format-line [1.0 2.0 3.4 0.2])) :to-be t)))

(describe "Array IO"
  :var (filepath)
  (before-all
    (setq filepath (make-temp-file "esi-test")))

  (after-all
    (delete-file filepath))

  (it "works for vector"
    (let ((array [1 2 3 4 5.0]))
      (esi-utils-save-array array filepath)
      (expect (vector-approx-equal array (esi-utils-load-array filepath)))))

  (it "works for matrix"
    (let ((array [[1 2 3] [4 5 6]]))
      (esi-utils-save-array array filepath)
      (expect (matrix-approx-equal array (esi-utils-load-array filepath))))))
