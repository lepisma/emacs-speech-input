(require 'cl-lib)

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
