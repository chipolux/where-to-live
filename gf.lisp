;; GridFloat Parsing

(defun parse-float (n)
  "Parse an IEEE-754 32bit float from the provided integer."
  (let ((sign-bit (ldb (byte 1 31) n))
        (exponent (ldb (byte 8 23) n))
        (mantissa (ldb (byte 23 0) n)))
    (if (< exponent 255)
      (* (expt -1 sign-bit) (1+ (/ mantissa (expt 2 23))) (expt 2 (- exponent 127)))
      nil)))

(defun read-float (s &key (order :lsb))
  "Read an IEEE-754 32bit float from the provided stream."
  (let ((raw 0))
    (loop for i upto 3 do
          (setf raw (dpb (read-byte s)
                         (byte 8 (if (eq order :lsb) (* i 8) (- 24 (* i 8))))
                         raw)))
    (parse-float raw)))

(defun load-gf (p &key (ncols 3612) (nrows 3612) (order :lsb))
  "Load a GridFloat flt file with the given parameters."
  (with-open-file (s p :direction :input :element-type '(unsigned-byte 8))
    (loop repeat ncols collect (read-float s :order order))))
