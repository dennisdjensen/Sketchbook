(defun fac (n)
  (if (= n 1)
      1
      (* n (fac (1- n)))))

(defun factorial-digit-sum (n)
  (reduce #'(lambda (sum d)
              (+ sum (- d #.(char-int #\0))))
          (with-output-to-string (s)
            (princ (fac n) s))
          :initial-value 0
          :key #'char-int))

(princ (factorial-digit-sum 100))
