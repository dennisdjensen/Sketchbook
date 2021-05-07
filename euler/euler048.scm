;;; Copyright 2021 Dennis Decker Jensen
;;; Date: 7 May 2021
;;; Purpose: Calculate the sum of self powers 1..1000
;;; Tectonics: chibi-scheme -q euler048.scm

(define sum
  (lambda (n)
    (if (= n 0)
        0
        (+ (expt n n) (sum (- n 1))))))

(define last-digits
  (lambda (n ds)
    (let ((s (number->string (sum n))))
      (substring s (- (string-length s) ds)))))

(display (last-digits 1000 10))
(newline)