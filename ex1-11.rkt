(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f2 n)
  (define (f2-iter f_1 f_2 f_3 counter max)
    (if (< max 3)
        max
        (if (= counter max)
            (+ f_3 (* f_2 2) (* f_1 3))
            (f2-iter f_2 f_3 (+ f_3 (* f_2 2) (* f_1 3)) (+ counter 1) max))))
  (f2-iter 0 1 2 3 n))
