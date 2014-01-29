(define (square n) 
  (* n n))

(define (expt_r b n)
  (if (= n 0) 1 (* b (expt_r b (- n 1)))))

(define (expt_i b n)
  (define (expt-iter b counter product)
    (if (= counter 0) 
        product
        (expt-iter b (- counter 1) (* b product))))
  (expt-iter b n 1))

(define (fast-expt_i b n)
  (define (expt-iter b counter product)
    (cond ((= counter 0) product)
          ((even? counter) (expt-iter (square b)
                                      (/ counter 2)
                                      product))
          (else (expt-iter b
                           (- counter 1)
                           (* b product)))))
  (expt-iter b n 1))

(define (fast-expt_r b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt_r b (/ n 2))))
        (else
         (* b (fast-expt_r b (- n 1))))))