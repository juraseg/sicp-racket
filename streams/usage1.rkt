#lang racket

(require "operations.rkt")
(provide add-streams mul-streams scale-stream partial-sums)

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
    sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                           seq))

; infinite streams
(define (divisible? x y) (= (remainder x y) 0))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;(define integers (integers-starting-from 1))

;(define no-sevens
;  (stream-filter (lambda (x) (not (divisible? x 7)))
;                 integers))

;(define (fibgen a b)
;  (cons-stream a (fibgen b (+ a b))))

;(define fibs (fibgen 0 1))


; sieve of Eratosthenes
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

;(define primes (sieve (integers-starting-from 2)))

; implicit streams
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map-multi + s1 s2))

(define (mul-streams s1 s2)
  (stream-map-multi * s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (* (stream-car ps) (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

; ex 3.54
(define factorials (cons-stream 1 (mul-streams integers factorials)))

; ex 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream)
                            (stream-cdr stream))))

; ex 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))