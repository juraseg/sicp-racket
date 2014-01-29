#lang racket
(define (product2 term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (inc n) (+ n 1))
(define (identity n) n)

(define (factorial n)
  (product identity 1 inc n))

(define (pi-prod n)
  (define (pi-term a)
    (if (even? a)
        (/ (+ a 2) (+ a 1))
        (/ (+ a 1) (+ a 2))))
  (product pi-term 1.0 inc n))