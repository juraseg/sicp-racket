#lang racket
(define (accumulate2 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (product term (next a) next b))))
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

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

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (integral2 f a b n)
  (define h (/ (- b a) n))
  (define (next a) (+ a h))
  (define (term a index)
    (define item (f a))
    (cond ((= index 0) item)
          ((= index n) item)
          ((even? index) (* 2 item))
          (else (* 4 item))))
  (* (/ h 3.0) (indexed-sum term a next b)))