#lang racket
(define (sum2 term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n) (+ n 1))
(define (identity n) n)
(define (cube n) (* n n n))

(define (sum-cubes a b)
  (sum cube a inc b))

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
  (define (term k)
    (define item (f (+ a (* h k))))
    (cond ((= k 0) item)
          ((= k n) item)
          ((even? k) (* 2 item))
          (else (* 4 item))))
  (* (/ h 3.0) (sum term 0 inc n)))