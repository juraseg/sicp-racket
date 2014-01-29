#lang racket
; auxilary functions
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (square x) (* x x))
(define (cube x) (* (square x) x))

; predefined constants
(define tolerance 0.00001)
(define dx 0.00001)

; main functions
(define (iterative-improve improve good-enough?)
  (lambda (guess)
    (define try
      (lambda (x)
        (if (good-enough? x)
            x
            (try (improve x)))))
    (try guess)))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (x)
      (f x))
    (lambda (x)
      (< (abs (- x (f x))) tolerance)))
   first-guess))
(define (average a b) (/ (+ a b) 2))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

(define (average-damp f) (lambda (x) (average x (f x))))

(define (double f)
  (lambda (y)
    (display "double")
    (newline)
    (f (f y))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (< n 2)
        (f x)
        ((compose f (repeated f (- n 1))) x))))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smooth-repeated f n)
  ((repeated smooth n) f))

; created functions
(define (sqrt x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1.0))

(define (cont-frac2 n d k)
  (define (temp n d index)
    (if (> index k)
        0
        (/ (n index) (+ (d index) (temp n d (inc index))))))
  (temp n d 0))

(define (cont-frac n d k)
  (define (iter n d result index)
    (if (= index 0)
        result
        (iter n
              d
              (/ (n index) (+ (d index) result))
              (dec index))))
  (iter n d 0 k))

(define (find-golden-ratio repeats)
  (/ 1
     (cont-frac (lambda (y) 1.0)
                (lambda (y) 1.0)
                repeats)))

(define (find-e repeats)
  (+ 2
     (cont-frac (lambda (y) 1.0)
                (lambda (y)
                  (if (= (remainder (- y 2) 3) 0)
                      (* 2 (inc (/ (- y 2) 3)))
                      1.0))
                repeats)))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (i) 
               (+ 1 (* 2 (- i 1))))
             k))

(define (newtons-method g guess)
  (define (deriv g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x)
              ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))

(define (sqrt2 x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))

(define (sqrt3 x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))
(define (sqrt4 x)
  (newtons-method
   (lambda (y) (- (square y) x))
   1.0))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (expt-root x n)
  (fixed-point-of-transform
   (lambda (y) (/ x (expt y (- n 1))))
   (repeated average-damp (- n 2))
   1.0))
