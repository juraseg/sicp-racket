#lang racket
;(require racket/include)
(require "prime.rkt")

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low
            (enumerate-interval (+ low 1)
                                high))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

; Ex 2.33
(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; Ex 2.34
(define 
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
   0
   coefficient-sequence))

; Ex 2.35
(define (count-leaves t)
  (length (enumerate-tree t)))

(define (count-leaves2 t)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x)
                                   (count-leaves2 x)
                                   1))
                   t)))
; Ex 2.36
(define (accumulate-n op initial sequence)
  (if (null? (car sequence))
      null
      (cons (accumulate op initial (map car sequence))
            (accumulate-n op initial (map cdr sequence)))))

; Ex 2.37 Matrix algebra
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
  (accumulate-n cons null m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

; Ex 2.39
(define (reverse-right sequence)
  (fold-right
   (lambda (x y) (append y (list x))) null sequence))
(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))


; Next section
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval
                 1
                 (- i 1))))
         (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutations
                       (remove x s))))
               s)))

; Ex 2.40
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list i j))
          (enumerate-interval
           1
           (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter
        prime-sum?
        (unique-pairs n))))

; Ex 2.41
(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map (lambda (k)
               (list i j k))
             (enumerate-interval
              1
              (- j 1))))
      (enumerate-interval
       1
       (- i 1))))
   (enumerate-interval 1 n)))

(define (find-pairs-sum s n)
  (filter
   (lambda (triple) (= (+ 
                        (car triple)
                        (cadr triple)
                        (caddr triple))
                       s))
   (unique-triples n)))

(provide (all-defined-out))