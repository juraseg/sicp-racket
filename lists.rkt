#lang racket
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(define (reverse items)
  (define (iter x tail)
    (if (null? x)
        tail
        (iter (cdr x) (cons (car x) tail))))
  (iter items null))

(define (deep-reverse items)
  (define (iter x tail)
    (if (null? x)
        tail
        (if (pair? (car x))
            (iter (cdr x) (cons (deep-reverse (car x)) tail))
            (iter (cdr x) (cons (car x) tail)))))
  (iter items null))

(define (fringe items)
  