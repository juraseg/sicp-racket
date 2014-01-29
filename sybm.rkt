#lang racket

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (equal? list1 list2)
  (cond ((and (pair? list1) (pair? list2))
         (and (equal? (car list1) (car list2))
              (equal? (cdr list1) (cdr list2))))
        ((pair? list1) false)
        ((pair? list2) false)
        (else (eq? list1 list2))))