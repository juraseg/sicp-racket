#lang racket

(provide make-serializer make-mutex)

; Concurency utility functions
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (mcons false '())))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (cond ((test-and-set! cell)
                    (the-mutex 'acquire))))
            ((eq? m 'release) (set-mcar! cell false))))
    the-mutex))

(define (test-and-set! cell)
  (if (mcar cell)
      true
      (begin (set-mcar! cell true)
             false)))