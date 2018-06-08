#lang racket

(require racket/stream)

(provide stream-car stream-cdr stream-null? the-empty-stream cons-stream)

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-null? stream) (null? stream))

(define the-empty-stream '())

(define-syntax-rule (cons-stream a b)
  (cons a (delay b)))
