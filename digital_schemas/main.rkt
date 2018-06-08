#lang racket

(require "the-agenda.rkt")
(require "schemas.rkt")

(define a1 (make-wire))
(define a2 (make-wire))
(define a3 (make-wire))
(define a4 (make-wire))

(define b1 (make-wire))
(define b2 (make-wire))
(define b3 (make-wire))
(define b4 (make-wire))

(define s1 (make-wire))
(define s2 (make-wire))
(define s3 (make-wire))
(define s4 (make-wire))

(define c (make-wire))

; riple-carry added
; A - first number, which digits are passed to wires a1a2a3a4
; B - first number, which digits are passed to wires b1b2b3b4
; S - sum, with digits s1, s2, s3, s4

(define (ripple-carry-adder A B S c)
  (let ((c-in (make-wire)))
    (full-adder (car A) (car B) c-in (car S) c)
    (cond ((pair? (cdr S))
           (ripple-carry-adder (cdr A) (cdr B) (cdr S) c-in))
          (else
           (set-signal! c-in 0))))
  'ok)

(ripple-carry-adder (list a1 a2 a3 a4) (list b1 b2 b3 b4) (list s1 s2 s3 s4) c)

; 1001 + 0101 = 1110
(set-signal! a1 1)
(set-signal! a2 0)
(set-signal! a3 0)
(set-signal! a4 1)

(set-signal! b1 0)
(set-signal! b2 1)
(set-signal! b3 1)
(set-signal! b4 0)

(display (list (get-signal s1) (get-signal s2) (get-signal s3) (get-signal s4)))
(newline)
(display (get-signal c))
(newline)

(propagate)

(display (list (get-signal s1) (get-signal s2) (get-signal s3) (get-signal s4)))
(newline)
(display (get-signal c))