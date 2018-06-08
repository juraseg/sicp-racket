#lang racket

(define (make-queue)
  (let ((front-ptr null)
        (rear-ptr null))
    (define (set-front-ptr! item) (set front-ptr item))
    (define (set-rear-ptr! item) (set rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT is called with empty queue")
          (mcar front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (mcons item null)))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! is called with empty queue"))
            (else
             (set! front-ptr (mcdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))))
    dispatch))

(define (empty-queue? queue)
  (queue 'empty-queue?))

(define (front-queue queue)
  (queue 'front-queue))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item)
  queue)

(define (delete-queue! queue)
  (queue 'delete-queue!))

(define x (make-queue))
(empty-queue? x)

(insert-queue! x 1)
(empty-queue? x)
(front-queue x)

(insert-queue! x 2)
(empty-queue? x)
(front-queue x)

(delete-queue! x)
(empty-queue? x)
(front-queue x)

(delete-queue! x)
(empty-queue? x)