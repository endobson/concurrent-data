#lang racket/base

(require 
  racket/match
  racket/function
  racket/future)

(provide)

(struct queue (vector (start-index #:mutable) (size #:mutable) lock read-counter write-counter))

(define (make-queue max-size)
  (queue (make-vector max-size #f) 0 0 (make-fsemaphore 1) (make-fsemaphore 0) (make-fsemaphore max-size)))


(define (enqueue! q v)
  (match-define (queue vec _ _ lock read-counter write-counter) q)
  (fsemaphore-wait write-counter)
  (fsemaphore-wait lock)
  (match-define (queue _ start-index size _ _ _) q)
  (define index (modulo (+ start-index size) (vector-length vec)))
  (vector-set! vec index v)
  (set-queue-size! q (add1 size))
  (fsemaphore-post lock)
  (fsemaphore-post read-counter))

(define (enqueue-front! q v)
  (match-define (queue vec _ _ lock read-counter write-counter) q)
  (fsemaphore-wait write-counter)
  (fsemaphore-wait lock)
  (match-define (queue _ start-index size _ _ _) q)
  (define index (modulo (sub1 start-index) (vector-length vec)))
  (vector-set! vec index v)
  (set-queue-start-index! q index)
  (set-queue-size! q (add1 size))
  (fsemaphore-post lock)
  (fsemaphore-post read-counter))

(define (dequeue! q)
  (match-define (queue vec _ _ lock read-counter write-counter) q)
  (fsemaphore-wait read-counter)
  (fsemaphore-wait lock)
  (match-define (queue _ start-index size _ _ _) q)
  (define index start-index)
  (define value (vector-ref vec index))
  (vector-set! vec index #f)
  (set-queue-start-index! q (modulo (add1 index) (vector-length vec)))
  (set-queue-size! q (sub1 size))
  (fsemaphore-post lock)
  (fsemaphore-post write-counter))

(define (dequeue-back! q)
  (match-define (queue vec _ _ lock read-counter write-counter) q)
  (fsemaphore-wait read-counter)
  (fsemaphore-wait lock)
  (match-define (queue _ start-index size _ _ _) q)
  (define index (modulo (+ start-index size) (vector-length vec)))
  (define value (vector-ref vec index))
  (vector-set! vec index #f)
  (set-queue-size! q (sub1 size))
  (fsemaphore-post lock)
  (fsemaphore-post write-counter))

(define (queue-length q)
  (define lock (queue-lock q))
  (fsemaphore-wait lock)
  (begin0
    (queue-size q)
    (fsemaphore-post lock)))
  


;  (require future-visualizer)

  (define N 1000000)

  (require (prefix-in r: data/queue))
  (let ((q (r:make-queue)))
    (time
      (for ((i (in-range N)))
        (r:enqueue! q i)))
    (time
      (for ((i (in-range N)))
        (r:dequeue! q))))

(let ()
    (time 
  (define queue (make-queue N))
      (let ()
        (define f1
          (for ((i (in-range N)))
            (enqueue! queue i)))
        (define f2
          (for ((i (in-range N)))
            (dequeue! queue)))
        (void))))




