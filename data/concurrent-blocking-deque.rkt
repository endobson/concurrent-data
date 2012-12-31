#lang racket/base

(require
  racket/match
  racket/contract
  racket/function
  racket/future)

(provide
  (contract-out
    (deque? (any/c . -> . boolean?))
    (make-deque (exact-positive-integer? . -> . deque?))
    (enqueue! (deque? any/c . -> . void?))
    (enqueue-front! (deque? any/c . -> . void?))
    (dequeue! (deque? . -> . any/c))
    (dequeue-back! (deque? . -> . any/c))

    (try-enqueue! ((deque? any/c) ((-> any)) . ->* . any))
    (try-enqueue-front! ((deque? any/c) ((-> any)) . ->* . any))
    (try-dequeue! ((deque?) ((-> any)) . ->* . any/c))
    (try-dequeue-back! ((deque?) ((-> any)) . ->* . any/c))

    (deque-length (deque? . -> . exact-nonnegative-integer?))))

(struct deque (vector (start-index #:mutable) (size #:mutable) lock read-counter write-counter))

(define (make-deque max-size)
  (deque (make-vector max-size #f) 0 0 (make-fsemaphore 1) (make-fsemaphore 0) (make-fsemaphore max-size)))


;; Use syntax rules because procedure name is correct, and inlining still works
(define-syntax-rule (enqueuer enqueue)
  (lambda (q v)
   (fsemaphore-wait (deque-write-counter q))
   (enqueue q v)))

(define-syntax-rule (try-enqueuer enqueue)
  (lambda (q v (full-fn (λ () #f)))
    (if (fsemaphore-try-wait? (deque-write-counter q))
        (enqueue q v)
        (full-fn))))

(define-syntax-rule (dequeuer dequeue)
  (lambda (q)
    (fsemaphore-wait (deque-read-counter q))
    (dequeue q)))

(define-syntax-rule (try-dequeuer dequeue)
  (lambda (q (empty-fn (λ () #f)))
    (if (fsemaphore-try-wait? (deque-read-counter q))
        (dequeue q)
        (empty-fn))))


(define (inner-enqueue! q v)
  (define lock (deque-lock q))
  (fsemaphore-wait lock)
  (match-define (deque vec start-index size _ read-counter _) q)
  (define index (modulo (+ start-index size) (vector-length vec)))
  (vector-set! vec index v)
  (set-deque-size! q (add1 size))
  (fsemaphore-post lock)
  (fsemaphore-post read-counter))

(define (inner-enqueue-front! q v)
  (define lock (deque-lock q))
  (fsemaphore-wait lock)
  (match-define (deque vec start-index size _ read-counter _) q)
  (define index (modulo (sub1 start-index) (vector-length vec)))
  (vector-set! vec index v)
  (set-deque-start-index! q index)
  (set-deque-size! q (add1 size))
  (fsemaphore-post lock)
  (fsemaphore-post read-counter))

(define (inner-dequeue! q)
  (define lock (deque-lock q))
  (fsemaphore-wait lock)
  (match-define (deque vec start-index size _ _ write-counter) q)
  (define index start-index)
  (define value (vector-ref vec index))
  (vector-set! vec index #f)
  (set-deque-start-index! q (modulo (add1 index) (vector-length vec)))
  (set-deque-size! q (sub1 size))
  (fsemaphore-post lock)
  (fsemaphore-post write-counter)
  value)

(define (inner-dequeue-back! q)
  (define lock (deque-lock q))
  (fsemaphore-wait lock)
  (match-define (deque vec start-index size _ _ write-counter) q)
  (define index (modulo (+ start-index size) (vector-length vec)))
  (define value (vector-ref vec index))
  (vector-set! vec index #f)
  (set-deque-size! q (sub1 size))
  (fsemaphore-post lock)
  (fsemaphore-post write-counter)
  value)

(define enqueue! (enqueuer inner-enqueue!))
(define try-enqueue! (try-enqueuer inner-enqueue!))
(define enqueue-front! (enqueuer inner-enqueue-front!))
(define try-enqueue-front! (try-enqueuer inner-enqueue-front!))
(define dequeue! (dequeuer inner-dequeue!))
(define try-dequeue! (try-dequeuer inner-dequeue!))
(define dequeue-back! (dequeuer inner-dequeue-back!))
(define try-dequeue-back! (try-dequeuer inner-dequeue-back!))


(define (deque-length q)
  (define lock (deque-lock q))
  (fsemaphore-wait lock)
  (begin0
    (deque-size q)
    (fsemaphore-post lock)))




(module+ comparison
  (require (prefix-in r: data/queue))
  ;(require (prefix-in c: "concurrent-blocking-deque.rkt"))
  (define N 2000)

  (define (slow x)
    (if (zero? x) 0 (add1 (slow (sub1 x)))))
  (time (slow 50000))

  (collect-garbage)
  (let ((q (r:make-queue)))
    (time
      (for ((i (in-range N)))
        (r:enqueue! q (slow (* 100 i)))))
    (time
      (for ((i (in-range N)))
        (slow (r:dequeue! q)))))

  (collect-garbage)
  (let ((q (make-deque N)))
    (time
      (for ((i (in-range N)))
        (enqueue! q (slow (* 100 i)))))
    (time 
      (for ((i (in-range N)))
        (slow (dequeue! q)))))



  (collect-garbage)
  ;(require future-visualizer)
  ;(visualize-futures
  (time
    (let ((q (make-deque 10000)))
      (define f1
        (future
          (thunk
            (for ((i (in-range N)))
              (enqueue! q (slow (* 100 i)))))))
      (define f2
        (future
          (thunk
            (for ((i (in-range N)))
              (slow (dequeue! q))))))
      (touch f1)
      (touch f2)
      (void)))


  (collect-garbage)
  (time
    (let ((q (make-deque 1000)))
      (define f1
        (future
          (thunk
            (for ((i (in-range (/ N 2))))
              (enqueue! q (slow (* 100 (* i 2))))))))
      (define f2
        (future
          (thunk
            (for ((i (in-range (/ N 2))))
              (enqueue! q (slow (* 100 (add1 (* i 2)))))))))
      (define f3
        (future
          (thunk
            (for ((i (in-range N)))
              (slow (dequeue! q))))))
      (touch f1)
      (touch f2)
      (touch f3)
      (void))))


