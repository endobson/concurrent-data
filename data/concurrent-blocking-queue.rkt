#lang racket/base

(require
  (prefix-in nb: (submod "concurrent-nonblocking-queue.rkt" unsafe))
  racket/future
  racket/contract)

;Slow full contracts
#;
(provide
  (contract-out
    (queue? (any/c . -> . boolean?))
    (make-queue (-> queue?))
    (enqueue! (queue? any/c . -> . void?))
    (try-dequeue! ((queue?) ((-> any)) . ->* .  any))
    (dequeue! (queue? . -> .  any))
    (queue-length (queue? . -> . exact-nonnegative-integer?))
    (queue-empty? (queue? . -> . boolean?))))

;Fast contracts
(provide
  (contract-out
    (queue? (any/c . -> . any))
    (make-queue (-> any))
    (enqueue! (queue? any/c . -> . any))
    (try-dequeue! ((queue?) ((-> any)) . ->* .  any))
    (dequeue! (queue? . -> .  any))
    (queue-length (queue? . -> . any))
    (queue-empty? (queue? . -> . any))))

(module* unsafe #f
  (provide
    queue?
    make-queue
    enqueue!
    try-dequeue!
    dequeue!
    queue-length
    queue-empty?))

(struct queue (sema inner))

(define (make-queue)
  (queue (make-fsemaphore 0) (nb:make-queue)))

(define (enqueue! q v)
  (nb:enqueue! (queue-inner q) v)
  (fsemaphore-post (queue-sema q)))

(define (inner-dequeue! q)
  (nb:try-dequeue! (queue-inner q)
    (lambda () (error 'conncurrent-blocking-queue "Corrupt queue."))))

(define (dequeue! q)
  (fsemaphore-wait (queue-sema q))
  (inner-dequeue! q))

(define (try-dequeue! q (empty-fn (lambda () #f)))
  (if (fsemaphore-try-wait? (queue-sema q))
      (inner-dequeue! q)
      (empty-fn)))

(define (queue-empty? q)
  (nb:queue-empty? (queue-inner q)))

(define (queue-length q)
  (nb:queue-length (queue-inner q)))


(module+ comparison
  (require (prefix-in r: data/queue))

  (define N 6000000)
  (collect-garbage) 
  (let ((q (r:make-queue)))
    (time
      (for ((i (in-range N)))
        (r:enqueue! q i)))
    (time
      (for ((i (in-range N)))
        (r:dequeue! q))))

  (collect-garbage) 
  (collect-garbage)
  (let ((q (make-queue)))
    (time
      (for ((i (in-range N)))
        (enqueue! q #t)))
    (time 
      (for ((i (in-range N)))
        (let loop ((v #f))
          (unless v
            (dequeue! q)))))))

