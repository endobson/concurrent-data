#lang racket/base

(require
  racket/list
  racket/contract)

;Slow full contracts
#;
(provide
  (contract-out
    (queue? (any/c . -> . boolean?))
    (make-queue (-> queue?))
    (enqueue! (queue? any/c . -> . void?))
    (dequeue! ((queue?) ((-> any)) . ->* .  any))
    (queue-length (queue? . -> . exact-nonnegative-integer?))
    (queue-empty? (queue? . -> . boolean?))))

;Fast contracts
(provide
  (contract-out
    (queue? (any/c . -> . any))
    (make-queue (-> any))
    (enqueue! (queue? any/c . -> . any))
    (dequeue! ((queue?) ((-> any)) . ->* .  any))
    (queue-length (queue? . -> . any))
    (queue-empty? (queue? . -> . any))))

(module* unsafe #f
  (provide
    queue?
    make-queue
    enqueue!
    dequeue!
    queue-length
    queue-empty?))

(struct queue (head tail)
        #:property prop:sequence
        (λ (q) (queue->sequence q)))
(struct node ((value #:mutable) next-box))

(define (make-queue)
  (define the-box (box #f))
  (queue (box the-box) (box the-box)))


(define (enqueue! q v)
  (define new-box (box #f))
  (define new-node (node v new-box))
  (let loop ()
    (let* ((tail-box (queue-tail q))
           (next-box (unbox tail-box))
           (next (unbox next-box)))
      (if next 
          (begin (box-cas! tail-box next-box (node-next-box next)) (loop))
          (if (box-cas! next-box next new-node)
              (void (box-cas! tail-box next-box new-box))
              (loop))))))

(define (dequeue! q (empty-fn (lambda () #f)))
  (let loop ()
    (let* ((head-box (queue-head q))
           (head-node-box (unbox head-box))
           (tail-box (queue-tail q))
           (tail-node-box (unbox tail-box))
           (next (unbox head-node-box)))
      (if (eq? head-node-box tail-node-box)
          (if next
              (begin (box-cas! tail-box tail-node-box (node-next-box next)) (loop))
              (empty-fn))
          (if (box-cas! head-box head-node-box (node-next-box next))
              (node-value next)
              (loop))))))

(define (queue-length queue)
  (let loop ((count 0) (node (unbox (queue-head queue))))
    (let ((next-node (unbox (node-next-box node))))
      (if next-node
          (loop (add1 count) next-node)
          count))))

(define (queue-empty? queue)
  (not (unbox (node-next-box (unbox (queue-head queue))))))

(define (queue->sequence q)
  (define stop (gensym 'stop))
  (in-producer dequeue! stop q (λ () stop)))


(module+ comparison
  (require (prefix-in r: data/queue))

  (define N 6000000)
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


