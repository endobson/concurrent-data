#lang racket/base

(require
  racket/list
  racket/contract)


(provide
  (contract-out
    (queue? (any/c . -> . boolean?))
    (make-queue (-> queue?))
    (enqueue! (queue? any/c . -> . void?))
    (dequeue! ((queue?) ((-> any)) . ->* .  any))
    (queue-length (queue? . -> . exact-nonnegative-integer?))
    (queue-empty? (queue? . -> . boolean?))))

(struct queue (nodes head tail)
        #:property prop:sequence
        (λ (q) (queue->sequence q)))
(struct node ((value #:mutable) next))

(define (make-queue)
  (define new-node (node #f (box #f)))
  (queue (box empty) (box new-node) (box new-node)))


(define (enqueue! q v)
  (define new-node (node v (box #f)))
  (let loop ()
    (let* ((tail-box (queue-tail q))
           (tail (unbox tail-box))
           (next-box (node-next tail))
           (next (unbox next-box)))
      (if next 
          (begin (box-cas! tail-box tail next) (loop))
          (unless (box-cas! next-box next new-node)
            (loop))))))

;;TODO figure out how to make this space safe and fast at the same time
(define (dequeue! q (empty-fn (lambda () #f)))
  (let loop ()
    (let* ((head-box (queue-head q))
           (head (unbox head-box))
           (tail-box (queue-tail q))
           (tail (unbox tail-box))
           (next (unbox (node-next head))))
      (if (eq? head tail)
          (if next
              (begin (box-cas! tail-box tail next) (loop))
              (empty-fn))
          (if (box-cas! head-box head next)
              (begin0
                (node-value next)
                (set-node-value! next #f))
              (loop))))))

(define (fast-dequeue! q (empty-fn (lambda () #f)))
  (let loop ()
    (let* ((head-box (queue-head q))
           (head (unbox head-box))
           (tail-box (queue-tail q))
           (tail (unbox tail-box))
           (next (unbox (node-next head))))
      (if (eq? head tail)
          (if next
              (begin (box-cas! tail-box tail next) (loop))
              (empty-fn))
          (if (box-cas! head-box head next)
              (node-value next)
              (loop))))))


(define (queue-length queue)
  (let loop ((count 0) (node (unbox (queue-head queue))))
    (let ((next-node (unbox (node-next node))))
      (if next-node
          (loop (add1 count) next-node)
          count))))

(define (queue-empty? queue)
  (not (unbox (node-next (unbox (queue-head queue))))))

(define (queue->sequence q)
  (define stop (gensym 'stop))
  (in-producer dequeue! stop q (λ () stop)))



(module+ comparison
  (require (prefix-in r: data/queue))

  (define N 4000000)
  (let ((q (r:make-queue)))
    (time
      (for ((i (in-range N)))
        (r:enqueue! q i)))
    (time
      (for ((i (in-range N)))
        (r:dequeue! q))))

  (collect-garbage) 
  (let ((q (make-queue)))
    (time
      (for ((i (in-range N)))
        (enqueue! q #t)))
    (time 
      (for ((i (in-range N)))
        (let loop ((v #f))
          (unless v
            (fast-dequeue! q))))))

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


