#lang racket/base

(require racket/contract)
(provide
  (contract-out
    (ivar? (any/c . -> . any))
    (make-ivar (-> any))
    (ivar-set (ivar? any/c . -> . any))
    (ivar-set? (ivar? . -> . any))
    (ivar-try-get ((ivar?) ((-> any)) . ->* . any))))

(module* unsafe #f
  (provide
    ivar?
    make-ivar
    ivar-set
    ivar-set?
    ivar-try-get))


(define unset (gensym 'unset))
(define (unset? x) (eq? x unset))

(struct ivar (box))

(define (make-ivar (v unset))
  (ivar (box v)))

(define (ivar-set iv v)
  (box-cas! (ivar-box iv) unset v))

(define (ivar-set? iv)
  (not (unset? (unbox (ivar-box iv)))))

(define (ivar-try-get iv (unset-fn (lambda () #f)))
  (define v (unbox (ivar-box iv)))
  (if (unset? v)
      (unset-fn)
      v))
