#lang racket/base

(require racket/contract)

(provide
  (contract-out
    (mvar? (any/c . -> . any))
    (make-mvar (() (any/c) . ->* . any))
    (mvar-set (mvar? any/c . -> . any))
    (mvar-set? (mvar? . -> . any))
    (mvar-try-get ((mvar?) ((-> any)) . ->* . any))))

(module* unsafe #f
  (provide
    mvar?
    make-mvar
    mvar-set
    mvar-set?
    mvar-try-get))

(define unset (gensym 'unset))
(define (unset? x) (eq? x unset))

(struct mvar (box))

(define (make-mvar (v unset))
  (mvar (box unset)))

(define (mvar-set m v)
  (box-cas! (mvar-box m) unset v))

(define (mvar-set? m)
  (not (unset? (unbox (mvar-box m)))))

(define (mvar-try-get m (empty-fn (lambda () #f)))
  (define box (mvar-box m))
  (define v (unbox box))
  (if (unset? v)
      (empty-fn)
      (if (box-cas! box v unset)
          v
          (empty-fn))))
