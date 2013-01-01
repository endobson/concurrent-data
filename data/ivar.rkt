#lang racket/base

(require (prefix-in nb: (submod "nonblocking-ivar.rkt" unsafe)))

(require racket/contract)
(provide
  (contract-out
    (ivar? (any/c . -> . any))
    (make-ivar (-> any))
    (ivar-set (ivar? any/c . -> . any))
    (ivar-set? (ivar? . -> . any))
    (ivar-try-get ((ivar?) ((-> any)) . ->* . any))
    (ivar-get (ivar? . -> . any))))

(module* unsafe #f
  (provide
    ivar?
    make-ivar
    ivar-set
    ivar-set?
    ivar-try-get
    ivar-get))

(struct ivar (inner sema evt)
        #:property prop:evt (struct-field-index evt))

(define make-ivar
  (case-lambda
    (()
     (define sema (make-semaphore))
     (define inner (nb:make-ivar))
     (ivar inner sema
           (wrap-evt (semaphore-peek-evt sema)
             (λ (_)
                (nb:ivar-try-get inner (λ () (error 'ivar "Corrupt ivar.")))))))
    ((v)
     (define inner (nb:make-ivar v))
     (ivar inner #f (wrap-evt always-evt (λ (_) v))))))


(define (ivar-set iv v)
  (and
    (nb:ivar-set (ivar-inner iv) v)
    (semaphore-post (ivar-sema iv))
    #t))

(define (ivar-set? iv)
  (nb:ivar-set? (ivar-inner iv)))

(define (ivar-try-get iv (unset-fn (lambda () #f)))
  (nb:ivar-try-get (ivar-inner iv) unset-fn))

(define (ivar-get iv)
  (sync iv))
