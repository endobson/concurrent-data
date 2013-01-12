#lang racket/base

(require racket/contract)
(provide
  (contract-out
    (mvar? (any/c . -> . any))
    (make-mvar (() (any/c) . ->* . any))
    (mvar-put (mvar? any/c . -> . any))
    (mvar-get (mvar? . -> . any))
    (mvar-try-put (mvar? any/c . -> . any))
    (mvar-try-get ((mvar?) ((-> any)) . ->* . any))
    (mvar-put-evt (mvar? any/c . -> . any))
    (mvar-get-evt (mvar? . -> . any))))

(module* unsafe #f
  (provide
    mvar?
    make-mvar
    mvar-put
    mvar-get
    mvar-try-put
    mvar-try-get
    mvar-put-evt
    mvar-get-evt))



(struct mvar (read-chan thread write-chan))

(define make-mvar
  (let ()
    (define (initialize)
      (define read-chan (make-channel))
      (define write-chan (make-channel))
      (define (read-state)
        (write-state (sync write-chan)))
      (define (write-state v) 
        (channel-put read-chan v)
        (read-state))
      (values read-state write-state read-chan write-chan))
    (case-lambda
      (()
       (define-values (read-state write-state read-chan write-chan) (initialize))
       (define t (thread/suspend-to-kill read-state))
       (mvar read-chan t write-chan))
      ((v)
       (define-values (read-state write-state read-chan write-chan) (initialize))
       (define t (thread/suspend-to-kill (λ () (write-state v))))
       (mvar read-chan t write-chan)))))


(define (mvar-put-evt mv v)
  (thread-resume (mvar-thread mv) (current-thread))
  (channel-put-evt (mvar-write-chan mv) v))

(define (mvar-put mv v)
  (sync (mvar-put-evt mv v)))

(define (mvar-try-put mv v)
  (sync/timeout #f 
     (handle-evt (mvar-put-evt mv v) (λ (_) #t))))

(define (mvar-get-evt mv)
  (thread-resume (mvar-thread mv) (current-thread))
  (wrap-evt (mvar-read-chan mv) values))

(define (mvar-get mv)
  (sync (mvar-get-evt mv)))

(define (mvar-try-get mv (fail-fn (lambda () #f)))
  (sync/timeout fail-fn (mvar-get-evt mv)))
