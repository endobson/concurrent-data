#lang racket/base

(require racket/contract)
(provide
  (contract-out
    (mvar? (any/c . -> . any))
    (make-mvar (() (any/c) . ->* . any))
    (mvar-put (mvar? any/c . -> . any))
    (mvar-get (mvar? . -> . any))
    (mvar-swap (mvar? any/c . -> . any))
    (mvar-try-put ((mvar? any/c) ((-> any)) . ->* . any))
    (mvar-try-get ((mvar?) ((-> any)) . ->* . any))
    (mvar-try-swap ((mvar? any/c) ((-> any)) . ->* . any))
    (mvar-put-evt (mvar? any/c . -> . any))
    (mvar-get-evt (mvar? . -> . any))
    (mvar-swap-evt (mvar? any/c . -> . any))

    (mvar-peek ((mvar?) ((-> any)) . -> . any))
    (mvar-cas (mvar? any/c any/c . -> . any))
    (mvar-full? (mvar? . -> . any))
    (mvar-empty? (mvar? . -> . any))))

(module* unsafe #f
  (provide
    mvar?
    make-mvar
    mvar-put
    mvar-get
    mvar-swap
    mvar-try-put
    mvar-try-get
    mvar-try-swap
    mvar-put-evt
    mvar-get-evt
    mvar-swap-evt

    mvar-peek
    mvar-cas
    mvar-full?
    mvar-empty?))


(define unset (gensym 'unset))
(define (unset? x) (eq? x unset))

(struct mvar (read-sema box write-sema)
        #:property prop:evt
        (λ (mv) (mvar-get-evt mv)))

(define make-mvar
  (case-lambda
    (()
     (define read-sema (make-semaphore 0))
     (define write-sema (make-semaphore 1))
     (mvar read-sema (box unset) write-sema))
    ((v)
     (define read-sema (make-semaphore 1))
     (define write-sema (make-semaphore 0))
     (mvar read-sema (box v) write-sema))))

;; Internal Functions
(define (inner-put mv v)
  (set-box! (mvar-box mv) v)
  (semaphore-post (mvar-read-sema mv)))

(define (inner-get mv)
  (let ((value (mvar-box mv)))
    (set-box! (mvar-box mv) unset)
    (semaphore-post (mvar-write-sema mv))
    value))

(define (inner-swap mv new-v)
  (let loop ()
    (let* ((box (mvar-box mv))
           (old-v (unbox box)))
      (if (box-cas! box old-v new-v)
          (begin
            (semaphore-post (mvar-read-sema mv))
            old-v)
          (loop)))))

;; External functions
(define (mvar-put mv v)
  (parameterize-break #f
    (semaphore-wait (mvar-write-sema mv))
    (inner-put mv v)))

(define (mvar-try-put mv v (full-fn (lambda () #f)))
  (parameterize-break #f
    (if (semaphore-try-wait? (mvar-write-sema mv))
        (inner-put mv v)
        (full-fn))))

(define (mvar-get mv)
  (parameterize-break #f
    (semaphore-wait (mvar-read-sema mv))
    (inner-get mv)))

(define (mvar-try-get mv (empty-fn (lambda () #f)))
  (parameterize-break #f
    (if (semaphore-try-wait? (mvar-read-sema mv))
        (inner-get mv)
        (empty-fn))))

(define (mvar-swap mv v)
  (parameterize-break #f
    (semaphore-wait (mvar-read-sema mv))
    (inner-swap mv v)))

(define (mvar-try-swap mv v (empty-fn (lambda () #f)))
  (parameterize-break #f
    (if (semaphore-try-wait? (mvar-read-sema mv))
        (inner-swap mv v)
        (empty-fn))))

;; Events
(define (mvar-get-evt mv)
  (wrap-evt (mvar-read-sema mv)
    (λ (_) (inner-get mv))))

(define (mvar-put-evt mv v)
  (define e
    (wrap-evt (mvar-write-sema mv)
      (λ (_) (inner-put mv v) e)))
  e)

(define (mvar-swap-evt mv v)
  (wrap-evt (mvar-read-sema mv)
    (λ (_) (inner-swap mv v))))


;; Unsynchronized functions
(define (mvar-peek mv (empty-fn (lambda () #f)))
  (define v (mvar-box mv))
  (if (unset? v)
      (empty-fn)
      v))

(define (mvar-cas mv old new)
  (define box (mvar-box mv))
  (box-cas! box old new))

(define (mvar-full? iv)
  (not (mvar-empty? iv)))

(define (mvar-empty? iv)
  (unset? (mvar-box iv)))



