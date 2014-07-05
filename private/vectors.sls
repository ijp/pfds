#!r6rs
(library (pfds private vectors)
(export vector-set
        vector-insert
        vector-remove
        vector-copy
        vector-copy!
        )
(import (rnrs base)
        (rnrs control))

(define (vector-set h i x)
  (let ((v* (vector-copy h)))
    (vector-set! v* i x)
    v*))

(define (vector-remove v i)
  (define len (vector-length v))
  (assert (and (<= 0 i) (< i len)))
  (let ((newvec (make-vector (- len 1))))
    (vector-copy! v 0 newvec 0 i)
    (vector-copy! v (+ i 1) newvec i (- len i 1))
    newvec))

(define (vector-insert v i x)
  (define len (vector-length v))
  (assert (<= 0 i len))
  (let* ((newvec (make-vector (+ len 1))))
    (vector-set! newvec i x)
    (vector-copy! v 0 newvec 0 i)
    (vector-copy! v i newvec (+ 1 i) (- len i))
    newvec))

(define (vector-copy! source source-start target target-start k)
  ;; TODO: assertions
  ;; guile has vector-move functions, but rnrs does not :(
  (do ((i 0 (+ 1 i)))
      ((>= i k))
    (vector-set! target
                 (+ target-start i)
                 (vector-ref source (+ source-start i)))))

(define (vector-copy vector)
  (define len (vector-length vector))
  (define v* (make-vector len))
  (vector-copy! vector 0 v* 0 len)
  v*)

)
