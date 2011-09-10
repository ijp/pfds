#!r6rs
(library (pfds private lazy-lists)
(export cons*
        tail
        head
        empty?
        take
        drop
        rev
        append*
        )
(import (except (rnrs) cons*)
        (rnrs r5rs)
        )

;;; Lazy Lists
;;
;; If you want real lazy lists, use SRFI 41, but Okazaki uses 'odd'
;; lists, so I wrote a quick implementation.
;;
(define-syntax cons*
  (syntax-rules ()
    ((cons* a b)
     (cons a (delay b)))))

(define head car)

(define empty? null?)

(define (tail pair)
  (if (empty? pair)
      pair
      (force (cdr pair))))

(define (take n l)
  (if (zero? n)
      '()
      (cons* (head l)
             (take (- n 1) (tail l)))))

(define (drop n l)
  (if (zero? n)
      l
      (drop (- n 1) (tail l))))

(define (append* x y)
  (if (empty? x)
      y
      (cons* (head x)
             (append* (tail x) y))))

(define (rev l)
  (let loop ((l l) (a '()))
    (if (empty? l)
        a
        (loop (tail l) (cons* (head l) a)))))

)
