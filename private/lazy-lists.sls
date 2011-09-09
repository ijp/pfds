#!r6rs
(library (pfds private lazy-lists)
(export cons*
        tail
        head
        empty?
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
  (force (cdr pair)))

)
