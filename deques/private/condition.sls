#!r6rs
(library (pfds deques private condition)
(export &deque-empty
        make-deque-empty-condition
        deque-empty-condition?)
(import (rnrs conditions))

(define-condition-type &deque-empty
  &assertion
  make-deque-empty-condition
  deque-empty-condition?)

)
