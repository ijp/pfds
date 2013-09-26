#!r6rs
(library (pfds queues private condition)
(export &queue-empty
        make-queue-empty-condition
        queue-empty-condition?)
(import (rnrs conditions))

(define-condition-type &queue-empty
  &assertion
  make-queue-empty-condition
  queue-empty-condition?)

)
