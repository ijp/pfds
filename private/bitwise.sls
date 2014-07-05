#!r6rs
(library (pfds private bitwise)
(export bitwise-bit-set
        bitwise-bit-unset
        )
(import (rnrs base)
        (rnrs arithmetic bitwise))

(define (bitwise-bit-set bits i)
  (bitwise-ior bits (bitwise-arithmetic-shift-left 1 i)))

(define (bitwise-bit-unset bits i)
  (bitwise-and bits (bitwise-not (bitwise-arithmetic-shift-left 1 i))))

)
