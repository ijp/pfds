(library (test sequences)
  (export sequences)
  (import (rnrs (6))
          (chez-test suite)
          (chez-test assertions)
          (test utils)
          (pfds sequences))
  
  (define-test-suite sequences
    "Tests for the sequences implementation")
  ;; Note: at the moment, sequences are a trivial instantiation of
  ;; fingertrees, and so are pretty much covered by the fingertrees
  ;; tests.
  
  (define-test-case sequences sequences-bugs
    (let ((s (sequence 'zero 'one 'two)))
      (test-case sequences-bugs ()
        (assert-eqv 'zero (sequence-ref s 0))
        (assert-eqv 'two (sequence-ref s 2))
        (assert-raises assertion-violation? (sequence-ref s -1))
        (assert-raises assertion-violation? (sequence-ref s 3)))))
  
)
