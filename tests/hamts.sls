#!r6rs
(library (pfds tests hamts)
(export hamts)
(import (rnrs)
        (wak trc-testing)
        (pfds tests utils)
        (pfds hamts))

(define (make-string-hamt)
  (make-hamt string-hash string=?))

(define-test-suite (hamts pfds)
  "Tests for the Hash Array Mapped Trie implementation")

(define-test-case hamts empty-hamt ()
  (test-predicate hamt? (make-string-hamt)))

(define-test-case hamts hamt-ref/set ()
  ;; Referencing non-existent key
  (test-equal #f (hamt-ref (make-string-hamt) "foo" #f))
  ;; Referencing a non-existent key (exception)
  (test-exn assertion-violation? (hamt-ref (make-string-hamt) "bar"))
  ;; Referencing newly-added key
  (test-equal "bar" (hamt-ref (hamt-set (make-string-hamt) "foo" "bar") "foo" #f))
  ;; shadowing an existing key
  (test-equal "baz" (hamt-ref (hamt-set (hamt-set (make-string-hamt) "foo" "bar") "foo" "baz") "foo" #f)))

)
