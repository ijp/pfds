#!r6rs
(library (pfds tests hamts)
(export hamts)
(import (rnrs)
        (wak trc-testing)
        (pfds tests utils)
        (pfds hamts))

(define (make-string-hamt)
  (make-hamt string-hash string=?))

(define (compare-string-alist l1 l2)
  (lambda (l1 l2)
    (define (compare x y) (string<? (car x) (car y)))
    (equal? (list-sort compare l1)
            (list-sort compare l2))))

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

(define-test-case hamts hamt-conversion ()
  ;; alist->hamt / distinct keys
  (let* ((l '(("a" . 1) ("b" . 2) ("c" . 3)))
         (h (alist->hamt l string-hash string=?)))
    (test-equal (list 1 2 3)
                (map (lambda (x) (hamt-ref h x #f)) (list "a" "b" "c"))))
  ;; alist->hamt / overlapping keys (leftmost shadows)
  (let* ((l '(("a" . 1) ("b" . 2) ("c" . 3) ("a" . 4)))
         (h (alist->hamt l string-hash string=?)))
    (test-equal (list 1 2 3)
                (map (lambda (x) (hamt-ref h x #f)) (list "a" "b" "c"))))
  ;; hamt->alist / distinct keys means left inverse
  (let ((l '(("a" . 1) ("b" . 2) ("c" . 3))))
    (test-compare compare-string-alist l
                  (hamt->alist (alist->hamt l string-hash string=?)))))

)
