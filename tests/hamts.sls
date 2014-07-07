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

(define (bad-hash x) 0)

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

(define-test-case hamts hamt-contains ()
  (let ((h (hamt-set (make-string-hamt) "foo" 1)))
    (test-eqv #t (hamt-contains? h "foo")))
  (let ((h (hamt-set (make-string-hamt) "foo" 1)))
    (test-eqv #f (hamt-contains? h "bar"))))

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

(define-test-case hamts hamt-folding ()
  ;; count size
  (let ((h (alist->hamt '(("a" . 1) ("b" . 2) ("c" . 3)) string-hash string=?))
        (increment (lambda (k v acc) (+ 1 acc))))
    (test-equal 3 (hamt-fold increment 0 h)))
  ;; copy hamt
  (let* ((l '(("a" . 1) ("b" . 2) ("c" . 3)))
         (h (alist->hamt l string-hash string=?))
         (add (lambda (k v acc) (hamt-set acc k v))))
    (test-compare compare-string-alist l
                  (hamt->alist (hamt-fold add (make-string-hamt) h)))))

(define-test-case hamts hamt-removal ()
  ;; removed key exists
  (let* ((l  '(("a" . 1) ("b" . 2) ("c" . 3)))
         (h (alist->hamt l string-hash string=?)))
    (test-compare compare-string-alist '(("b" . 2) ("c" . 3)) (hamt-delete h "a")))
  ;; removed key does not exist
  (let* ((l  '(("a" . 1) ("b" . 2) ("c" . 3)))
         (h (alist->hamt l string-hash string=?)))
    (test-compare compare-string-alist l (hamt-delete h "d"))))

(define-test-case hamts hamt-collisions ()
  ;; a bad hash function does not cause problems
  (let* ((l  '(("a" . 1) ("b" . 2) ("c" . 3)))
         (h (alist->hamt l bad-hash string=?)))
    (test-compare compare-string-alist l (hamt->alist h)))
  ;; stress test, since bigger amounts data usually finds bugs
  (let ((insert (lambda (val hamt) (hamt-set hamt val val)))
        (hash   (lambda (n) (exact (floor (/ n 2))))))
    (test-no-exn (foldl insert (make-hamt hash =) (iota 100)))))


(define-test-case hamts hamt-mapping ()
  (let* ((l '(("a" . 97) ("b" . 98) ("c" . 99)))
         (h (alist->hamt l string-hash string=?)))
    (test-compare compare-string-alist l
                  (hamt->alist (hamt-map (lambda (x) x) h))))
  (let* ((l '(("a" . 97) ("b" . 98) ("c" . 99)))
         (h (alist->hamt l string-hash string=?))
         (stringify (lambda (n) (string (integer->char n)))))
    (test-compare compare-string-alist
                  '(("a". "a") ("b" . "b") ("c" . "c"))
                  (hamt->alist (hamt-map stringify h)))))

)
