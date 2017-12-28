(library (test bbtrees)
  (export bbtrees)
  (import (rnrs (6))
          (chez-test suite)
          (chez-test assertions)
  
          (test utils)
          (pfds bbtrees))
  
  (define-test-suite bbtrees
    "Tests for the bounded balance tree imlementation")
  
  (define-test-case bbtrees empty-tree ()
    (assert-predicate bbtree? (make-bbtree <))
    (assert-eqv 0 (bbtree-size (make-bbtree <))))
  
  (define-test-case bbtrees bbtree-set ()
    (let* ([tree1 (bbtree-set (make-bbtree <) 1 'a)]
           [tree2 (bbtree-set tree1 2 'b)]
           [tree3 (bbtree-set tree2 1 'c )])
      (assert-eqv 1 (bbtree-size tree1))
      (assert-eqv 'a (bbtree-ref tree1 1))
      (assert-eqv 2 (bbtree-size tree2))
      (assert-eqv 'b (bbtree-ref tree2 2))
      (assert-eqv 2 (bbtree-size tree3))
      (assert-eqv 'c (bbtree-ref tree3 1))
      (assert-eqv #f (bbtree-ref tree1 #xdeadbeef #f))
      (assert-eqv 'not-in (bbtree-ref tree1 #xdeadbeef 'not-in))
      (assert-raises assertion-violation? (bbtree-ref tree3 20))))
  
  
  (define-test-case bbtrees bbtree-update ()
    (let ([bb (alist->bbtree '(("foo" . 10) ("bar" . 12)) string<?)]
          [add1 (lambda (x) (+ x 1))])
      (test-case bbtree-update ()
        (assert-eqv 11 (bbtree-ref (bbtree-update bb "foo" add1 0) "foo"))
        (assert-eqv 13 (bbtree-ref (bbtree-update bb "bar" add1 0) "bar"))
        (assert-eqv  1 (bbtree-ref (bbtree-update bb "baz" add1 0) "baz")))))
  
  (define-test-case bbtrees bbtree-delete ()
    (let* ([tree1 (bbtree-set (bbtree-set (bbtree-set (make-bbtree string<?) "a" 3)
                                          "b"
                                          8)
                              "c"
                              19)]
           [tree2 (bbtree-delete tree1 "b")]
           [tree3 (bbtree-delete tree2 "a")])
      (assert-eqv 3 (bbtree-size tree1))
      (assert-eqv #t (bbtree-contains? tree1 "b"))
      (assert-eqv #t (bbtree-contains? tree1 "a"))
      (assert-eqv 2 (bbtree-size tree2))
      (assert-eqv #f (bbtree-contains? tree2 "b"))
      (assert-eqv #t (bbtree-contains? tree2 "a"))
      (assert-eqv 1 (bbtree-size tree3))
      (assert-eqv #f (bbtree-contains? tree3 "a"))
      (assert-no-raise (bbtree-delete (bbtree-delete tree3 "a") "a"))))
  
  (define-test-case bbtrees bbtree-folds
    (let ((bb (alist->bbtree '(("foo" . 1) ("bar" . 12) ("baz" . 7)) string<?)))
      (test-case bbtree-folds ()
        ;; empty case
        (assert-eqv #t (bbtree-fold (lambda args #f) #t (make-bbtree >)))      
        (assert-eqv #t (bbtree-fold-right (lambda args #f) #t (make-bbtree >)))
        ;; associative operations
        (assert-eqv 20 (bbtree-fold (lambda (key value accum) (+ value accum)) 0 bb))
        (assert-eqv 20 (bbtree-fold-right (lambda (key value accum) (+ value accum)) 0 bb))
        ;; non-associative operations
        (assert-equal '("foo" "baz" "bar")
                    (bbtree-fold (lambda (key value accum) (cons key accum)) '() bb))
        (assert-equal '("bar" "baz" "foo")
                    (bbtree-fold-right (lambda (key value accum) (cons key accum)) '() bb)))))
  
  (define-test-case bbtrees bbtree-map
    (let ((empty (make-bbtree <))
          (bb (alist->bbtree '((#\a . foo) (#\b . bar) (#\c . baz) (#\d . quux))
                             char<?)))
      (test-case bbtree-map ()
        (assert-eqv 0 (bbtree-size (bbtree-map (lambda (x) 'foo) empty)))
        (assert-equal '((#\a foo . foo) (#\b bar . bar) (#\c baz . baz) (#\d quux . quux))
                    (bbtree->alist (bbtree-map (lambda (x) (cons x x)) bb)))
        (assert-equal '((#\a . "foo") (#\b . "bar") (#\c . "baz") (#\d . "quux"))
                    (bbtree->alist (bbtree-map symbol->string bb))))))
  
  (define-test-case bbtrees conversion ()
    (assert-eqv '() (bbtree->alist (make-bbtree <)))
    (assert-eqv 0 (bbtree-size (alist->bbtree '() <)))
    (assert-equal '(("bar" . 12) ("baz" . 7) ("foo" . 1))
                (bbtree->alist (alist->bbtree '(("foo" . 1) ("bar" . 12) ("baz" . 7)) string<?)))
    (let ((l '(48 2 89 23 7 11 78))
          (tree-sort  (lambda (< l)
                        (map car
                             (bbtree->alist
                              (alist->bbtree (map (lambda (x) (cons x 'dummy))
                                                  l)
                                             <))))))
      (assert-equal (list-sort < l) (tree-sort < l))))
  
  (define-test-case bbtrees bbtree-union
    (let ([empty (make-bbtree char<?)]
          [bbtree1 (alist->bbtree '((#\g . 103) (#\u . 117) (#\i . 105) (#\l . 108) (#\e . 101))
                                  char<?)]
          [bbtree2 (alist->bbtree '((#\l . 8) (#\i . 5) (#\s . 15) (#\p . 12))
                                  char<?)])
      (test-case bbtree-union ()
        (assert-eqv 0 (bbtree-size (bbtree-union empty empty)))
        (assert-eqv (bbtree-size bbtree1)
                  (bbtree-size (bbtree-union empty bbtree1)))
        (assert-eqv (bbtree-size bbtree1)
                  (bbtree-size (bbtree-union bbtree1 empty)))
        (assert-eqv (bbtree-size bbtree1)
                  (bbtree-size (bbtree-union bbtree1 bbtree1)))
        (assert-equal '(#\e #\g #\i #\l #\p #\s #\u)
                    (bbtree-keys (bbtree-union bbtree1 bbtree2)))
        ;; union favours values in first argument when key exists in both
        (let ((union (bbtree-union bbtree1 bbtree2)))
          (assert-eqv 105 (bbtree-ref union #\i))
          (assert-eqv 108 (bbtree-ref union #\l)))
        ;; check this holds on larger bbtrees
        (let* ([l (string->list "abcdefghijlmnopqrstuvwxyz")]
               [b1 (map (lambda (x) (cons x (char->integer x))) l)]
               [b2 (map (lambda (x) (cons x #f)) l)])
          (assert-equal b1
                      (bbtree->alist (bbtree-union (alist->bbtree b1 char<?)
                                                   (alist->bbtree b2 char<?))))))))
  
  (define-test-case bbtrees bbtree-intersection
    (let ([empty (make-bbtree char<?)]
          [bbtree1 (alist->bbtree '((#\g . 103) (#\u . 117) (#\i . 105) (#\l . 108) (#\e . 101))
                                  char<?)]
          [bbtree2 (alist->bbtree '((#\l . 8) (#\i . 5) (#\s . 15) (#\p . 12))
                                  char<?)])
      (test-case bbtree-intersection ()
        (assert-eqv 0 (bbtree-size (bbtree-intersection empty empty)))
        (assert-eqv 0 (bbtree-size (bbtree-intersection bbtree1 empty)))
        (assert-eqv 0 (bbtree-size (bbtree-intersection empty bbtree1)))
        (assert-eqv (bbtree-size bbtree1)
                  (bbtree-size (bbtree-intersection bbtree1 bbtree1)))
        ;; intersection favours values in first set
        (assert-equal '((#\i . 105) (#\l . 108))
                    (bbtree->alist (bbtree-intersection bbtree1 bbtree2)))
        ;; check this holds on larger bbtrees
        (let* ([l (string->list "abcdefghijlmnopqrstuvwxyz")]
               [b1 (map (lambda (x) (cons x (char->integer x))) l)]
               [b2 (map (lambda (x) (cons x #f)) l)])
          (assert-equal b1
                      (bbtree->alist (bbtree-intersection (alist->bbtree b1 char<?)
                                                          (alist->bbtree b2 char<?)))))
        ;; definition of intersection is equivalent to two differences
        (assert-equal (bbtree->alist (bbtree-intersection bbtree1 bbtree2))
                    (bbtree->alist
                     (bbtree-difference bbtree1
                                        (bbtree-difference bbtree1 bbtree2)))))))
  
  (define-test-case bbtrees bbtree-difference
    (let ([empty (make-bbtree char<?)]
          [bbtree1 (alist->bbtree '((#\g . 103) (#\u . 117) (#\i . 105) (#\l . 108) (#\e . 101))
                                  char<?)]
          [bbtree2 (alist->bbtree '((#\l . 8) (#\i . 5) (#\s . 15) (#\p . 12))
                                  char<?)])
      (test-case bbtree-difference ()
        (assert-eqv 0 (bbtree-size (bbtree-difference empty empty)))
        (assert-eqv 5 (bbtree-size (bbtree-difference bbtree1 empty)))
        (assert-eqv 0 (bbtree-size (bbtree-difference empty bbtree1)))
        (assert-eqv 0 (bbtree-size (bbtree-difference bbtree1 bbtree1)))
        (assert-equal '((#\e . 101) (#\g . 103) (#\u . 117))
                    (bbtree->alist (bbtree-difference bbtree1 bbtree2)))
        (assert-equal '((#\p . 12) (#\s . 15))
                    (bbtree->alist (bbtree-difference bbtree2 bbtree1))))))
  
  (define-test-case bbtrees bbtree-indexing
    (let* ([l (string->list "abcdefghijklmno")]
           [bb (alist->bbtree (map (lambda (x) (cons x #f)) l) char<?)])
      "tnerfgxukscjmwhaod yz"
      (test-case bbtree-difference ()
        (assert-equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
                    (map (lambda (x) (bbtree-index bb x)) l))
        (assert-raises assertion-violation? (bbtree-index bb #\z))
        (assert-equal l
                    (map (lambda (x)
                           (let-values ([(k v) (bbtree-ref/index bb x)])
                             k))
                         '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
        (assert-raises assertion-violation? (bbtree-ref/index bb -1))
        (assert-raises assertion-violation? (bbtree-ref/index bb 15)))))
  
)
