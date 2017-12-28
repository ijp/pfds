(library (test sets)
  (export sets)
  (import (rnrs (6))
          (chez-test suite)
          (test utils)
          (pfds sets))
  
  (define-test-suite sets
    "Tests for the set implementation")
  
  (define-test-case sets set-basics
    (let ([empty (make-set string<?)]
          [set (fold-left set-insert
                          (make-set string<?)
                          (list "foo" "bar" "baz" "quux" "zot"))])
      (test-case set-basics ()
        (test-predicate set? empty)
        (test-predicate set? set)
        (test-eqv 0 (set-size empty))
        (test-eqv 5 (set-size set))
        (test-eqv #f (set-member? empty "foo"))
        (test-eqv #t (set-member? (set-insert empty "foo") "foo"))
        (test-eqv #t (set-member? set "foo"))
        (test-eqv #f (set-member? (set-remove set "foo") "foo"))
        (test-no-exn (set-remove empty "anything"))
        (test-no-exn (set-insert set "quux"))
        (test-eqv (set-size (set-insert empty "foo"))
                  (set-size (set-insert (set-insert empty "foo") "foo")))
        (test-eqv (set-size (set-remove set "foo"))
                  (set-size (set-remove (set-remove set "foo") "foo"))))))
  
  (define-test-case sets set-equality
    (let* ([empty (make-set string<?)]
           [set1  (list->set '("foo" "bar" "baz") string<?)]
           [set2  (list->set '("foo" "bar" "baz" "quux" "zot") string<?)]
           [sets  (list empty set1 set2)])
      (test-case set-equality ()
        (assert (for-all (lambda (x) (set=? x x)) sets))
        (assert (for-all (lambda (x) (subset? x x)) sets))
        (test-not (exists (lambda (x) (proper-subset? x x)) sets))
        (assert (set<? empty set1))
        (assert (set<? set1 set2))
        (assert (set=? (set-insert set1 "quux")
                     (set-remove set2 "zot"))))))
  
  (define-test-case sets set-operations
    (let* ([empty (make-set <)]
           [set1 (list->set '(0 2 5 7 12 2 3 62 5) <)]
           [set2 (list->set '(94 33 44 2 73 55 48 92 98 29
                              28 98 55 20 69 5 33 53 89 50)
                            <)]
           [sets (list empty set1 set2)])
      (test-case set-operations ()
        (assert (for-all (lambda (x) (set=? x (set-union x x))) sets))
        (assert (for-all (lambda (x) (set=? x (set-intersection x x))) sets))
        (assert (for-all (lambda (x) (set=? empty (set-difference x x))) sets))
        (assert (for-all (lambda (x) (set=? x (set-union empty x))) sets))
        (assert (for-all (lambda (x) (set=? empty (set-intersection empty x))) sets))
        (assert (for-all (lambda (x) (set=? x (set-difference x empty))) sets))
        (assert (for-all (lambda (x) (set=? empty (set-difference empty x))) sets))
  
        (assert (set=? (set-union set1 set2) (set-union set2 set1)))
        (assert (set=? (set-union set1 set2)
                     (list->set '(0 2 3 69 7 73 12 20 89 28
                                  29 94 5 33 98 92 44 48 50 53
                                  55 62)
                                <)))
  
        (assert (set=? (set-intersection set1 set2) (set-intersection set2 set1)))
        (assert (set=? (set-intersection set1 set2)
                     (list->set '(2 5) <)))
        (assert (set=? (set-difference set1 set2)
                     (list->set '(0 3 12 62 7) <)))
        (assert (set=? (set-difference set2 set1)
                     (list->set '(33 98 69 73 44 48 92 50 20 53
                                  55 89 28 29 94)
                                <))))))
  
  (define-test-case sets set-conversion ()
    (test-eqv '() (set->list (make-set <)))
    (test-eqv 0 (set-size (list->set '() <)))
    (test-equal (string->list "abcdefghijklmno")
                (list-sort char<?
                           (set->list
                            (list->set (string->list "abcdefghijklmno") char<?))))
    (test-equal '(0) (set->list (fold-left set-insert (make-set <) '(0 0 0 0)))))
  
  (define-test-case sets set-iterators ()
    (test-eqv 0 (set-fold + 0 (list->set '() <)))
    (test-eqv 84 (set-fold + 0 (list->set '(3 12 62 7) <)))
    (test-eqv 499968 (set-fold * 1 (list->set '(3 12 62 7 8 4) <))))
  
)
