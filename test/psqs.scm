(library (test psqs)
  (export psqs)
  (import (rnrs (6))
          (chez-test suite)
          (chez-test assertions)
          (test utils)
          (pfds psqs))
  
  (define (alist->psq alist key<? priority<?)
    (fold-left (lambda (psq kv)
             (psq-set psq (car kv) (cdr kv)))
           (make-psq key<? priority<?)
           alist))
  
  (define-test-suite psqs
    "Tests for the functional priority search tree implementation")
  
  (define-test-case psqs empty-psq ()
    (assert-predicate psq? (make-psq string<? <))
    (assert-predicate psq-empty? (make-psq string<? <))
    (assert-predicate zero? (psq-size (make-psq string<? <))))
  
  (define-test-case psqs psq-set
    (let* ((empty (make-psq char<? <))
           (psq1  (psq-set empty #\a 10))
           (psq2  (psq-set psq1 #\b 33))
           (psq3  (psq-set psq2 #\c 3))
           (psq4  (psq-set psq3 #\a 12)))
      (test-case psq-set ()
        (assert-eqv 10 (psq-ref psq1 #\a))
        (assert-raises assertion-violation? (psq-ref psq1 #\b))
        (assert-eqv 1 (psq-size psq1))
        
        (assert-eqv 10 (psq-ref psq2 #\a))
        (assert-eqv 33 (psq-ref psq2 #\b))
        (assert-not (psq-contains? psq2 #\c))
        (assert-eqv 2 (psq-size psq2))
        
        (assert-eqv 10 (psq-ref psq3 #\a))
        (assert-eqv 33 (psq-ref psq3 #\b))
        (assert-eqv 3  (psq-ref psq3 #\c))
        (assert-eqv 3 (psq-size psq3))
  
        (assert-eqv 12 (psq-ref psq4 #\a))
        (assert-eqv 33 (psq-ref psq4 #\b))
        (assert-eqv 3  (psq-ref psq4 #\c))
        (assert-eqv 3 (psq-size psq4)))))
  
  (define-test-case psqs psq-delete
    (let* ((psq1 (alist->psq '((#\a . 10) (#\b . 33) (#\c . 3))
                             char<?
                             <))
           (psq2 (psq-delete psq1 #\c))
           (psq3 (psq-delete psq2 #\b))
           (psq4 (psq-delete psq3 #\a))
           (psq5 (psq-delete psq1 #\d)))
      (test-case psq-delete ()
        (assert-eqv #t (psq-contains? psq1 #\c))
        (assert-not (psq-contains? psq2 #\c))
        (assert-eqv #t (psq-contains? psq2 #\b))
        (assert-not (psq-contains? psq3 #\b))
        (assert-eqv #t (psq-contains? psq3 #\a))
        (assert-predicate psq-empty? psq4)
        (assert-eqv (psq-size psq1)
                  (psq-size psq5)))))
  
  (define-test-case psqs psq-update
    (let* ((empty (make-psq char<? <))
           (psq1  (psq-update empty #\a add1 10))
           (psq2  (psq-update psq1 #\b add1 33))
           (psq3  (psq-update psq2 #\c add1 3))
           (psq4  (psq-update psq3 #\a add1 0))
           (psq5  (psq-update psq3 #\c add1 0)))
      (test-case psq-update ()
        (assert-eqv 11 (psq-ref psq3 #\a))
        (assert-eqv 34 (psq-ref psq3 #\b))
        (assert-eqv 4  (psq-ref psq3 #\c))
        
        (assert-eqv 12 (psq-ref psq4 #\a))
        (assert-eqv 34 (psq-ref psq4 #\b))
        (assert-eqv 4  (psq-ref psq4 #\c))
        (assert-eqv 3  (psq-size psq4))
        
        (assert-eqv 11 (psq-ref psq5 #\a))
        (assert-eqv 34 (psq-ref psq5 #\b))
        (assert-eqv 5  (psq-ref psq5 #\c))
        (assert-eqv 3  (psq-size psq5)))))
  
  (define-test-case psqs priority-queue-functions
    (let* ((psq1 (alist->psq '((#\a . 10) (#\b . 33) (#\c . 3) (#\d . 23) (#\e . 7))
                             char<?
                             <))
           (psq2 (psq-delete-min psq1))
           (psq3 (psq-delete-min (psq-set psq2 #\b 9)))
           (psq4 (make-psq < <)))
      (test-case priority-queue-functions ()
        (assert-eqv #\c (psq-min psq1))
        (assert-eqv #\e (psq-min psq2))
        (assert-raises assertion-violation? (psq-delete-min psq4))
        (assert-eqv #\a (psq-min (psq-set psq1 #\a 0)))
        (call-with-values
            (lambda ()
              (psq-pop psq3))
          (lambda (min rest)
            (assert-eqv #\b min)
            (assert-eqv #\a (psq-min rest)))))))
  
  (define-test-case psqs ranged-functions
    (let* ((alist '((#\f . 24) (#\u . 42) (#\p . 16) (#\s . 34) (#\e . 17)
                    (#\x . 45) (#\l . 14) (#\z . 5) (#\t . 45) (#\r . 41)
                    (#\k . 32) (#\w . 14) (#\d . 12) (#\c . 16) (#\m . 20) (#\j . 25)))
           (alist-sorted (list-sort (lambda (x y)
                                      (char<? (car x) (car y)))
                                    alist))
           (psq  (alist->psq alist char<? <)))
      (test-case ranged-functions ()
        (assert-equal alist-sorted
                    (psq-at-most psq +inf.0))
        (assert-equal '() (psq-at-most psq 0))
        (assert-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
                      (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
                    (psq-at-most psq 20))
        (assert-equal alist-sorted
                    (psq-at-most-range psq +inf.0 #\x00 #\xFF))
        ;; with bounds outwith range in psq, is the same as psq-at-most
        (assert-equal '() (psq-at-most-range psq 0 #\x00 #\xFF))
        (assert-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
                      (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
                    (psq-at-most-range psq 20 #\x00 #\xFF))
        (assert-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
                      (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
                    (psq-at-most psq 20))
        (assert-equal (filter (lambda (x) (char<=? #\e (car x) #\u)) alist-sorted)
                    (psq-at-most-range psq +inf.0 #\e #\u))
        (assert-equal '() (psq-at-most-range psq 0 #\e #\u))
        (assert-equal '((#\e . 17) (#\l . 14) (#\m . 20) (#\p . 16))
                    (psq-at-most-range psq 20 #\e #\u))
        ;; inclusiveness check
        (assert-equal '((#\t . 45))
                    (psq-at-most-range psq 80 #\t #\t))
        ;; if lower bound is higher than upper, then nothing
        (assert-equal '() (psq-at-most-range psq 80 #\t #\r)))))
  
)
