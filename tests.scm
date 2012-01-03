#!r6rs
;; Copyright (C) 2011,2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Code:
(import (rnrs)
        (pfds queues)
        (pfds deques)
        (pfds bbtrees)
        (wak trc-testing))

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))

(define-syntax test-exn
  (syntax-rules ()
    ((test-exn exception-pred? body)
     (test-eqv #t
               (guard (exn ((exception-pred? exn) #t)
                           (else #f))
                 body
                 #f)))))

(define-syntax test-no-exn
  (syntax-rules ()
    ((test-no-exn body)
     (test-eqv #t
               (guard (exn (else #f))
                 body
                 #t)))))

(define-test-suite pfds
  "Test suite for libraries under the (pfds) namespace")

(define-test-suite (queues pfds)
  "Tests for the functional queue implementation")

(define-test-case queues empty-queue ()
  (test-predicate queue? (make-queue))
  (test-predicate queue-empty? (make-queue))
  (test-eqv 0 (queue-length (make-queue))))

(define-test-case queues enqueue ()
  (let ((queue (enqueue (make-queue) 'foo)))
    (test-predicate queue? queue)
    (test-eqv #t (not (queue-empty? queue)))
    (test-eqv 1 (queue-length queue))
    (test-eqv 10 (queue-length
                  (foldl (lambda (val queue)
                           (enqueue queue val))
                         (make-queue)
                         '(0 1 2 3 4 5 6 7 8 9))))))

(define-test-case queues dequeue ()
  (let ((empty (make-queue))
        (queue1 (enqueue (make-queue) 'foo))
        (queue2 (enqueue (enqueue (make-queue) 'foo) 'bar)))
    (let-values (((item queue) (dequeue queue1)))
      (test-eqv 'foo item)
      (test-predicate queue? queue)
      (test-predicate queue-empty? queue))
    (let*-values (((first queue*) (dequeue queue2))
                  ((second queue) (dequeue queue*)))
                 (test-eqv 'foo first)
                 (test-eqv 'bar second)
                 (test-eqv 1 (queue-length queue*))
                 (test-eqv 0 (queue-length queue)))
    (test-eqv #t
              (guard (exn ((queue-empty-condition? exn) #t)
                          (else #f))
                (dequeue empty)
                #f))))


(define-test-case queues queue-ordering ()
  (let* ((list '(bar quux foo zot baz))
         (queue (foldl (lambda (val queue)
                          (enqueue queue val))
                        (make-queue)
                        list)))
    (test-eqv 5 (queue-length queue))
    (let loop ((queue queue) (list list))
      (unless (null? list)
        (let-values (((item queue) (dequeue queue)))
          (test-eqv (car list) item)
          (loop queue (cdr list)))))))


(define-test-suite (deques pfds)
  "Tests for the functional deque implementation")

(define-test-case deques empty-deque ()
  (test-predicate deque? (make-deque))
  (test-predicate deque-empty? (make-deque))
  (test-eqv 0 (deque-length (make-deque))))

(define-test-case deques deque-insert ()
  (let ((deq (enqueue-front (make-deque) 'foo)))
    (test-predicate deque? deq)
    (test-eqv 1 (deque-length deq)))
  (let ((deq (enqueue-rear (make-deque) 'foo)))
    (test-predicate deque? deq)
    (test-eqv 1 (deque-length deq)))
  (test-eqv 5 (deque-length
               (foldl (lambda (pair deque)
                        ((car pair) deque (cdr pair)))
                      (make-deque)
                      `((,enqueue-front . 0)
                        (,enqueue-rear  . 1)
                        (,enqueue-front . 2)
                        (,enqueue-rear  . 3)
                        (,enqueue-front . 4))))))

(define-test-case deques deque-remove ()
  (let ((deq (enqueue-front (make-deque) 'foo)))
    (let-values (((item0 deque0) (dequeue-front deq))
                 ((item1 deque1) (dequeue-rear deq)))
      (test-eqv 'foo item0)
      (test-eqv 'foo item1)
      (test-predicate deque-empty? deque0)
      (test-predicate deque-empty? deque1)))
  (let ((deq (foldl (lambda (item deque)
                      (enqueue-rear deque item))
                    (make-deque)
                    '(0 1 2 3 4 5))))
    (let*-values (((item0 deque0) (dequeue-front deq))
                  ((item1 deque1) (dequeue-front deque0))
                  ((item2 deque2) (dequeue-front deque1)))
      (test-eqv 0 item0)
      (test-eqv 1 item1)
      (test-eqv 2 item2)
      (test-eqv 3 (deque-length deque2))))
  (let ((deq (foldl (lambda (item deque)
                      (enqueue-rear deque item))
                    (make-deque)
                    '(0 1 2 3 4 5))))
    (let*-values (((item0 deque0) (dequeue-rear deq))
                  ((item1 deque1) (dequeue-rear deque0))
                  ((item2 deque2) (dequeue-rear deque1)))
      (test-eqv 5 item0)
      (test-eqv 4 item1)
      (test-eqv 3 item2)
      (test-eqv 3 (deque-length deque2))))
  (let ((empty (make-deque)))
    (test-eqv #t
              (guard (exn ((deque-empty-condition? exn) #t)
                          (else #f))
                (dequeue-front empty)
                #f))
    (test-eqv #t
              (guard (exn ((deque-empty-condition? exn) #t)
                          (else #f))
                (dequeue-rear empty)
                #f))))


(define-test-case deques mixed-operations ()
  (let ((deque (foldl (lambda (pair deque)
                        ((car pair) deque (cdr pair)))
                      (make-deque)
                      `((,enqueue-front . 0)
                        (,enqueue-rear  . 1)
                        (,enqueue-front . 2)
                        (,enqueue-rear  . 3)
                        (,enqueue-front . 4)))))
    (let*-values (((item0 deque) (dequeue-front deque))
                  ((item1 deque) (dequeue-front deque))
                  ((item2 deque) (dequeue-front deque))
                  ((item3 deque) (dequeue-front deque))
                  ((item4 deque) (dequeue-front deque)))
      (test-eqv 4 item0)
      (test-eqv 2 item1)
      (test-eqv 0 item2)
      (test-eqv 1 item3)
      (test-eqv 3 item4)))
  (let ((deq (foldl (lambda (item deque)
                      (enqueue-rear deque item))
                    (make-deque)
                    '(0 1 2))))
    (let*-values (((item0 deque0) (dequeue-rear deq))
                  ((item1 deque1) (dequeue-front deque0))
                  ((item2 deque2) (dequeue-rear deque1)))
      (test-eqv 2 item0)
      (test-eqv 0 item1)
      (test-eqv 1 item2)
      (test-predicate deque-empty? deque2))))


(define-test-suite (bbtrees pfds)
  "Tests for the bounded balance tree imlementation")

(define-test-case bbtrees empty-tree ()
  (test-predicate bbtree? (make-bbtree <))
  (test-eqv 0 (bbtree-size (make-bbtree <))))

(define-test-case bbtrees bbtree-set ()
  (let* ([tree1 (bbtree-set (make-bbtree <) 1 'a)]
         [tree2 (bbtree-set tree1 2 'b)]
         [tree3 (bbtree-set tree2 1 'c )])
    (test-eqv 1 (bbtree-size tree1))
    (test-eqv 'a (bbtree-ref tree1 1))
    (test-eqv 2 (bbtree-size tree2))
    (test-eqv 'b (bbtree-ref tree2 2))
    (test-eqv 2 (bbtree-size tree3))
    (test-eqv 'c (bbtree-ref tree3 1))
    (test-exn assertion-violation? (bbtree-ref tree3 20))))

(define-test-case bbtrees bbtree-delete ()
  (let* ([tree1 (bbtree-set (bbtree-set (bbtree-set (make-bbtree string<?) "a" 3)
                                        "b"
                                        8)
                            "c"
                            19)]
         [tree2 (bbtree-delete tree1 "b")]
         [tree3 (bbtree-delete tree2 "a")])
    (test-eqv 3 (bbtree-size tree1))
    (test-eqv #t (bbtree-contains? tree1 "b"))
    (test-eqv #t (bbtree-contains? tree1 "a"))
    (test-eqv 2 (bbtree-size tree2))
    (test-eqv #f (bbtree-contains? tree2 "b"))
    (test-eqv #t (bbtree-contains? tree2 "a"))
    (test-eqv 1 (bbtree-size tree3))
    (test-eqv #f (bbtree-contains? tree3 "a"))
    (test-no-exn (bbtree-delete (bbtree-delete tree3 "a") "a"))))

(run-test pfds)
