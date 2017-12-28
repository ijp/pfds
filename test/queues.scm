(library (test queues)
  (export queues)
  (import (rnrs (6))
          (chez-test suite)
          (chez-test assertions)
          (test utils)
          (pfds queues))
  
  (define-test-suite queues
    "Tests for the functional queue implementation")
  
  (define-test-case queues empty-queue ()
    (assert-predicate queue? (make-queue))
    (assert-predicate queue-empty? (make-queue))
    (assert-eqv 0 (queue-length (make-queue))))
  
  (define-test-case queues enqueue
    (let ((queue (enqueue (make-queue) 'foo)))
      (test-case enqueue ()
        (assert-predicate queue? queue)
        (assert-eqv #t (not (queue-empty? queue)))
        (assert-eqv 1 (queue-length queue))
        (assert-eqv 10 (queue-length
                      (fold-left (lambda (queue val)
                               (enqueue queue val))
                             (make-queue)
                             '(0 1 2 3 4 5 6 7 8 9)))))))
  
  (define-test-case queues dequeue ()
    (let ((empty (make-queue))
          (queue1 (enqueue (make-queue) 'foo))
          (queue2 (enqueue (enqueue (make-queue) 'foo) 'bar)))
      (let-values (((item queue) (dequeue queue1)))
        (assert-eqv 'foo item)
        (assert-predicate queue? queue)
        (assert-predicate queue-empty? queue))
      (let*-values (((first queue*) (dequeue queue2))
                    ((second queue) (dequeue queue*)))
                   (assert-eqv 'foo first)
                   (assert-eqv 'bar second)
                   (assert-eqv 1 (queue-length queue*))
                   (assert-eqv 0 (queue-length queue)))
      (assert-eqv #t
                (guard (exn ((queue-empty-condition? exn) #t)
                            (else #f))
                  (dequeue empty)
                  #f))))
  
  
  (define-test-case queues queue-ordering ()
    (let* ((list '(bar quux foo zot baz))
           (queue (list->queue list)))
      (assert-eqv 5 (queue-length queue))
      (assert-equal list (queue->list queue))))
  
)
