#!r6rs
;; Purely functional queues
;;
;; A scheme translation of "Simple and Efficient Purely Functional
;; Queues and Deques" by Chris Okazaki
;;
;; by Ian Price, 2011-09-02
;;
;;
;; Documentation:
;;
;; make-queue : () -> queue
;; returns a queue containing no items
;;
;; queue? : any -> boolean
;; tests if an object is a queue
;;
;; queue-length : queue -> non-negative integer
;; returns the number of items in the queue
;;
;; queue-empty? : queue -> boolean
;; returns true if there are no items in the queue, false otherwise
;;
;; enqueue : queue any -> queue
;; returns a new queue with the enqueued item at the end
;;
;; dequeue : queue -> value queue
;; returns two values, the item at the front of the queue, and a new
;; queue containing the all the other items
;; raises a &queue-empty condition if the queue is empty
;;
;; queue-empty-condition? : object -> boolean
;; tests if an object is a &queue-empty condition
;;
(library (pfds queues)
(export make-queue
        queue?
        queue-length
        queue-empty?
        enqueue
        dequeue
        queue-empty-condition?
        )
(import (rnrs)
        (rnrs r5rs))

;;; Lazy Lists
;;
;; If you want real lazy lists, use SRFI 41, but Okazaki uses 'odd'
;; lists, so I wrote a quick implementation.
;;
(define-syntax cons*
  (syntax-rules ()
    ((cons* a b)
     (cons a (delay b)))))

(define head car)

(define empty? null?)

(define (tail pair)
  (force (cdr pair)))

(define (rotate l r a)
  (if (empty? l)
      (cons* (head r) a)
      (cons* (head l)
             (rotate (tail l)
                     (tail r)
                     (cons* (head r) a)))))


;;; Implementation

(define-record-type (queue %make-queue queue?)
  (fields
   (immutable length)
   (immutable l)
   (immutable r)
   (immutable l^)))


(define (make-queue)
  (%make-queue 0 '() '() '()))

(define (enqueue queue item)
  (let ((len (queue-length queue))
        (l (queue-l queue))
        (r (queue-r queue))
        (l^ (queue-l^ queue)))
    (makeq (+ len 1) l (cons* item r) l^)))

(define (dequeue queue)
  (when (queue-empty? queue)
    ;; (error 'dequeue  "Can't dequeue empty queue")
    (raise (condition
            (make-queue-empty-condition)
            (make-who-condition 'dequeue)
            (make-message-condition "There are no elements to dequeue")
            (make-irritants-condition (list queue)))))
  (let ((len (queue-length queue))
        (l (queue-l queue))
        (r (queue-r queue))
        (l^ (queue-l^ queue)))
    (values (head l)
            (makeq (- len 1) (tail l) r l^))))

(define (makeq length l r l^)
  (if (empty? l^)
      (let ((l* (rotate l r '())))
        (%make-queue length l* '() l*))
      (%make-queue length l r (tail l^))))


(define (queue-empty? queue)
  (zero? (queue-length queue)))

(define-condition-type &queue-empty
  &assertion
  make-queue-empty-condition
  queue-empty-condition?)

)
