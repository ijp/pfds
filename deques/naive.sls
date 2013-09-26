#!r6rs
(library (pfds deques naive)
(export make-deque
        deque?
        deque-length
        deque-empty?
        enqueue-front
        enqueue-rear
        dequeue-front
        dequeue-rear
        deque-empty-condition?
        deque->list
        list->deque
        )
(import (rnrs)
        (pfds deques private condition))

(define-record-type (deque %make-deque deque?)
  (fields length head tail))

(define (make-deque)
  (%make-deque 0 '() '()))

(define (deque-empty? deque)
  (zero? (deque-length deque)))

(define (enqueue-front deque object)
  (%make-deque (+ 1 (deque-length deque))
               (cons object (deque-head deque))
               (deque-tail deque)))

(define (enqueue-rear deque object)
  (%make-deque (+ 1 (deque-length deque))
               (deque-head deque)
               (cons object (deque-tail deque))))

(define (dequeue-front deque)
  (when (deque-empty? deque)
    (raise (condition
            (make-deque-empty-condition)
            (make-who-condition 'dequeue-front)
            (make-message-condition "There are no elements to dequeue")
            (make-irritants-condition (list deque)))))
  (let ((l (deque-length deque))
        (h (deque-head deque))
        (t (deque-tail deque)))
    (if (null? h)
        (let ((h* (reverse t)))
          (values (car h*)
                  (%make-deque (- l 1) (cdr h*) '())))
        (values (car h)
                (%make-deque (- l 1) (cdr h) t)))))

(define (dequeue-rear deque)
  (when (deque-empty? deque)
    (raise (condition
            (make-deque-empty-condition)
            (make-who-condition 'dequeue-rear)
            (make-message-condition "There are no elements to dequeue")
            (make-irritants-condition (list deque)))))
  (let ((l (deque-length deque))
        (h (deque-head deque))
        (t (deque-tail deque)))
    (if (null? t)
        (let ((t* (reverse h)))
          (values (car t*)
                  (%make-deque (- l 1) '() (cdr t*))))
        (values (car t)
                (%make-deque (- l 1) h (cdr t))))))

(define (list->deque l)
  (%make-deque (length l) l '()))

(define (deque->list deque)
  (let ((h (deque-head deque))
        (t (deque-tail deque)))
    (append h (reverse t))))

)
