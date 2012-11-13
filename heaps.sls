#!r6rs
(library (pfds heaps)
(export make-heap
        (rename (%heap heap))
        heap?
        heap-size
        heap-empty?
        heap-min
        heap-delete-min
        heap-insert
        heap-pop
        heap->list
        list->heap
        heap-merge
        heap-sort
        (rename (heap-ordering-predicate heap-ordering-procedure))
        heap-empty-condition?
        )
(import (rnrs))

(define-record-type (node %make-node node?)
  (fields size height value left right))

(define-record-type leaf)

(define (height x)
  (if (leaf? x)
      0
      (node-height x)))

(define (size x)
  (if (leaf? x)
      0
      (node-size x)))

(define (make-node v l r)
  (define sl (height l))
  (define sr (height r))
  (define m (+ 1 (min sl sr)))
  (define sz (+ 1 (size l) (size r)))
  (if (< sl sr)
      (%make-node sz m v r l)
      (%make-node sz m v l r)))

(define (singleton v)
  (%make-node 1 0 v (make-leaf) (make-leaf)))

(define (insert tree value prio<?)
  (merge-trees tree (singleton value) prio<?))

(define (delete-min tree prio<?)
  (merge-trees (node-left tree)
               (node-right tree)
               prio<?))

(define (merge-trees tree1 tree2 prio<?)
  (cond ((leaf? tree1) tree2)
        ((leaf? tree2) tree1)
        ((prio<? (node-value tree2)
                 (node-value tree1))
         (make-node (node-value tree2)
                    (node-left tree2)
                    (merge-trees tree1
                                 (node-right tree2)
                                 prio<?)))
        (else
         (make-node (node-value tree1)
                    (node-left tree1)
                    (merge-trees (node-right tree1)
                                 tree2
                                 prio<?)))))


;; outside interface
(define-record-type (heap %make-heap heap?)
  (fields tree ordering-predicate))

(define (make-heap priority<?)
  (%make-heap (make-leaf) priority<?))

(define (%heap < . vals)
  (list->heap vals <))

(define (heap-size heap)
  (size (heap-tree heap)))

(define (heap-empty? heap)
  (leaf? (heap-tree heap)))

(define (heap-min heap)
  (when (heap-empty? heap)
    (raise (condition
            (make-heap-empty-condition)
            (make-who-condition 'heap-min)
            (make-message-condition "There is no minimum element.")
            (make-irritants-condition (list heap)))))
  (node-value (heap-tree heap)))

(define (heap-delete-min heap)
  (when (heap-empty? heap)
    (raise (condition
            (make-heap-empty-condition)
            (make-who-condition 'heap-delete-min)
            (make-message-condition "There is no minimum element.")
            (make-irritants-condition (list heap)))))
  (let ((< (heap-ordering-predicate heap)))
    (%make-heap (delete-min (heap-tree heap) <) <)))

(define (heap-pop heap)
  (when (heap-empty? heap)
    (raise (condition
            (make-heap-empty-condition)
            (make-who-condition 'heap-pop)
            (make-message-condition "There is no minimum element.")
            (make-irritants-condition (list heap)))))
  (let* ((tree (heap-tree heap))
         (top  (node-value tree))
         (<    (heap-ordering-predicate heap))
         (rest (delete-min tree <)))
    (values top
            (%make-heap rest <))))

(define (heap-insert heap value)
  (assert (heap? heap))
  (let ((< (heap-ordering-predicate heap)))
    (%make-heap (insert (heap-tree heap) value <) <)))

(define (heap->list heap)
  (assert (heap? heap))
  (let ((< (heap-ordering-predicate heap)))
    (let loop ((tree (heap-tree heap)) (list '()))
      (if (leaf? tree)
          (reverse list)
          (loop (delete-min tree <)
                (cons (node-value tree) list))))))

(define (list->heap list <)
  (%make-heap
   (fold-left (lambda (h item)
                (insert h item <))
              (make-leaf)
              list)
   <))

(define (heap-merge heap1 heap2)
  (define < (heap-ordering-predicate heap1))
  (%make-heap
   (merge-trees (heap-tree heap1)
                (heap-tree heap2)
                <)
   <))

(define (heap-sort < list)
  (heap->list (list->heap list <)))

(define-condition-type &heap-empty
  &assertion
  make-heap-empty-condition
  heap-empty-condition?)
)
