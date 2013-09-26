;;; Functional Breadth First Search
(import (rnrs)
        (pfds queues))

;; This is the traditional solution using Queues, for a more
;; interesting solution, see "The Under-Appreciated Unfold" by Jeremy
;; Gibbons and Geraint Jones.

;; We'll need a tree type, we'll use #f for an empty child.
(define-record-type tree
  (fields value left right))

;; A small section of the Stern-Brocot Tree
;; https://en.wikipedia.org/wiki/Stern%E2%80%93Brocot_tree
(define stern-brocot
  (make-tree 1
             (make-tree 1/2
                        (make-tree 1/3
                                   (make-tree 1/4 #f #f)
                                   (make-tree 2/5 #f #f))
                        (make-tree 2/3
                                   (make-tree 3/5 #f #f)
                                   (make-tree 3/4 #f #f)))
             (make-tree 2
                        (make-tree 3/2
                                   (make-tree 4/3 #f #f)
                                   (make-tree 5/3 #f #f))
                        (make-tree 3
                                   (make-tree 5/2 #f #f)
                                   (make-tree 4 #f #f)))))

;; We'll search it breadth-first for the first fraction expressed in
;; fifths.
(define (fifth? f)
  (= 5 (denominator f)))

;; The queue search
(define (bfs p? tree)
  (define (step queue)
    (if (queue-empty? queue)
        #f
        (let-values ([(head queue*) (dequeue queue)])
          (cond ((not head)                 ; empty-tree, skip
                 (step queue*))
                ((p? (tree-value head)) (tree-value head))
                (else
                 (step (enqueue (enqueue queue* (tree-left head))
                                (tree-right head))))))))

  (step (enqueue (make-queue) tree)))

(equal? 2/5 (bfs fifth? stern-brocot))
