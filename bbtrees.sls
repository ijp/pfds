#!r6rs
;;; bbtrees.sls --- Bounded Balance trees

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; Documentation:
;;
;; Note: For all procedures which take a key as an argument, the key
;; must be comparable with the ordering procedure of the bbtree.
;;
;; make-bbtree : (any -> any -> boolean) -> bbtree
;; returns an empty bbtree. bbtrees derived from this one will use the
;; procedure argument for ordering keys.
;;
;; bbtree? : any -> bool
;; returns #t if the argument is a bbtree, #f otherwise
;;
;; bbtree-size : bbtree -> non-negative integer
;; returns the number of elements in a bbtree
;;
;; bbtree-ref : bbtree any -> any
;; returns the value associated with the key in the bbtree.
;; If the value is not in the tree, an &assertion-violation condition is raised.
;;
;; bbtree-set : bbtree any any -> bbtree
;; returns a new bbtree with the key associated with the value. If the
;; key is already in the bbtree, its associated value is replaced with
;; the new value in the returned bbtree.
;;
;; bbtree-delete : bbtree any -> bbtree
;; returns a new bbtree with the key and its associated value
;; removed. If the key is not in the bbtree, the returned bbtree is a
;; copy of the original
;;
;; bbtree-contains? : bbtree any -> boolean
;; returns #t if there is association for key in the bbtree, false
;; otherwise
;;
;; bbtree-traverse : (any any (any -> any) (any -> any) any) any bbtree -> any
;; A general tree traversal procedure. Returns the value of applying
;; the traverser procedure to the current node's key, value, a
;; procedure to traverse the left subtree, a procedure to traverse the
;; right subtree, and a base value. The subtree traversal procedures
;; both take a base argument, and call bbtree-traverse recursively on
;; the appropriate subtree. It is mostly useful for implementing
;; other, more specific tree traversal procedures. For example,
;;   (define (l-to-r-pre-order cons base bbtree)
;;     (bbtree-traverse (lambda (key value left right base)
;;                        (r (l (cons key value base))))
;;                      base
;;                      bbtree))
;; implements a left-to-right pre-order traversal variant of bbtree-fold
;;
;; bbtree-fold : (any any any -> any) any bbtree -> any
;; returns the value obtained by the iterating the combine procedure
;; over each node in the tree. The combine procedure takes three
;; arguments, the key and value of the current node, and an
;; accumulator value, and its return value is used as the accumulator
;; value for the next node. The initial accumulator value is provided
;; by the base argument. bbtree-fold performs an left-to-right
;; in-order traversal or "minimum key to maximum key".
;;
;; bbtree-fold-right : (any any any -> any) any bbtree -> any
;; like bbtree-fold, but it performs a right-to-left in-order
;; traversal instead (i.e. maximum to minimum).
;;
;; bbtree-map : (any -> any) bbtree -> bbtree
;; returns the tree obtained by updating the value of each node with
;; the result of applying the procedure to its value.
;;
;; bbtree->alist : bbtree -> Listof(Pairs)
;; returns the key value associations of the bbtree as a list of
;; pairs. The list returned is in sorted order according to the
;; ordering procedure of the bbtree. A consequence of this is that one
;; could write a sort procedure for lists of pairs as
;;   (define (alist-sort alist <)
;;     (bbtree->alist (alist->bbtree alist <)))
;;
;; alist->bbtree : Listof(Pairs) -> (any any -> boolean) -> bbtree
;; returns the bbtree containing each of the key value pairs in the
;; alist, using the < argument as the ordering procedure.
(library (pfds bbtrees)
(export make-bbtree
        bbtree?
        bbtree-size
        bbtree-ref
        bbtree-set
        bbtree-delete
        bbtree-contains?
        bbtree-ordering-procedure
        bbtree-traverse
        bbtree-fold
        bbtree-fold-right
        bbtree-map
        bbtree->alist
        alist->bbtree
        )

(import (rnrs))

(define weight 4)

;;; bbtree is the wrapper that you interact with from outside the
;;; module, so there is no need to deal with empty and node record types
(define-record-type (bbtree %make-bbtree bbtree?)
  (fields tree ordering-procedure))

(define (update-tree bbtree new-tree)
  (%make-bbtree new-tree (bbtree-ordering-procedure bbtree)))

;;; inner representation of trees
;;; all non exposed methods can assume a valid tree
(define-record-type empty)

(define-record-type node
  (fields key value length left right))

;;; smart constructor for nodes, automatically fills in size field
(define (node* key value left right)
  (make-node key value (+ 1 (size left) (size right)) left right))

(define (size tree)
  (if (empty? tree)
      0
      (node-length tree)))

;; looks key up in the tree, and applies proc to the value if it finds
;; it, and calls failure otherwise
(define (lookup tree key proc failure <)
  (define (search tree)
    (cond [(empty? tree) (failure)]
          [(< (node-key tree) key)
           (search (node-right tree))]
          [(< key (node-key tree))
           (search (node-left tree))]
          [else (proc tree)]))
  (search tree))

;; returns the key and value of the minimum element in the tree
(define (min tree)
  (cond [(empty? tree)
         (assertion-violation 'min "Can't take the minimum value of an empty tree")]
        [(empty? (node-left tree))
         (values (node-key tree)
                 (node-value tree))]
        [else
         (min (node-left tree))]))

;;; rotations
(define (rotate-left key value left right)
  (let ([r-key   (node-key right)]
        [r-value (node-value right)]
        [r-left  (node-left right)]
        [r-right (node-right right)])
    (node* r-key
           r-value
           (node* key value left r-left)
           r-right)))

(define (rotate-right key value left right)
  (let ([l-key   (node-key left)]
        [l-value (node-value left)]
        [l-left  (node-left left)]
        [l-right (node-right left)])
    (node* l-key
           l-value
           l-left
           (node* key value l-right right))))

(define (rotate-left/double key value left right)
  (let ([r-key   (node-key right)]
        [r-value (node-value right)]
        [r-left  (node-left right)]
        [r-right (node-right right)])
    (let ([rl-key   (node-key r-left)]
          [rl-value (node-value r-left)]
          [rl-left  (node-left r-left)]
          [rl-right (node-right r-left)])
      (node* rl-key
             rl-value
             (node* key value left rl-left)
             (node* r-key r-value rl-right r-right)))))

(define (rotate-right/double key value left right)
  (let ([l-key   (node-key left)]
        [l-value (node-value left)]
        [l-left  (node-left left)]
        [l-right (node-right left)])
    (let ([lr-key   (node-key l-right)]
          [lr-value (node-value l-right)]
          [lr-left  (node-left l-right)]
          [lr-right (node-right l-right)])
      (node* lr-key
             lr-value
             (node* l-key l-value l-left lr-left)
             (node* key value lr-right right)))))

;;; smart constructor for after adding/removing a node
(define (T key value left right)
  (let ((l-size (size left))
        (r-size (size right)))
    (cond [(< (+ l-size r-size) 2)
           (node* key value left right)]
          [(> r-size (* weight l-size))
           (let ([r-left (node-left right)]
                 [r-right (node-right right)])
             (if (< (size r-left) (size r-right))
                 (rotate-left key value left right)
                 (rotate-left/double key value left right)))]
          [(> l-size (* weight r-size))
           (let ([l-left (node-left left)]
                 [l-right (node-right left)])
             (if (< (size l-right) (size l-left))
                 (rotate-right key value left right)
                 (rotate-right/double key value left right)))]
          [else
           (node* key value left right)])))

(define (add tree key value <)
  (define (add-to tree)
    (if (empty? tree)
        (make-node key value 1 (make-empty) (make-empty))
        (let ([k (node-key tree)]
              [v (node-value tree)]
              [l (node-left tree)]
              [r (node-right tree)])
          (cond [(< key k)
                 (T k v (add-to l) r)]
                [(< k key)
                 (T k v l (add-to r))]
                [else
                 (node* key value l r)]))))
  (add-to tree))

(define (delete tree key <)
  (define (delete-from tree)
    (if (empty? tree)
        tree
        (let ([k (node-key tree)]
              [v (node-value tree)]
              [l (node-left tree)]
              [r (node-right tree)])
          (cond [(< key k)
                 (T k v (delete-from l) r)]
                [(< k key)
                 (T k v l (delete-from r))]
                [else
                 (delete* l r)]))))
  (delete-from tree))

(define (delete* left right)
  (cond ((empty? left) right)
        ((empty? right) left)
        (else
         (let-values (((k v) (min right)))
           (T k v left (delete-min right))))))

(define (delete-min tree)
  (cond ((empty? tree)
         (assertion-violation 'delete-min
                              "Can't delete the minimum value of an empty tree"))
        ((empty? (node-left tree))
         (node-right tree))
        (else
         (T (node-key tree)
            (node-value tree)
            (delete-min (node-left tree))
            (node-right tree)))))

;;; External procedures

(define (make-bbtree <)
  (assert (procedure? <))
  (%make-bbtree (make-empty) <))

(define (bbtree-size bbtree)
  (assert (bbtree? bbtree))
  (size (bbtree-tree bbtree)))

(define (bbtree-ref bbtree key)
  (assert (bbtree? bbtree))
  (lookup (bbtree-tree bbtree)
          key
          node-value
          (lambda ()
            (assertion-violation 'bbtree-ref
                                 "Key is not in the tree"
                                 key))
          (bbtree-ordering-procedure bbtree)))

(define (bbtree-set bbtree key value)
  (assert (bbtree? bbtree))
  (update-tree bbtree
               (add (bbtree-tree bbtree)
                    key
                    value
                    (bbtree-ordering-procedure bbtree))))

(define (bbtree-delete bbtree key)
  (assert (bbtree? bbtree))
  (update-tree bbtree
               (delete (bbtree-tree bbtree)
                       key
                       (bbtree-ordering-procedure bbtree))))

(define (bbtree-contains? bbtree key)
  (assert (bbtree? bbtree))
  (lookup (bbtree-tree bbtree)
          key
          (lambda (_) #t)
          (lambda () #f)
          (bbtree-ordering-procedure bbtree)))

;; iterators

(define (traverse traverser base tree)
  (define (left base)
    (traverse traverser base (node-left tree)))
  (define (right base)
    (traverse traverser base (node-right tree)))
  (if (empty? tree)
      base
      (traverser (node-key tree)
                 (node-value tree)
                 left
                 right
                 base)))

(define (bbtree-traverse traverser base bbtree)
  (assert (bbtree? bbtree))
  (traverse traverser base (bbtree-tree bbtree)))

(define (bbtree-fold combine base bbtree)
  (assert (bbtree? bbtree))
  (traverse (lambda (k v l r n)
              (r (combine k v (l n))))
            base
            (bbtree-tree bbtree)))

(define (bbtree-fold-right combine base bbtree)
  (assert (bbtree? bbtree))
  (traverse (lambda (k v l r n)
              (l (combine k v (r n))))
            base
            (bbtree-tree bbtree)))

;; I could do this more efficiently, but is it worth it?
(define (bbtree-map mapper bbtree)
  (bbtree-fold (lambda (key value tree)
                 (bbtree-set tree key (mapper value)))
               (make-bbtree (bbtree-ordering-procedure bbtree))
               bbtree))

(define (alist-cons a b c)
  (cons (cons a b) c))

(define (bbtree->alist bbtree)
  (bbtree-fold-right alist-cons '() bbtree))

(define (alist->bbtree list <)
  (fold-left (lambda (tree kv-pair)
               (bbtree-set tree (car kv-pair) (cdr kv-pair)))
             (make-bbtree <)
             list))

)
