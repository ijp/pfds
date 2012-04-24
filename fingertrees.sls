;; Fingertrees are a useful base on which to build other data structures
#!r6rs
(library (pfds fingertrees)
(export fingertree?
        fingertree-empty?
        make-fingertree
        fingertree-cons
        fingertree-snoc
        fingertree-uncons
        fingertree-unsnoc
        fingertree-append
        list->fingertree
        fingertree->list
        fingertree-measure
        )
(import (rnrs))

;;; List helpers

(define (snoc l val)
  (append l (list val)))

(define (take l n)
  (if (or (null? l) (zero? n))
      '()
      (cons (car l)
            (take (cdr l) (- n 1)))))

(define (last list)
  (if (null? (cdr list))
      (car list)
      (last (cdr list))))

(define (but-last list)
  (if (null? (cdr list))
      '()
      (cons (car list)
            (but-last (cdr list)))))

;;; Node type

(define-record-type node2
  (protocol
   (lambda (new)
     (lambda (monoid a b)
       (define app (mappend monoid))
       (new (app (measure a monoid)
                 (measure b monoid))
            a
            b))))
  (fields measure a b))

(define-record-type node3
  (protocol
   (lambda (new)
     (lambda (monoid a b c)
       (define app (mappend monoid))
       (new (app (app (measure a monoid)
                      (measure b monoid))
                 (measure c monoid))
            a
            b
            c))))
  (fields measure a b c))

(define (node-case node k2 k3)
  (if (node2? node)
      (k2 (node2-a node) (node2-b node))
      (k3 (node3-a node) (node3-b node) (node3-c node))))

(define (node-fold-right f base node)
  (node-case node
     (lambda (a b)
       (f a (f b base)))
     (lambda (a b c)
       (f a (f b (f c base))))))

(define (node->list node)
  (node-fold-right cons '() node))

;;; Tree type

(define-record-type empty)

(define-record-type single
  (fields value))

(define-record-type rib
  (protocol
   (lambda (new)
     (lambda (monoid left middle right)
       (define app (mappend monoid))
       (new (app (app (measure left monoid)
                      (measure middle monoid))
                 (measure right monoid))
            left
            middle
            right))))
  ;; left and right expected to be lists of length 0 < l < 5
  (fields measure left middle right))

(define (ftree-case ftree empty-k single-k rib-k)
  (cond ((empty? ftree) (empty-k))
        ((single? ftree)
         (single-k (single-value ftree)))
        (else
         (rib-k (rib-left ftree)
                (rib-middle ftree)
                (rib-right ftree)))))

(define (insert-front ftree val monoid)
  (ftree-case ftree
    (lambda ()
      (make-single val))
    (lambda (a)
      (make-rib monoid (list val) (make-empty) (list a)))
    (lambda (l m r)
      (if (= (length l) 4)
          (make-rib monoid
                    (list val (car l))
                    (insert-front m (apply make-node3 monoid (cdr l)) monoid)
                    r)
          (make-rib monoid (cons val l) m r)))))

(define (view-front ftree empty-k cons-k monoid)
  (ftree-case ftree
              empty-k
              (lambda (a)
                (cons-k a (make-empty)))
              (lambda (l r m)
                (cons-k (car l)
                        (rib-l (cdr l) r m monoid)))))

(define (list->tree l monoid)
  (fold-right (lambda (val tree)
                (insert-front tree val monoid))
              (make-empty)
              l))

(define (rib-l l m r monoid)
  (if (null? l)
      (view-front m
        (lambda ()
          (list->tree r monoid))
        (lambda (x xs)
          (make-rib monoid
                    (node->list x)
                    xs
                    r))
        monoid)
      (make-rib monoid l m r)))

(define (remove-front ftree monoid)
  (view-front ftree
    (lambda ()
      (error 'remove-front "can't remove from an empty tree"))
    values
    monoid))

(define (insert-rear ftree val monoid)
  (ftree-case ftree
    (lambda ()
      (make-single val))
    (lambda (a)
      (make-rib monoid (list a) (make-empty) (list val)))
    (lambda (l m r)
      ;; TODO: should r be maintained in reverse order, rather than
      ;; normal?
      ;; yes! it will make concatenation slightly slower, but will
      ;; speed up inserts and removals
      (if (= (length r) 4)
          (make-rib monoid
                    l
                    (insert-rear m (apply make-node3 monoid (take r 3)) monoid)
                    (list (list-ref r 3) val))
          (make-rib monoid l m (snoc r val))))))

(define (remove-rear ftree monoid)
  (view-rear ftree
    (lambda ()
      (error 'remove-rear "can't remove from an empty tree"))
    values
    monoid))

(define (view-rear ftree empty-k snoc-k monoid)
  (ftree-case ftree
              empty-k
              (lambda (a)
                (snoc-k (make-empty) a))
              (lambda (l r m)
                (snoc-k (rib-r l r (but-last m) monoid)
                        (last m)))))

(define (rib-r l m r monoid)
  (if (null? r)
      (view-rear m
        (lambda ()
          (list->tree l monoid))
        (lambda (m* r*)
          (make-rib monoid l m* (node->list r*)))
        monoid)
      (make-rib monoid l m r)))

(define (insert-front/list tree l monoid)
  (fold-right (lambda (val tree)
                (insert-front tree val monoid))
              tree
              l))

(define (insert-rear/list tree l monoid)
  (fold-left (lambda (tree val)
                (insert-rear tree val monoid))
              tree
              l))

(define (app3 ftree1 ts ftree2 monoid)
  (cond ((empty? ftree1)
         (insert-front/list ftree2 ts monoid))
        ((empty? ftree2)
         (insert-rear/list ftree1 ts monoid))
        ((single? ftree1)
         (insert-front (insert-front/list ftree2 ts monoid)
                       (single-value ftree1)
                       monoid))
        ((single? ftree2)
         (insert-rear (insert-rear/list ftree1 ts monoid)
                      (single-value ftree2)
                      monoid))
        (else
         (let ((l1 (rib-left ftree1))
               (m1 (rib-middle ftree1))
               (r1 (rib-right ftree1))
               (l2 (rib-left ftree2))
               (m2 (rib-middle ftree2))
               (r2 (rib-right ftree2)))
           (make-rib monoid
                     l1
                     (app3 m1
                           (nodes (append r1 ts l2) monoid)
                           m2
                           monoid)
                     r2)))))

(define (nodes lst monoid)
  ;; *sigh*
  (let ((a (car lst))
        (b (cadr lst)))
    (cond ((null? (cddr lst))
           (list (make-node2 monoid a b)))
          ((null? (cdddr lst))
           (list (make-node3 monoid a b (caddr lst))))
          ((null? (cddddr lst))
           (list (make-node2 monoid a b)
                 (make-node2 monoid (caddr lst) (cadddr lst))))
          (else
           (cons (make-node3 monoid a b (caddr lst))
                 (nodes (cdddr lst) monoid))))))

;; generalising fingertrees with monoids

;; I think I'm going to need a "configuration" type and pass it around
;; in order to generalize over arbitrary monoids
;; call the type iMeasured or something

(define-record-type monoid*
  ;; a monoid, but augmented with a procedure to convert objects into the
  ;; monoid type
  (fields (immutable empty mempty)
          (immutable append mappend)
          (immutable convert mconvert)))

(define (measure obj monoid)
  (cond ((node2? obj) (node2-measure obj))
        ((node3? obj) (node3-measure obj))
        ((empty? obj) (mempty monoid))
        ((single? obj) (measure (single-value obj) monoid))
        ((rib? obj) (rib-measure obj))
        ;; digits currently represented by lists
        ;; not a good idea, since it may interfere with lists in the sequence
        ((list? obj)
         (fold-left (lambda (i a)
                      ((mappend monoid) i (measure a monoid)))
                    (mempty monoid)
                    obj))
        (else
         ((mconvert monoid) obj)
         ;;(error 'measure "measure not supported for this type")
         )))

;; exported interface

(define-record-type (fingertree %make-fingertree fingertree?)
  (fields tree monoid))

(define (%wrap fingertree tree)
  (%make-fingertree tree
                    (fingertree-monoid fingertree)))

(define (make-fingertree id append convert)
  (%make-fingertree (make-empty)
                    (make-monoid* id append convert)))

(define (fingertree-cons a fingertree)
  ;; TODO: should it obey normal cons interface, or have fingertree
  ;; first?
  (%wrap fingertree
         (insert-front (fingertree-tree fingertree)
                       a
                       (fingertree-monoid fingertree))))

(define (fingertree-snoc fingertree a)
  (%wrap fingertree
         (insert-rear (fingertree-tree fingertree)
                      a
                      (fingertree-monoid fingertree))))

(define (fingertree-uncons fingertree)
  (call-with-values
      (lambda ()
        (remove-front (fingertree-tree fingertree)
                      (fingertree-monoid fingertree)))
    (lambda (val rest)
      (values val
              (%wrap fingertree rest)))))

(define (fingertree-unsnoc fingertree)
  (call-with-values
      (lambda ()
        (remove-rear (fingertree-tree fingertree)
                     (fingertree-monoid fingertree)))
    (lambda (rest val)
      (values val
              (%wrap fingertree rest)))))

(define (fingertree-empty? fingertree)
  (empty? (fingertree-tree fingertree)))

(define (fingertree-append fingertree1 fingertree2)
  (%wrap fingertree1
         (app3 (fingertree-tree fingertree1)
               '()
               (fingertree-tree fingertree2)
               (fingertree-monoid fingertree1))))

;; TODO: fix this
(define (list->fingertree l id append convert)
  (define monoid (make-monoid* id append convert))
  (%make-fingertree (list->tree l monoid) monoid))

;; should be implemented in terms of a fingertree-fold-right, but later
(define (fingertree->list t)
  (if (fingertree-empty? t)
      '()
      (call-with-values
          (lambda ()
            (fingertree-uncons t))
        (lambda (a t*)
          (cons a (fingertree->list t*))))))

(define (fingertree-measure fingertree)
  (measure (fingertree-tree fingertree)
           (fingertree-monoid fingertree)))

)
