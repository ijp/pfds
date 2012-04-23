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
  (fields a b))

(define-record-type node3
  (fields a b c))

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
  ;; left and right expected to be lists of length 0 < l < 5
  (fields left middle right))

(define (ftree-case ftree empty-k single-k rib-k)
  (cond ((empty? ftree) (empty-k))
        ((single? ftree)
         (single-k (single-value ftree)))
        (else
         (rib-k (rib-left ftree)
                (rib-middle ftree)
                (rib-right ftree)))))

(define (insert-front ftree val)
  (ftree-case ftree
    (lambda ()
      (make-single val))
    (lambda (a)
      (make-rib (list val) (make-empty) (list a)))
    (lambda (l m r)
      (if (= (length l) 4)
          (make-rib (list val (car l))
                    (insert-front m (apply make-node3 (cdr l)))
                    r)
          (make-rib (cons val l) m r)))))

(define (view-front ftree empty-k cons-k)
  (ftree-case ftree
              empty-k
              (lambda (a)
                (cons-k a (make-empty)))
              (lambda (l r m)
                (cons-k (car l)
                        (rib-l (cdr l) r m)))))

(define (list->tree l)
  (fold-right (lambda (val tree)
                (insert-front tree val))
              (make-empty)
              l))

(define (rib-l l m r)
  (if (null? l)
      (view-front m
        (lambda ()
          (list->tree r))
        (lambda (x xs)
          (make-rib (node->list x)
                    xs
                    r)))
      (make-rib l m r)))

(define (remove-front ftree)
  (view-front ftree
    (lambda ()
      (error 'remove-front "can't remove from an empty tree"))
    values))

(define (insert-rear ftree val)
  (ftree-case ftree
    (lambda ()
      (make-single val))
    (lambda (a)
      (make-rib (list a) (make-empty) (list val)))
    (lambda (l m r)
      ;; TODO: should r be maintained in reverse order, rather than normal?
      (if (= (length r) 4)
          (make-rib l
                    (insert-rear m (apply make-node3 (take r 3)))
                    (list (list-ref r 3) val))
          (make-rib l m (snoc r val))))))

(define (remove-rear ftree)
  (view-rear ftree
    (lambda ()
      (error 'remove-rear "can't remove from an empty tree"))
    values))

(define (view-rear ftree empty-k snoc-k)
  (ftree-case ftree
              empty-k
              (lambda (a)
                (snoc-k (make-empty) a))
              (lambda (l r m)
                (snoc-k (rib-r l r (but-last m))
                        (last m)))))

(define (rib-r l m r)
  (if (null? r)
      (view-rear m
        (lambda ()
          (list->tree l))
        (lambda (m* r*)
          (make-rib l m* (node->list r*))))
      (make-rib l m r)))

(define (insert-front/list tree l)
  (fold-right (lambda (val tree)
                (insert-front tree val))
              tree
              l))

(define (insert-rear/list tree l)
  (fold-left (lambda (tree val)
                (insert-rear tree val))
              tree
              l))

(define (app3 ftree1 ts ftree2)
  (cond ((empty? ftree1)
         (insert-front/list ftree2 ts))
        ((empty? ftree2)
         (insert-rear/list ftree1 ts))
        ((single? ftree1)
         (insert-front (insert-front/list ftree2 ts)
                       (single-value ftree1)))
        ((single? ftree2)
         (insert-rear (insert-rear/list ftree1 ts)
                      (single-value ftree2)))
        (else
         (let ((l1 (rib-left ftree1))
               (m1 (rib-middle ftree1))
               (r1 (rib-right ftree1))
               (l2 (rib-left ftree2))
               (m2 (rib-middle ftree2))
               (r2 (rib-right ftree2)))
           (make-rib l1
                     (app3 m1
                           (nodes (append r1 ts l2))
                           m2)
                     r2)))))

(define (nodes lst)
  ;; *sigh*
  (let ((a (car lst))
        (b (cadr lst)))
    (cond ((null? (cddr lst))
           (list (make-node2 a b)))
          ((null? (cdddr lst))
           (list (make-node3 a b (caddr lst))))
          ((null? (cddddr lst))
           (list (make-node2 a b)
                 (make-node2 (caddr lst) (cadddr lst))))
          (else
           (cons (make-node3 a b (caddr lst))
                 (nodes (cdddr lst)))))))

;; exported interface

(define-record-type (fingertree %make-fingertree fingertree?)
  (fields tree))

(define (%wrap fingertree tree)
  (%make-fingertree tree))

(define (make-fingertree)
  (%make-fingertree (make-empty)))

(define (fingertree-cons a fingertree)
  ;; TODO: should it obey normal cons interface, or have fingertree
  ;; first?
  (%wrap fingertree
         (insert-front (fingertree-tree fingertree) a)))

(define (fingertree-snoc fingertree a)
  (%wrap fingertree
         (insert-rear (fingertree-tree fingertree) a)))

(define (fingertree-uncons fingertree)
  (call-with-values
      (lambda ()
        (remove-front (fingertree-tree fingertree)))
    (lambda (val rest)
      (values val
              (%wrap fingertree rest)))))

(define (fingertree-unsnoc fingertree)
  (call-with-values
      (lambda ()
        (remove-rear (fingertree-tree fingertree)))
    (lambda (rest val)
      (values val
              (%wrap fingertree rest)))))

(define (fingertree-empty? fingertree)
  (empty? (fingertree-tree fingertree)))

(define (fingertree-append fingertree1 fingertree2)
  (%wrap fingertree1
         (app3 (fingertree-tree fingertree1)
               '()
               (fingertree-tree fingertree2))))

(define (list->fingertree l )
  (%make-fingertree (list->tree l)))

;; should be implemented in terms of a fingertree-fold-right, but later
(define (fingertree->list t)
  (if (fingertree-empty? t)
      '()
      (call-with-values
          (lambda ()
            (fingertree-uncons t))
        (lambda (a t*)
          (cons a (fingertree->list t*))))))

)
