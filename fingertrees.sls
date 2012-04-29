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
        fingertree-split
        fingertree-split3
        fingertree-fold
        fingertree-fold-right
        fingertree-reverse
        )
(import (rnrs)
        (pfds private reader-monad))

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

(define (map-reverse f l)
  (fold-left (lambda (o n) (cons (f n) o)) '() l))

;;; Node type

(define-record-type node2
  (protocol
   (lambda (new)
     (lambda (a b)
       (mlet ((app (asks mappend))
              (a-measure (measure-nodetree a))
              (b-measure (measure-nodetree b)))
         (return (new (app a-measure b-measure) a b))))))
  (fields measure a b))

(define-record-type node3
  (protocol
   (lambda (new)
     (lambda (a b c)
       (mlet ((app (asks mappend))
              (a-measure (measure-nodetree a))
              (b-measure (measure-nodetree b))
              (c-measure (measure-nodetree c)))
         (return (new (app (app a-measure b-measure) c-measure) a b c))))))
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

(define (nodetree-fold-right f base nodetree)
  (define (foldr node base)
    (cond ((node2? node)
           (foldr (node2-a node)
                  (foldr (node2-b node) base)))
          ((node3? node)
           (foldr (node3-a node)
                  (foldr (node3-b node)
                         (foldr (node3-c node) base))))
          (else (f node base))))
  (foldr nodetree base))

(define (nodetree-fold-left f base nodetree)
  (define (foldl node base)
    (cond ((node2? node)
           (foldl (node2-b node)
                  (foldl (node2-a node) base)))
          ((node3? node)
           (foldl (node3-c node)
                  (foldl (node3-b node)
                         (foldl (node3-a node) base))))
          (else (f node base))))
  (foldl nodetree base))

;;; Tree type

(define-record-type empty
  (protocol
   (lambda (new)
     (lambda ()
       (return (new))))))

(define-record-type single
  (protocol
   (lambda (new)
     (lambda (val)
       (return (new val)))))
  (fields value))

(define-record-type rib
  (protocol
   (lambda (new)
     (lambda (left middle right)
       (mlet ((app (asks mappend))
              (left* (measure-digit left))
              (middle* (measure-ftree middle))
              (right* (measure-digit right)))
         (return
          (new (app (app left* middle*) right*)
               left
               middle
               right))))))
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

(define (digits-fold-right f b d)
  (fold-right (lambda (ntree base)
                (nodetree-fold-right f base ntree))
              b
              d))

(define (digits-fold-left f b d)
  (fold-left (lambda (base ntree)
                (nodetree-fold-left f base ntree))
              b
              d))

(define (ftree-fold-right proc base ftree)
  (ftree-case ftree
    (lambda () base)
    (lambda (x) (nodetree-fold-right proc base x))
    (lambda (l x r)
      (define base* (digits-fold-right proc base r))
      (define base** (ftree-fold-right proc base* x))
      (digits-fold-right proc base** l))))

(define (ftree-fold-left proc base ftree)
  (ftree-case ftree
    (lambda () base)
    (lambda (x) (nodetree-fold-left proc base x))
    (lambda (l x r)
      (define base* (digits-fold-left proc base l))
      (define base** (ftree-fold-left proc base* x))
      (digits-fold-left proc base** r))))

(define (insert-front ftree val)
  (ftree-case ftree
    (lambda ()
      (make-single val))
    (lambda (a)
      (mlet ((m (make-empty)))
        (make-rib (list val) m (list a))))
    (lambda (l m r)
      (if (= (length l) 4)
          (mlet ((node (apply make-node3 (cdr l)))
                 (m* (insert-front m node)))
            (make-rib (list val (car l)) m* r))
          (make-rib (cons val l) m r)))))

(define (view-front ftree empty-k cons-k)
  (ftree-case ftree
              empty-k
              (lambda (a)
                (mlet ((empty (make-empty)))
                  (cons-k a empty)))
              (lambda (l r m)
                (mlet ((rest (rib-l (cdr l) r m)))
                  (cons-k (car l) rest)))))

(define (list->tree l)
  (fold-right (lambda (val tree)
                (mlet ((tree* tree))
                  (insert-front tree* val)))
              (make-empty)
              l))

(define (rib-l l m r)
  (if (null? l)
      (view-front m
        (lambda ()
          (list->tree r))
        (lambda (x xs)
          (make-rib (node->list x) xs r)))
      (make-rib l m r)))

(define (remove-front ftree)
  (view-front ftree
    (lambda ()
      (error 'remove-front "can't remove from an empty tree"))
    return-values))

(define (insert-rear ftree val)
  (ftree-case ftree
    (lambda ()
      (make-single val))
    (lambda (a)
      (mlet ((m (make-empty)))
        (make-rib (list a) m (list val))))
    (lambda (l m r)
      ;; TODO: should r be maintained in reverse order, rather than
      ;; normal?
      ;; yes! it will make concatenation slightly slower, but will
      ;; speed up inserts and removals
      (if (= (length r) 4)
          (mlet ((node (apply make-node3 (take r 3)))
                 (m* (insert-rear m node)))
            (make-rib l m* (list (list-ref r 3) val)))
          (make-rib l m (snoc r val))))))

(define (remove-rear ftree)
  (view-rear ftree
    (lambda ()
      (error 'remove-rear "can't remove from an empty tree"))
    return-values))

(define (view-rear ftree empty-k snoc-k)
  (ftree-case ftree
              empty-k
              (lambda (a)
                (mlet ((empty (make-empty)))
                  (snoc-k empty a)))
              (lambda (l r m)
                (mlet ((init (rib-r l r (but-last m))))
                  (snoc-k init (last m))))))

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
                (mlet ((tree* tree))
                  (insert-front tree* val)))
              (return tree)
              l))

(define (insert-rear/list tree l)
  (fold-left (lambda (tree val)
               (mlet ((tree* tree))
                 (insert-rear tree* val)))
             (return tree)
              l))

(define (app3 ftree1 ts ftree2)
  (cond ((empty? ftree1)
         (insert-front/list ftree2 ts))
        ((empty? ftree2)
         (insert-rear/list ftree1 ts))
        ((single? ftree1)
         (mlet ((right (insert-front/list ftree2 ts)))
           (insert-front right (single-value ftree1))))
        ((single? ftree2)
         (mlet ((left (insert-rear/list ftree1 ts)))
           (insert-rear left (single-value ftree2))))
        (else
         (let ((l1 (rib-left ftree1))
               (m1 (rib-middle ftree1))
               (r1 (rib-right ftree1))
               (l2 (rib-left ftree2))
               (m2 (rib-middle ftree2))
               (r2 (rib-right ftree2)))
           (mlet ((m* (nodes (append r1 ts l2)))
                  (new-middle (app3 m1 m* m2)))
             (make-rib l1 new-middle r2))))))

(define (nodes lst)
  ;; *sigh*
  (let ((a (car lst))
        (b (cadr lst)))
    (cond ((null? (cddr lst))
           (fmap list (make-node2 a b)))
          ((null? (cdddr lst))
           (fmap list (make-node3 a b (caddr lst))))
          ((null? (cddddr lst))
           (mlet ((first (make-node2 a b))
                  (second (make-node2 (caddr lst) (cadddr lst))))
             (return (list first second))))
          (else
           (mlet ((head (make-node3 a b (caddr lst)))
                  (tail (nodes (cdddr lst))))
             (return (cons head tail)))))))

(define (reverse-tree tree)
  (ftree-case tree
    (lambda () (make-empty))
    (lambda (x) (mlet ((nodes (reverse-nodetree x)))
             (make-single nodes)))
    (lambda (l x r)
      (mlet ((r* (reverse-digit r))
             (x* (reverse-tree x))
             (l* (reverse-digit l)))
        (make-rib r* x* l*)))))

(define (reverse-digit l)
  (mapM reverse-nodetree (reverse l)))

(define (reverse-nodetree l)
  (cond ((node2? l)
         (mlet ((a (reverse-nodetree (node2-a l)))
                (b (reverse-nodetree (node2-b l))))
           (make-node2 b a)))
        ((node3? l)
         (mlet ((a (reverse-nodetree (node3-a l)))
                (b (reverse-nodetree (node3-b l)))
                (c (reverse-nodetree (node3-c l))))
           (make-node3 c b a)))
        (else (return l))))

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

(define (measure-digit obj)
  (mlet ((mempty (asks mempty))
         (mappend (asks mappend)))
    (fold-left (lambda (i a)
                 (mlet ((a* (measure-nodetree a))
                        (i* i))
                   (return (mappend i* a*))))
               (return mempty)
               obj)))

(define (measure-ftree obj)
  (cond ((empty? obj)
         (asks mempty))
        ((single? obj)
         (measure-nodetree (single-value obj)))
        (else
         (return (rib-measure obj)))))

(define (measure-nodetree obj)
  (cond ((node2? obj) (return (node2-measure obj)))
        ((node3? obj) (return (node3-measure obj)))
        (else
         (>>= (asks mconvert)
              (lambda (convert)
                (return (convert obj)))))))

(define (split proc tree)
  (if (empty? tree)
      (mlet ((empty (make-empty)))
        (return-values empty empty))
      (mlet ((measure (measure-ftree tree)))
        (if (proc measure)
            (mlet ((mempty (asks mempty))
                   ((l x r) (split-tree proc mempty tree))
                   (rest (insert-front r x)))
              (return-values l rest))
            (mlet ((empty (make-empty)))
              (return-values tree empty))))))

(define (split-tree proc i tree)
  (ftree-case tree
    (lambda ()
      (error 'split-tree "shouldn't happen?"))
    (lambda (a)
      (mlet ((empty (make-empty)))
        (return-values empty a empty)))
    (lambda (l m r)
      (mlet ((app (asks mappend))
             (measure-l (measure-digit l))
             (measure-m (measure-ftree m)))
        (let* ((vpr (app i measure-l))
               (vm  (app vpr measure-m)))
          (cond ((proc vpr)
                 (mlet (((l* x* r*) (split-digit proc i l))
                        (l** (list->tree l*))
                        (r** (rib-l r* m r)))
                   (return-values l** x* r**)))
                ((proc vm)
                 (mlet (((ml xs mr) (split-tree proc vpr m))
                        (ml-measure (measure-ftree ml))
                        ((l* x* r*)
                         (split-digit proc (app vpr ml-measure) (node->list xs)))
                        (l** (rib-r l ml l*))
                        (r**  (rib-l r* mr r)))
                   (return-values l** x* r**)))
                (else
                 (mlet (((l* x* r*) (split-digit proc vm r))
                        (l** (rib-r l m l*))
                        (r** (list->tree r*)))
                   (return-values l** x* r**)))))))))

(define (split-digit proc i xs)
  (if (null? (cdr xs))
      (return-values '() (car xs) '())
      (mlet ((app (asks mappend))
             (car-measure (measure-nodetree (car xs))))
        (let ((i* (app i car-measure)))
          (if (proc i*)
              (return-values '() (car xs) (cdr xs))
              (mlet (((l x r) (split-digit proc i* (cdr xs))))
                (return-values (cons (car xs) l) x r)))))))

;; exported interface

(define-record-type (fingertree %make-fingertree fingertree?)
  (fields tree monoid))

(define (%wrap fingertree tree)
  (%make-fingertree tree
                    (fingertree-monoid fingertree)))

(define (make-fingertree id append convert)
  (define monoid (make-monoid* id append convert))
  (%make-fingertree (run-reader (make-empty) monoid)
                    monoid))

(define (fingertree-cons a fingertree)
  ;; TODO: should it obey normal cons interface, or have fingertree
  ;; first?
  (%wrap fingertree
         (run-reader (insert-front (fingertree-tree fingertree) a)
                     (fingertree-monoid fingertree))))

(define (fingertree-snoc fingertree a)
  (%wrap fingertree
         (run-reader (insert-rear (fingertree-tree fingertree) a)
                     (fingertree-monoid fingertree))))

(define (fingertree-uncons fingertree)
  (define monoid (fingertree-monoid fingertree))
  (call-with-values
      (lambda ()
        (run-reader (remove-front (fingertree-tree fingertree))
                    monoid))
    (lambda (val rest)
      (values val (%wrap fingertree (run-reader rest monoid))))))

(define (fingertree-unsnoc fingertree)
  (define monoid (fingertree-monoid fingertree))
  (call-with-values
      (lambda ()
        (run-reader (remove-rear (fingertree-tree fingertree))
                    monoid))
    (lambda (rest val)
      (values val (%wrap fingertree (run-reader rest monoid))))))

(define (fingertree-empty? fingertree)
  (empty? (fingertree-tree fingertree)))

(define (fingertree-append fingertree1 fingertree2)
  (%wrap fingertree1
         (run-reader 
          (app3 (fingertree-tree fingertree1)
                '()
                (fingertree-tree fingertree2))
          (fingertree-monoid fingertree1))))

;; TODO: fix this
(define (list->fingertree l id append convert)
  (define monoid (make-monoid* id append convert))
  (%make-fingertree (run-reader (list->tree l) monoid) monoid))

(define (fingertree->list t)
  (fingertree-fold-right cons '() t))

(define (fingertree-measure fingertree)
  (run-reader (measure-ftree (fingertree-tree fingertree))
              (fingertree-monoid fingertree)))

(define (fingertree-split p fingertree)
  (call-with-values
      (lambda ()
        (run-reader
         (split p (fingertree-tree fingertree))
         (fingertree-monoid fingertree)))
    (lambda (a b)
      (values (%wrap fingertree a)
              (%wrap fingertree b)))))

(define (fingertree-split3 p fingertree)
  (call-with-values
      (lambda ()
        (define monoid (fingertree-monoid fingertree))
        (run-reader
         (split-tree p (mempty monoid) (fingertree-tree fingertree))
         monoid))
    (lambda (a b c)
      (values (%wrap fingertree a)
              b
              (%wrap fingertree c)))))

(define (fingertree-fold f b fingertree)
  (ftree-fold-left f b (fingertree-tree fingertree)))

(define (fingertree-fold-right f b fingertree)
  (ftree-fold-right f b (fingertree-tree fingertree)))

(define (fingertree-reverse fingertree)
  (%wrap fingertree
         (run-reader (reverse-tree (fingertree-tree fingertree))
                     (fingertree-monoid fingertree)))))
