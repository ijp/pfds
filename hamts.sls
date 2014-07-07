#!r6rs
(library (pfds hamts)
(export make-hamt
        hamt?
        hamt-size
        hamt-ref
        hamt-set
        hamt-update
        hamt-delete
        hamt-contains?
        hamt-equivalence-predicate
        hamt-hash-function
        hamt-fold
        ;; hamt-map
        hamt->alist
        alist->hamt
        ;; make-eqv-hamt
        ;; make-equal-hamt
        ;; make-eq-hamt
        )
(import (rnrs)
        (pfds private vectors)
        (pfds private alists)
        (pfds private bitwise))

;;; Helpers

(define cardinality 32) ; 64

(define (shift key)
  (bitwise-arithmetic-shift-right key 5))

(define (mask key)
  (bitwise-and key (- (expt 2 5) 1)))

(define (level-up level)
  (+ level 5))

(define (ctpop key index)
  (bitwise-bit-count (bitwise-arithmetic-shift-right key (+ 1 index))))

;;; Node types

(define-record-type subtrie
  (fields bitmap vector))

(define-record-type leaf
  (fields key value))

(define-record-type collision
  (fields hash alist))

;;; Main

(define (lookup vector key default hash eqv?)
  (define (handle-subtrie node hash)
    (define bitmap (subtrie-bitmap node))
    (define vector (subtrie-vector node))
    (define index (mask hash))
    (if (not (bitwise-bit-set? bitmap index))
        default
        (let ((node (vector-ref vector (ctpop bitmap index))))
          (cond ((leaf? node)
                 (handle-leaf node))
                ((collision? node)
                 (handle-collision node))
                (else
                 (handle-subtrie node (shift hash)))))))

  (define (handle-leaf node)
    (if (eqv? key (leaf-key node))
        (leaf-value node)
        default))

  (define (handle-collision node)
    (alist-ref (collision-alist node) key default eqv?))
  
  (define h (hash key))
  (define node (vector-ref vector (mask h)))

  (cond ((not node) default)
        ((leaf? node) (handle-leaf node))
        ((collision? node) (handle-collision node))
        (else
         (handle-subtrie node (shift h)))))

(define (insert hvector key value hash eqv?)
  (define (handle-subtrie subtrie hash level)
    (define bitmap (subtrie-bitmap subtrie))
    (define vector (subtrie-vector subtrie))
    (define index (mask hash))
    (define (fixup node)
      (make-subtrie bitmap (vector-set vector index node)))
    (if (not (bitwise-bit-set? bitmap index))
        (make-subtrie (bitwise-bit-set bitmap index)
                      (vector-insert vector
                                     (ctpop bitmap index)
                                     (make-leaf key value)))
        (let ((node (vector-ref vector (ctpop bitmap index))))
          (cond ((leaf? node)
                 (fixup (handle-leaf node hash level)))
                ((collision? node)
                 (fixup (handle-collision node hash level)))
                (else
                 (fixup (handle-subtrie node (shift hash) (level-up level))))))))

  (define (handle-leaf node khash level)
    (define lkey  (leaf-key node))
    (define lhash (hash lkey))
    (cond ((eqv? key lkey)
           (make-leaf key value))
          ((equal? khash lhash)
           (make-collision lhash
                           (list (cons lkey (leaf-value node))
                                 (cons key value))))
          (else
           (handle-subtrie (wrap-subtrie node lhash) khash (level-up level)))))

  (define (handle-collision node hash level)
    (define chash (bitwise-arithmetic-shift-right (collision-hash node) level))
    (if (equal? hash chash)
        (make-collision (collision-hash node)
                        (alist-set (collision-alist node) key value eqv?))
        ;; TODO: there may be a better (more efficient) way to do this
        ;; but simple is better for now (see also handle-leaf)
        (handle-subtrie (wrap-subtrie node chash) hash (level-up level))))

  (define (wrap-subtrie node chash)
    (make-subtrie (bitwise-bit-set 0 (mask chash)) (vector node)))

  (define h (hash key))
  (define idx (mask h))
  (define node (vector-ref hvector idx))
  (define initial-level (level-up 0))

  (cond ((not node)
         (vector-set hvector idx (make-leaf key value)))
        ((leaf? node)
         (vector-set hvector idx (handle-leaf node (shift h) initial-level)))
        ((collision? node)
         (vector-set hvector idx (handle-collision node (shift h) initial-level)))
        (else
         (vector-set hvector idx (handle-subtrie node (shift h) initial-level)))))

(define (remove vector key hash eqv?)
  (define (handle-subtrie subtrie hash)
    (define bitmap  (subtrie-bitmap subtrie))
    (define vector  (subtrie-vector subtrie))
    (define index   (mask hash))
    (define (fixup node)
      (update bitmap vector index node))
    (if (not (bitwise-bit-set? bitmap index))
        subtrie
        (let ((node (vector-ref vector (ctpop bitmap index))))
          (cond ((leaf? node)
                 (fixup (handle-leaf node)))
                ((collision? node)
                 (fixup (handle-collision node)))
                (else
                 (fixup (handle-subtrie node (shift hash))))))))

  (define (update bitmap vector index value)
    (if value
        (make-subtrie bitmap (vector-set vector index value))
        (let ((vector* (vector-remove vector index)))
          (if (equal? '#() vector)
              #f
              (make-subtrie (bitwise-bit-unset bitmap index)
                            vector*)))))

  (define (handle-leaf node)
    (if (eqv? key (leaf-key node))
        #f
        node))

  (define (handle-collision node)
    (let ((al (alist-delete (collision-alist node) key eqv?)))
      (cond ((null? (cdr al))
             (make-leaf (car (car al)) (cdr (car al))))
            (else
             (make-collision (collision-hash node) al)))))
  
  (define h (hash key))
  (define idx (mask h))
  (define node (vector-ref vector idx))

  (cond ((not node) vector)
        ((leaf? node)
         (vector-set vector idx (handle-leaf node)))
        ((collision? node)
         (vector-set vector idx (handle-collision node)))
        (else
         (vector-set vector idx (handle-subtrie node (shift h))))))

(define (fold combine initial vector)
  ;; vector-fold is left to right
  (define (vector-fold combine initial vector)
    (define len (vector-length vector))
    (let loop ((index 0) (accum initial))
      (if (>= index len)
          accum
          (loop (+ index 1)
                (combine (vector-ref vector index) accum)))))

  (define (handle-subtrie trie accum)
    (vector-fold dispatch accum (subtrie-vector vector)))

  (define (handle-leaf leaf accum)
    (combine (leaf-key leaf) (leaf-value leaf) accum))

  (define (handle-collision collision accum)
    (fold-right (lambda (pair acc)
                  (combine (car pair) (cdr pair) acc))
                accum
                (collision-alist collision)))

  (define (dispatch val accum)
    (cond ((leaf? val)
           (handle-leaf val accum))
          ((collision? val)
           (handle-collision val accum))
          (else
           (handle-subtrie val accum))))

  (vector-fold (lambda (val accum)
                 ;; top level can have false values
                 (if (not val) accum (dispatch val accum)))
               initial
               vector))


;;; Exported Interface

(define-record-type (hamt %make-hamt hamt?)
  (fields root hash-function equivalence-predicate))

(define (wrap-root root hamt)
  (%make-hamt root
              (hamt-hash-function hamt)
              (hamt-equivalence-predicate hamt)))

(define (make-hamt hash eqv?)
  (%make-hamt (make-vector cardinality #f) hash eqv?))

(define (hamt-size hamt)
  (not-implemented-yet 'size)) ; TODO:

(define hamt-ref
  (case-lambda
    ((hamt key)
     (define token (cons #f #f))
     (define return-val (hamt-ref hamt key token))
     (when (eqv? token return-val)
       (assertion-violation 'hamt-ref "Key is not in the hamt" key))
     return-val)
    ((hamt key default)
     ;; assert hamt?
     (lookup (hamt-root hamt)
             key
             default
             (hamt-hash-function hamt)
             (hamt-equivalence-predicate hamt)))))

(define (hamt-set hamt key value)
  (define root
    (insert (hamt-root hamt)
            key
            value
            (hamt-hash-function hamt)
            (hamt-equivalence-predicate hamt)))
  (wrap-root root hamt))

(define (hamt-update hamt key proc default)
  (not-implemented-yet 'update)) ; TODO:

(define (hamt-delete hamt key)
  (define root
    (remove (hamt-root hamt)
            key
            (hamt-hash-function hamt)
            (hamt-equivalence-predicate hamt)))
  (wrap-root root hamt))

(define (hamt-contains? hamt key)
  (define token (cons #f #f))
  (if (eqv? token (hamt-ref hamt key token))
      #t
      #f))

(define (hamt-fold combine initial hamt)
  (fold combine initial (hamt-root hamt)))

(define (hamt->alist hamt)
  (hamt-fold (lambda (key value accumulator)
               (cons (cons key value) accumulator))
             '()
             hamt))

(define (alist->hamt alist hash eqv?)
  (fold-right (lambda (kv-pair hamt)
                (hamt-set hamt (car kv-pair) (cdr kv-pair)))
              (make-hamt hash eqv?)
              alist))

(define (not-implemented-yet who)
 (error 'not-implemented-yet who)) ; TODO: delete

)
