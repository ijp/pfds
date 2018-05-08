(library (test fingertrees)
  (export fingertrees)
  (import (rnrs (6))
          (chez-test suite)
          (chez-test assertions)
          (test utils)
          (rename (pfds fingertrees)
                  (make-fingertree %make-fingertree)
                  (list->fingertree %list->fingertree))
          )
  
  ;; Right now, I am not testing the monoidal parts of fingertrees, so
  ;; we use constructor that replaces these with arbitrary values
  (define (make-fingertree)
    (%make-fingertree 0 (lambda (x y) x) (lambda (x) x)))
  
  (define (list->fingertree l)
    (%list->fingertree l 0 (lambda (x y) x) (lambda (x) x)))
  
  (define (list->product-tree l)
    (%list->fingertree l 1 * values))
  
  (define (list->last-tree l)
    (define *cookie* (cons 'no 'last))
    (define (pick x y)
      (if (eq? *cookie* y)
          x
          y))
    (%list->fingertree l *cookie* pick values))
  
  (define-test-suite fingertrees
    "Tests for the fingertree implementation")
  
  (define-test-case fingertrees empty-tree ()
    (assert-predicate fingertree? (make-fingertree))
    (assert-predicate fingertree-empty? (make-fingertree)))
  
  (define-test-case fingertrees construction
    (let ((l1 '(a b c d e f))
          (l2 '((#t . f) (#t . e) (#t . d) (#t . c) (#t . b) (#t . a)))
          (l3 '((#f . a) (#f . b) (#f . c) (#f . d) (#f . e) (#f . f)))
          (l4 '((#f . b) (#f . c) (#t . a) (#f . d) (#f . e) (#f . f)))
          (l5 '((#f . e) (#t . d) (#t . c) (#t . b) (#f . f) (#t . a)))
          (make (lambda (alist)
                  (fold-left (lambda (tree pair)
                               (if (car pair)
                                   (fingertree-cons (cdr pair) tree)
                                   (fingertree-snoc tree (cdr pair))))
                             (make-fingertree)
                             alist)))
          (empty (make-fingertree)))
      (test-case construction ()
        (assert-eqv #f (fingertree-empty? (fingertree-cons #f empty)))
        (assert-eqv #f (fingertree-empty? (fingertree-snoc empty #f)))
        (assert-equal l1 (fingertree->list (make l2)))
        (assert-equal l1 (fingertree->list (make l3)))
        (assert-equal l1 (fingertree->list (make l4)))
        (assert-equal l1 (fingertree->list (make l5))))))
  
  (define-test-case fingertrees removal
    (let* ((l1 '(a b c d e f))
           (f1 (list->fingertree l1))
           (f2 (make-fingertree)))
      (test-case removal ()
        (assert-raises fingertree-empty-condition? (fingertree-uncons f2))
        (assert-raises fingertree-empty-condition? (fingertree-unsnoc f2))
        (let-values (((head tail) (fingertree-uncons f1)))
          (assert-eqv (car l1) head)
          (assert-equal (cdr l1) (fingertree->list tail)))
        (let*-values (((init last) (fingertree-unsnoc f1))
                      ((l*) (reverse l1))
                      ((l1-last) (car l*))
                      ((l1-init) (reverse (cdr l*))))
          (assert-eqv l1-last last)
          (assert-equal l1-init (fingertree->list init))))))
  
  (define-test-case fingertrees conversion
    (let ((l1 '(31 238 100 129 6 169 239 150 96 141 207 208 190 45 56
                183 199 254 78 210 14 131 10 220 205 203 125 111 42 249))
          (l2 '(25 168 21 246 39 211 60 83 103 161 192 201 31 253
                156 218 204 186 155 117)))
      (test-case conversion ()
        (assert-equal '() (fingertree->list (list->fingertree '())))
        (assert-equal l1 (fingertree->list (list->fingertree l1)))
        (assert-equal l2 (fingertree->list (list->fingertree l2))))))
  
  (define-test-case fingertrees ftree-append
    (let ((l1 '(31 238 100 129 6 169 239 150 96 141 207 208 190 45 56
                183 199 254 78 210 14 131 10 220 205 203 125 111 42 249))
          (l2 '(25 168 21 246 39 211 60 83 103 161 192 201 31 253
                156 218 204 186 155 117))
          (append* (lambda (a b)
                     (fingertree->list
                      (fingertree-append
                       (list->fingertree a)
                       (list->fingertree b))))))
      (test-case ftree-append ()
        (assert-equal (append l1 '()) (append* l1 '()))
        (assert-equal (append '() l1) (append* '() l1))
        (assert-equal (append l1 l2) (append* l1 l2))
        (assert-equal (append l1 l1) (append* l1 l1))
        (assert-equal (append l1 l2) (append* l1 l2)))))
  
  (define-test-case fingertrees monoidal-operation
    (let ((l1 '(31 238 100 129 6 169 239 150 96 141
                207 208 190 45 56 183 199 254 78 210))
          (l2 '((31 238 100 129 6) (169 239 150) (96 141 207 208 190)
                ()  (45 56 183 199) (254 78 210)))
          (car/default (lambda (dflt) (lambda (x) (if (pair? x) (car x) dflt))))
          (list->sum-tree (lambda (l1) (%list->fingertree l1 0 + values))))
      (test-case moniodal-operation ()
        (assert-equal 254 (fingertree-measure (%list->fingertree l1 0 max values)))
        (assert-equal 6 (fingertree-measure (%list->fingertree l1 1000 min values)))
        (assert-equal l1 (fingertree-measure (%list->fingertree l2 '() append values)))
        (assert-equal 595 (fingertree-measure
                         (%list->fingertree l2 0 + (car/default 0))))
        ;; sum of l1 is 4239
        (assert-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 0))
                                                             (list->sum-tree l1))))
                         (fingertree->list (fingertree-append a b))))
        (assert-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 1000))
                                                             (list->sum-tree l1))))
                         (fingertree->list (fingertree-append a b))))
        (assert-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 2000))
                                                             (list->sum-tree l1))))
                         (fingertree->list (fingertree-append a b))))
        (assert-equal l1 (let-values (((a b) (fingertree-split (lambda (x) (> x 5000))
                                                             (list->sum-tree l1))))
                         (fingertree->list (fingertree-append a b)))))))
  
  (define-test-case fingertrees fingertree-folds
    (let* ((l '(31 238 100 129 6 169 239 150 96 141
                   207 208 190 45 56 183 199 254 78 210))
           (lrev (reverse l))
           (total (apply + l))
           (ft (list->fingertree l)))
      (test-case fingertree-folds ()
        ;; empty case
        (assert-eqv #t (fingertree-fold (lambda _ #f) #t (make-fingertree)))
        (assert-eqv #t (fingertree-fold-right (lambda _ #f) #t (make-fingertree)))
        ;; associative operations
        (assert-eqv total (fingertree-fold + 0 ft))
        (assert-eqv total (fingertree-fold-right + 0 ft))
        ;; non-associative operations
        (assert-equal lrev (fingertree-fold cons '() ft))
        (assert-equal l (fingertree-fold-right cons '() ft)))))
  
  (define-test-case fingertrees reversal
    (let ((rev (lambda (l)
                 (fingertree->list
                  (fingertree-reverse (list->fingertree l)))))
          (id (lambda (l)
                (fingertree->list
                 (fingertree-reverse
                  (fingertree-reverse (list->fingertree l))))))
          (l1 '(126 6 48 86 2 119 233 92 230 160))
          (l2 '(25 168 21 246 39 211 60 83 103 161
                192 201 31 253 156 218 204 186 155 117)))
      (test-case reversal ()
        ;; behaves the same as regular reverse on lists
        (assert-eqv '() (rev '()))
        (assert-equal '(1) (rev '(1)))
        (assert-equal '(6 5 4 3 2 1) (rev '(1 2 3 4 5 6)))
        (assert-equal (reverse l1) (rev l1))
        (assert-equal (reverse l2) (rev l2))
        ;; double reversal is the the same list
        (assert-equal l1 (id l1))
        (assert-equal l2 (id l2))
        ;; a fingertree will have the same measure as its reverse if
        ;; the monoid is commutative
        (assert-equal (fingertree-measure (list->product-tree l1))
                    (fingertree-measure
                     (fingertree-reverse (list->product-tree l1))))
        ;; otherwise they are not necessarily the same
        ;; in this case, they are the same only if the first and last
        ;; elements are the same
        (assert-not
         (equal? (fingertree-measure (list->last-tree l2))
                 (fingertree-measure (fingertree-reverse (list->product-tree l2))))))))
  
)
