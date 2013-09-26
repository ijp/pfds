;; One advantage of dlists is that they allow you to write more
;; efficient programs, while keeping the lucidity of the less
;; efficient version. Take the naïve version of 'reverse'

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l))
              (list (car l)))))

;; The definition is obviously correct, however it isn't very
;; efficient. For a given step, the cost of the non-trivial case is
;; dependant on the size of the list we have gotten from the recursive
;; call. That is, it takes time proportional to the square of its
;; input list.
;; Of course, no self respecting functional programmer would write
;; reverse in this manner, as the trick of using an accumulating
;; parameter is so well established. Instead we would write

(define (reverse l)
  (define (reverse-helper from to)
    (if (null? from)
        to
        (reverse-helper (cdr from)
                        (cons (car from) to))))
  (reverse-helper l '()))

;; By introducing this additional parameter, we have reclaimed a more
;; reasonable complexity of constant time at each recursive call,
;; giving us linear complexity overall.
;; This is a big improvement, and with a little practice, it becomes
;; easy to convince yourself of the correctness of code written in
;; this manner.

;; However, why should you have to practice? Why can't there be a
;; definition as obviously correct as the former, with the efficiency
;; of the latter?
;; Turns out, it is possible to do this, by using a different
;; representation for lists.

(define (reverse* l)
  (if (null? l)
      (dlist)
      (dlist-append (reverse* (cdr l))
                    (dlist (car l)))))

(define (reverse l)
  (dlist->list (reverse* l)))

;; Difference lists, or representing lists as functions, gives us a
;; constant time version of append, thus reducing the complexity of
;; reverse* to O(n), and the definition differs from the original,
;; only in the names we use for the append and list procedures. The
;; final result of this function, however, is a dlist rather than a
;; list, so we must convert back. This also has linear complexity, so
;; the overall complexity is still linear.

;; How does this work? Well, let's replace dlist and dlist-append with
;; their definitions
(define (reverse* l)
  (if (null? l)
      (lambda (x) (append '() x))
      (compose (reverse* (cdr l))
               (lambda (x) (append (list (car l)) x)))))

(define (reverse l)
  ((reverse* l) '()))

;; Now, we replace compose with its definition
(define (reverse* l)
  (if (null? l)
      (lambda (x) (append '() x))
      (lambda (x)
        ((reverse* (cdr l))
         ((lambda (x) (append (list (car l)) x)) x)))))

(define (reverse l)
  ((reverse* l) '()))

;; With a few simplifications: substituting x for its definition,
;; x for (append '() x), and (cons x y) for (append (list x) y)
(define (reverse* l)
  (if (null? l)
      (lambda (x) x)
      (lambda (x)
        ((reverse* (cdr l))
         (cons (car l) x)))))

(define (reverse l)
  ((reverse* l) '()))

;; Now, if we uncurry reverse*
(define (reverse* l x)
  (if (null? l)
      x
      (reverse* (cdr l) (cons (car l) x))))

(define (reverse l)
  (reverse* l '()))

;; Then, it turns out the dlist version is the traditional O(n)
;; implementation in disguise.

;; As an exercise, you can try doing the same thing for the flatten
;; function
(define (flatten xs)
  (cond ((null? xs) '())
        ((pair? xs)
         (append (flatten (car xs))
                 (flatten (cdr xs))))
        (else (list xs))))
