#!r6rs
(library (pfds private reader-monad)
(export return return-values >>= >>=* mlet ask asks run-reader fmap mapM)
(import (rnrs))

(define (return x)
  (lambda (env) x))

(define (>>= m f)
  (lambda (env)
    ((f (m env)) env)))

(define (return-values . args)
  (lambda (env)
    (apply values args)))

(define (>>=* m f)
  (lambda (env)
    ((call-with-values (lambda () (m env)) f)
     env)))

(define-syntax mlet
  (syntax-rules ()
    ((mlet () body bodies ...)
     (begin body bodies ...))
    ((mlet (((bound-vars ...) val) (vars vals) ...) body bodies ...)
     (>>=* val
           (lambda (bound-vars ...)
             (mlet ((vars vals) ...) body bodies ...))))
    ((mlet ((var val) (vars vals) ...) body bodies ...)
     (>>= val
          (lambda (var)
            (mlet ((vars vals) ...) body bodies ...))))))

(define (ask env) env)

(define (asks f)
  (lambda (env)
    (f env)))

(define (run-reader reader env)
  (reader env))

(define (fmap f m)
  (>>= m (lambda (x) (return (f x)))))

(define (sequence l)
  (if (null? l)
      (return '())
      (mlet ((head (car l))
             (tail (sequence (cdr l))))
        (return (cons head tail)))))

(define (mapM f l)
  (sequence (map f l)))

)
