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
(library (pfds bbtrees)
(export make-bbtree
        bbtree?
        bbtree-size
        bbtree-ref
        bbtree-set
        bbtree-delete
        bbtree-contains?
        )

(import (rnrs))

(define (make-bbtree <) #f)
(define (bbtree? object) #f)
(define (bbtree-size bbtree) #f)
(define (bbtree-ref bbtree key) #f)
(define (bbtree-set bbtree key value) #f)
(define (bbtree-delete bbtree key) #f)
(define (bbtree-contains? bbtree key) #f)

)
