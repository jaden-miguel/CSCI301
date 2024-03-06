#lang racket

; checks if x is contained in the List L
(define (member? x L) 
   (if (null? L)
      #f
      (if (eqv? (car L) x)
          #t
          (member? x (cdr L))
       )
    )
 )
; checks if a list is within a given list
(define (subset? L1 L2)
   (if (null? L1)
      #t
      (if (and (member? (car L1) L2) (equal? (length L1) 1))
          #t
          (if (member? (car L1) L2)
              (subset? (cdr L1) L2)
              #f
          )
       )
    )
 )
; checks if two lists are equal are not (if they both contain eachother)
(define (list-equal? L1 L2)
  (if (and (subset? L1 L2) (subset? L2 L1))
      #t
      #f
   )
 )
; checks if a given set exists within another set. 
(define (subset-exists? Sub List)
  (if (null? List)
      #f
      (if (list? (car List))
          (if (equal? Sub (car List))
              #t
              (subset-exists? Sub (cdr List))
           )
          (subset-exists? Sub (cdr List))
       )
  )
 )
; remove an item from a list
(define (remove x list)
  (if (null? list)
      '()
      (if (eqv? x (car list))
          (remove x (cdr list))
          (cons (car list)(remove x (cdr list)))
       )
   )
)
; adds an x into a list
(define (add-to x List)
  (append List (list x))
 )
; adds x into a list N times
(define (add-many x List N)
  (if (> N 0)
      (add-many x (append List (list x)) (- N 1))
      List
   )
 )
; remove first found element from list/bag
(define (remove-first x Bag)
  (if (null? Bag)
      '()
      (if (eqv? (car Bag) x)
          (cdr Bag)
          (cons (car Bag) (remove-first x (cdr Bag)))
       )
   )
 )
; element count helper
(define (x-count x Bag n)
  (if (null? Bag)
      n
      (if (eqv? x (car Bag))
          (x-count x (cdr Bag) (+ n 1))
          (x-count x (cdr Bag) n)
       )
   )
 )
; bag-difference
(define (bag-difference bag1 bag2)
  (if (null? bag2)
      bag1
      (bag-difference (remove-first (car bag2) bag1) (cdr bag2))
   )
 )
; bag union helper function
(define (bag-union-helper bag1 bag2 E)
  (if (null? bag2)
      (if (null? bag1)
          E
          bag1
       )
      (if (null? bag1)
          (if (>= (x-count (car bag2) E 0) (x-count (car bag2) bag2 0))
              (bag-union-helper bag1 (cdr bag2) E)
              (bag-union-helper bag1 bag2 (add-to (car bag2) E))
          )
          (bag-union-helper (cdr bag1) bag2 (add-to (car bag1) E) )
      )
   )
 )
; bag union
(define (bag-union bag1 bag2)
  (bag-union-helper bag1 bag2 '())
 )
; bag intersection helper function
(define (bag-intersection-helper bag1 bag2 E)
  (if (null? bag1)
      E
      (if (<= (x-count (car bag1) bag1 0) (x-count (car bag1) bag2 0))
          (bag-intersection-helper (remove (car bag1) bag1) (remove (car bag1) bag2) (add-many (car bag1) E (x-count (car bag1) bag1 0)))
          (bag-intersection-helper (remove (car bag1) bag1) (remove (car bag1) bag2) (add-many (car bag1) E (x-count (car bag1) bag2 0)))
      )
   )
 )
; bag intersection
(define (bag-intersection bag1 bag2)
  (if (null? bag1)
      '()
      (if (null? bag2)
          '()
          (bag-intersection-helper bag1 bag2 '())
       )
   )
 )