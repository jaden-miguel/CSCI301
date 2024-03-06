#lang racket

; Name: Jaden Ji Miguel
; Date: Spring 2022
; Purpose: CSCI301 lab03.rkt


; function checks if a single element is in a list L
(define member?
  (lambda (x L)
    (if (null? L) 
        #f
        (or (equal? x (car L)) (member? x (cdr L))))))

; function to check subset. if list empty, return true
; tests whether L1 ⊆ L2. L1 is a subset of L2 if
; every element of L1 is also a member of L2.
(define (subset? L1 L2)
  (cond
    ((null? L1) #t)
    (else (if (not (member? (first L1) L2)) #f (subset? (rest L1) L2)))))


; function to check two sets are equal
; if the lengths are not equal return false
; if the lengths are equal and the one is subset of the other, then they are equal
(define (set-equal? L1 L2)
  (cond
    ((not (eq? (length L1) (length L2))) #f)
    (else (if (subset? L1 L2) #t #f))))


; function to calculate union of two sets
; if first set null, return the 2nd set
; the union of two sets is the set of all
; elements that appear in either set (with no repetitions)
(define (union S1 S2)
  (cond ((null? S1) S2)
        ((member? (first S1) S2)
         (union (rest S1) S2))
        (else
         (cons (first S1) (union (rest S1) S2)))))

; function to calculate the intersection of two sets
; if the first set is null return empty set
; if the first element of first set is member of second set append it to the output
; call the function with rest of the first set
(define (intersect S1 S2)
  (append
   (cond
     ((null? S1) '())
     (else (if (member? (first S1) S2) (list (first S1)) '())))
   (if (not (null? S1)) (intersect (rest S1) S2) '())))

