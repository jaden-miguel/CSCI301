#lang racket

; Name: Jaden Ji Miguel
; Date: Spring 2022
; Purpose: Lab 5 - CSCI301

; recursive function to check reflexive relation between
; a set and its relation.
(define (Reflexive? L S)
  (cond
    ;if the two lists are empty return true
    ((and (empty? L) (empty? S)) #t)
    ;if only one list is empty return false
    ((and (empty? L) (not (empty? S))) #f)
    ((and (not (empty? L)) (empty? S)) #f)
    (else (if (and (eq? (first S) (first (first L))) (eq? (first S) (second (first L)))) (Reflexive? (rest L) (rest S)) #f))))


; recursive function to check symmetric relation between
; a set and its relation.
(define (Symmetric? L)
  ;null case
    (if (null? L) true 
        (if (eq? (caar L) (cadar L)) (Symmetric? (cdr L)) 
            (if (and (eq? (caar L) (cadadr L)) 
                    (eq? (cadar L) (caadr L)))(Symmetric? (cddr L))  false))))


; wrapper function to check transitive relation between
; a set and its relation.
(define Transitive?
  (lambda (L)
    (real-transitive? L L)))

; helper function breaking into two lists
(define real-transitive?
  (lambda (L changeL)
    (if (null? changeL) #t
        (if (check-for-transitive L (car changeL) L)
            (real-transitive? L (cdr changeL))
            #f))))

; where most of the work is done, recursively + reset
(define check-for-transitive
 (lambda (L pair resetL)
   ;null case
   (if (null? resetL) #t
       (if (equal? (car (car resetL)) (car (cdr pair)))
           (if (set-exists? (list (car pair) (car (cdr (car resetL)))) L) 
               (check-for-transitive L pair (cdr resetL))
               #f)
           (check-for-transitive L pair (cdr resetL))))))

; 
; helper functions
;

(define relation?
  (lambda (L S)
    (if (null? L) #t
        (if (and (member? (car (car L)) S) (member? (car (cdr (car L))) S))
            (relation? (cdr L) S)
            #f))))

; set exists helper function
(define set-exists?
  (lambda (L1 L2)
    (if (null? L1) #t
        (if (null? L2) #f
            (if (equal? L1 (car L2)) #t
                (set-exists? L1 (cdr L2)))))))

; member helper function
(define member?
  (lambda (x L)
    (if (null? L) 
        #f
        (or (equal? x (car L)) (member? x (cdr L))))))