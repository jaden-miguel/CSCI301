#lang racket

; lab3.rkt
; Name: Jaden Miguel
; Spring 2021
;
; This program implements twelve functions to
; perform basic set operations. Notably without
; the Racket built-in representations.  

; Sets and Set Operations: In order.

; s-equal? (1.)
;
; Returns true iff sets A and B are equal.
;
; Parameters:
;    A (list): primary set
;    B (list): secondary set
;
; Returns:
;    A boolean value, determining the equality
;    of the two sets.
(define (s-equal? A B)
  (and (s-subset? A B)
       (s-superset? A B)))

; s-in? (2.)
;
; Returns true iff a is an element of A.
;
; Parameters:
;    a (list): value questioned
;    A (list): primary set
;
; Returns:
;    A boolean value, determining
;    if element a is in the set A.
(define (s-in? a A)
  (ormap (lambda (x)
           (equal? x a)) A))

; s-intersect (3.)
;
; Returns the set containing all (and only)
; the elements in A ∩ B.
;
; Parameters:
;    A (list): primary set
;    B (list): secondary set
;
; Returns:
;    A set of shared elements between sets A and B.
(define (s-intersect A B)
  (cond
    [(empty? A) '()]
        [(s-in? (car A) B) (cons (car A) (s-intersect (cdr A) B))]
        [else (s-intersect (cdr A) B)]))

; s-union (4.)
;
; Interprets the union between sets A and B. A ∪ B.
;
; Parameters:
;    A (list): primary set
;    B (list): secondary set
;
; Returns:
;    A set of elements in both sets A and B.
(define (s-union A B)
  (list-to-set (append A B)))

; s-diff (5.)
;
; Interprets the difference between sets A and B
; recursively.
;
; Parameters:
;    A (list): primary set
;    B (list): secondary set
;
; Returns:
;    The set containing all (and only)
;    the elements in A - B.
(define (s-diff A B)
  ; empty set
  (cond [(empty? A) '()]
        [(not (s-in? (car A) B)) (cons (car A) (s-diff (cdr A) B))]
        [else (s-diff (cdr A) B)]))

; s-add (6.)
;
; Adds value a to set A.
;
; Parameters:
;    a (list): value to add
;    A (list): primary set
;
; Returns:
;    A union set of value a and set A.
(define (s-add a A)
  (s-union a A))

; s-remove (7.)
;
; Removes value a from set A.
;
; Parameters:
;    a (list): value to remove
;    A (list): primary set
;
; Returns:
;    Set A with element a removed. A - {a}.
(define (s-remove a A)
  (s-diff A a))

; s-product (8.)
;
; Interprets the Cartesian product of A and B.
;
; Parameters:
;    A (list): primary set
;    B (list): secondary set
;
; Returns:
;    A set containing the product of
;    sets A and B. A × B.
(define (s-product A B)
  (apply append (map (lambda (x)
                       (map (lambda (y)
                              (list x y)) B)) A)))

; s-subset? (9.)
;
; Returns true iff A is a subset of B.
;
; Parameters:
;    A (list): primary set
;    B (list): secondary set
;
; Returns:
;    A boolean value, determining if A
;    is a subset of B.
(define (s-subset? A B)
  (andmap (lambda (x)
            (s-in? x A)) B))
              
; s-superset? (10.)
;
; Returns true iff A is a superset of B.
;
; Parameters:
;    A (list): primary set
;    B (list): secondary set
;
; Returns:
;    A boolean value, returning true iff A ⊇ B.
(define (s-superset? A B)
  (andmap (lambda (x)
            (s-in? x B)) A))

; s-size (11.)
;
; Interprets the cardinality of set A.
;
; Parameters:
;    A (list): primary set
;
; Returns:
;    A numerical value, representing the size of set A.
(define (s-size A)
  (length A))

; list-to-set (12.)
;
; Converts a list to a set recursively.
;
; Parameters:
;    L (list): primary list
;
; Returns:
;    A set with all and only the elements in L.
(define (list-to-set L)
  (cond
    ; empty set
    [(empty? L) '()]
        [(s-in? (car L) (cdr L)) (list-to-set (cdr L))]
        [else (cons (car L) (list-to-set (cdr L)))]))