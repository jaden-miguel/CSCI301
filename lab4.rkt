#lang racket

; lab4.rkt
; Jaden Miguel
; Spring 2021
;
; This program implements seven properties
; and operations on Binary Relations. 
;
; arbitrary definitions
(define A (list 1 2 3))
(define R1 (list (cons 3 3) (cons 1 2)))
(define R2 (list (cons 1 3) (cons 2 1) (cons 3 2)))

; reflexive? (1.)
;
; ∀x∈A xRx
;
; Parameters:
;    A (list): The set over which R is determined to be reflexive.
;    R (list-pairs): A relation with elements from set A.
;
; Returns:
;    A boolean value, validating whether or not relation R
;    over set A is reflexive.
(define (reflexive? A R)
  ; create universe for predicate functionality
  (define (for-all? X Y E)
   (cond
     [(null? Y) E]
     [(eq? (car Y) 'forall) (for-all (lambda (next_arg) (for-all? X (cdr Y) (curry E next_arg))) X)]))
  (for-all? A (list 'forall)
               (lambda (x)
                 (is-in? (cons x x) R))))

; irreflexive? (2.)
;
; ∀x∈A ¬xRx
;
; Parameters:
;    A (list): The set over which R is determined to be irreflexive.
;    R (list-pairs): A relation with elements from set A.
;
; Returns:
;    A boolean value, validating whether or not relation R
;    over set A is irreflexive.
(define (irreflexive? A R)
  ; create universe for predicate functionality
  (define (for-all? X Y E)
   (cond
     [(null? Y) E]
     [(eq? (car Y) 'forall) (for-all (lambda (next_arg) (for-all? X (cdr Y) (curry E next_arg))) X)]))
  (for-all? A (list 'forall)
            (lambda (x)
              (not (is-in? (cons x x) R)))))


; symmetric? (3.)
;
; ∀x∈A ∀y∈A (xRy→yRx)
;
; Parameters:
;    A (list): The set over which R is determined to be symmetric.
;    R (list-pairs): A relation with elements from set A.
;
; Returns:
;    A boolean value, validating whether or not relation R
;    over set A is symmetric.
(define (symmetric? A R)
  ; create universe for predicate functionality
  (define (for-all? X Y E)
   (cond
     [(null? Y) E]
     [(eq? (car Y) 'forall) (for-all (lambda (next_arg) (for-all? X (cdr Y) (curry E next_arg))) X)]))
  (for-all? A (list 'forall 'forall)
            (lambda (x y) (implies (is-in? (cons x y) R)
                                   (is-in? (cons y x) R)))))


; anti-symmetric? (4.)
;
; ∀x∈A ∀y∈A((xRy∧yRx)→(x=y))  
;
; Parameters:
;    A (list): The set over which R is determined to be anti-symmetric.
;    R (list-pairs): A relation with elements from set A.
;
; Returns:
;    A boolean value, validating whether or not relation R
;    over set A is anti-symmetric.
(define (anti-symmetric? A R)
  ; create universe for predicate functionality
  (define (for-all? X Y E)
   (cond
     [(null? Y) E]
     [(eq? (car Y) 'forall) (for-all (lambda (next_arg) (for-all? X (cdr Y) (curry E next_arg))) X)]))
  (for-all? A (list 'forall 'forall)
            (lambda (x y)
              (implies (and (is-in? (cons x y) R)
                            (is-in? (cons y x) R))
                       (eq? x y)))))

; total? (5.)
;
; ∀x∈A ∀y∈A(xRy ∨ yRx)
;
; Parameters:
;    A (list): The set over which R is determined to be total.
;    R (list-pairs): A relation with elements from set A.
;
; Returns:
;    A boolean value, validating whether or not relation R
;    over set A is total/connex.
(define (total? A R)
  ; create universe for predicate functionality
  (define (for-all? X Y E)
   (cond
     [(null? Y) E]
     [(eq? (car Y) 'forall) (for-all (lambda (next_arg) (for-all? X (cdr Y) (curry E next_arg))) X)]))
  (for-all? A (list 'forall 'forall)
            (lambda (x y)
              (or (is-in? (cons x y) R)
                  (is-in? (cons y x) R)))))

; transitive? (6.)
;
; ∀x∈A ∀y∈A ∀z∈A((xRy ∧ yRz)→xRz)
;
; Parameters:
;    A (list): The set over which R is determined to be transitive.
;    R (list-pairs): A relation with elements from set A.
;
; Returns:
;    A boolean value, validating whether or not relation R
;    over set A is transitive.
(define (transitive? A R)
  ; create universe for predicate functionality
  (define (for-all? X Y E)
   (cond
     [(null? Y) E]
     [(eq? (car Y) 'forall) (for-all (lambda (next_arg) (for-all? X (cdr Y) (curry E next_arg))) X)]))
  (for-all? A (list 'forall 'forall 'forall)
            ; need three instances
            (lambda (x y z) (implies
                             (and (is-in? (cons x y) R)
                                  (is-in? (cons y z) R))
                             (is-in? (cons x z) R)))))

; compose (7.)
;
; Returns a composition of relations R1 and R2 over set A.
;
; Parameters:
;    A (list): the set over which R1 and R2 are composed.
;    R1 (list): primary relation with elements from set A.
;    R2 (list): secondary relation with elements from set A.
;
; Returns:
;    A list of pairs, representing the composition of relations R1 and
;    R2 over set A, omitting any duplicates.
(define (compose A R1 R2)
  ; need to handle pair case
  (define (create-pair a b R2)
    (cons a (cdr (car (filter (lambda (x)
                                (eq? b (car x))) R2)))))
  
  (cond
    [(empty? R1) '()]
        ; create list of pairs, the composition of R1 and R2.
        ; remove pairs that hold values that are not in A.
        [else (filter (lambda (x) (and (is-in? (car x) A) (is-in? (cdr x) A)))
                      (rem-dupes (append (cons
                                          (create-pair (car (car R1)) (cdr (car R1)) R2)
                                          (compose A (cdr R1) R2)))))]))

;; helper functions


; implies
;
; Determines whether or not one statement implies another.
;
; Parameters:
;    U (list): primary statement
;    Q (list): secondary statement
;
; Returns:
;    A boolean value, validating whether or not statement
;    P implies statement Q.
(define (implies P Q)
  (or (not P) Q))

; for-all
;
; Implements the functionality of the for all logical quantifier.
;
; Parameters:
;    expr (proc): the expression which the procedure will test.
;    vars (list): a list of elements from a given universe.
;
; Returns:
;    A boolean value, validating whether or not the expression returns
;    true for all values in vars.
(define (for-all expr vars)
  (andmap expr vars))

; is-in?
;
; Checks to see if value a is an element of set A.
;
; Parameters:
;    a (element): value in question
;    A (list): primary set
;
; Returns:
;    A boolean value, corresponding to whether
;    or not "a" is an element of A.
(define (is-in? a A)
  (ormap (lambda (x)
           (equal? x a)) A))

; rem-dupes
;
; Removes duplicates from a selected list.
;
; Parameters:
;    L (list): primary list
;
; Returns:
;    A list w/o any duplicate elements.
(define (rem-dupes L)
  (cond
    ; empty case
    [(empty? L) '()]
    [(is-in? (car L) (cdr L)) (rem-dupes (cdr L))]
    [else (cons (car L) (rem-dupes (cdr L)))]))