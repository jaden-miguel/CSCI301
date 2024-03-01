#lang racket

; prog1.rkt
; Name: Jaden Miguel
; Date: Spring 2021
; Purpose: Program 1 - Predicate Logic in Racket
;
; This program includes ten logical expressions,
; with a procedure that implements the functionality
; of predicates.

;helper functions
(define (implies P Q)
  (or (not P) Q))

(define (contains? x S)
  (cond ((null? S) false)
        ((equal? x (car S)) true)
        (else (contains? x (cdr S)))))

;universal definitions
(define A '(hutchinson hearne fizzano 241 301 474 493 dennis doris spruce redwood))

(define is-course '((301) (241) (474) (493)))

(define is-faculty '((hutchinson) (hearne) (fizzano)))

(define is-student '((dennis) (doris)))

(define teaches-to '((hutchinson 301 dennis) (hutchinson 241 doris)
                                             (hearne 301 doris) (hearne 493 dennis) (fizzano 474 doris))) 

(define is-person '((hutchinson) (hearne) (fizzano) (dennis) (doris)))

(define is-plant '((spruce) (redwood)))

(define is-tree '((spruce) (redwood)))

(define taller-than '((redwood spruce) (redwood doris) (redwood dennis)
                                       (spruce doris) (spruce dennis) (dennis doris)))

(define likes-more-than '((dennis 241 301) (doris 301 241)))

;predicate definitions
(define (is-student? x)
  (contains? (list x) is-student))

(define (is-faculty? x)
  (contains? (list x) is-faculty))

(define (is-course? x)
  (contains? (list x) is-course))

(define (is-tree? x)
  (contains? (list x) is-tree))

(define (is-plant? x)
  (contains? (list x) is-plant))

(define (taller-than? x y)
  (contains? (list x y) taller-than))

(define (teaches-to? prof course student)
  (contains? (list prof course student) teaches-to))

(define (is-person? x)
  (contains? (list x) is-person))

; predicate-logic
;
; Implements the functionality of predicates, open sentences
; whose truth values are contingent to the arguments given to them.
;
; Hint from prog1.pdf: think of how ∀ and ∃
; could be viewed as mapping and applying.
;
; Parameters:
;    U (list): the universe over which the quantifiers will quantify.
;    Q (list): a list of quantifers of length k. ('forall, 'exists)
;    E (proc): a logical expression which accepts k variables.
;
; Returns:
;    A boolean value, corresponding to true or false,
;    after the quantifiers are applied over U to
;    the logical expression.
(define (predicate-logic U Q E)
   (cond
     ; Q null return case
     [(null? Q) E]
     [(eq? (car Q) 'forall) (for-all (lambda (n_arg)
                                       (predicate-logic U (cdr Q)(curry E n_arg))) U)]
     [(eq? (car Q) 'exists) (exists (lambda (n_arg)
                                      (predicate-logic U (cdr Q)(curry E n_arg))) U)]))

; for-all
;
; Functionality of the "for all" logical quantifier. Uses andmap.
;
; Parameters:
;    expr (proc): The expression over which the procedure will test.
;    vars (list): A list of elements from a given universe.
;
; Returns:
;    A boolean value, corresponding to whether or not the expression returns
;    true for all values in vars.
(define (for-all expr vars)
  (andmap expr vars))

; exists
;
; Functionality of the "exists" logical quantifier. Uses ormap.
;
; Parameters:
;    expr (proc): The expression over which the procedure will test.
;    vars (list): A list of elements from a given universe.
;
; Returns:
;    A boolean value, corresponding to whether or not the expression returns
;    true for at least one of the values in vars.
(define (exists expr vars)
  (ormap expr vars))


; expressions & statements utilizing predicate-logic
;
; e1 & statement1
;
; Implements: ∀x(isTree(x) → isPlant(x))
; Includes a statement which passes the expression into
; predicate-logic w/ added parameters.
;
; Parameters:
;    x (list): primary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e1 x)
  (implies (is-tree? x) (is-plant? x)))

(define statement1 (predicate-logic A (list 'forall) e1))

; e2 & statement2
;
; Implements the expression: ∃x∀y(isPlant(y) → isTree(x) ∧ ¬tallerThan(y, x))
;
; Parameters:
;    x (list): primary quantifier
;    y (list): secondary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e2 x y)
  (implies (is-plant? y)
           (and (is-tree? x)
                (not (taller-than? y x)))))

(define statement2 (predicate-logic A (list 'exists 'forall) e2))

; e3 & statement3
;
; Implements the expression: ∃x∃y(isTree(x) ∧ ¬isTree(y) ∧ tallerThan(y, x))
;
; Parameters:
;    x (list): primary quantifier
;    y (list): secondary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e3 x y)
  (and (and (is-tree? x)
            (not (is-tree? y)))
       (taller-than? y x)))

(define statement3 (predicate-logic A (list 'exists 'exists) e3))

; e4 & statement4
;
; Implements the expression: ∀x(isStudent(x) → isPerson(x))
;
; Parameters:
;    x (list): primary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e4 x)
  (implies (is-student? x)
           (is-person? x)))

(define statement4 (predicate-logic A (list 'forall) e4))

; e5 & statement5
;
; Implements the expression: ∃x(isFaculty(x) ∧ isStudent(x))
;
; Parameters:
;    x (list): primary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e5 x)
  (and (is-faculty? x)
       (is-student? x)))

(define statement5 (predicate-logic A (list 'exists) e5))
         
; e6 & statement6
;
; Implements the expression: ∃x∃y∃z(isFaculty(x) ∧ isStudent(y) ∧ isCourse(z) ∧ teachesTo(x, z, y))
;
; Parameters:
;    x (list): primary quantifier
;    y (list): secondary quantifier
;    z (list): tertiary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e6 x y z)
  (and (is-faculty? x)
       (is-student? y)
       (is-course? z)
       (teaches-to? x z y)))

(define statement6 (predicate-logic A (list 'exists 'exists 'exists) e6))

; e7 & statement7
;
; Implements the expression: ∀x(isFaculty(x) → ¬isStudent(x))
;
; Parameters:
;    x (list): primary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e7 x)
  (implies (is-faculty? x)
           (not (is-student? x))))

(define statement7 (predicate-logic A (list 'forall) e7))

; e8 & statement8
;
; Implements the expression: ∀x∃y(isStudent(x) → isCourse(y))
;
; Parameters:
;    x (list): primary quantifier
;    y (list): secondary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e8 x y)
  (implies (is-student? x)
           (is-course? y)))

(define statement8 (predicate-logic A (list 'forall 'exists) e8))

; e9 & statement9
;
; Implements the expression: ∃x(isPlant(x) → isTree(x))
;
; Parameters:
;    x (list): primary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e9 x)
  (implies (is-plant? x)
           (is-tree? x)))

(define statement9 (predicate-logic A (list 'exists) e9))

; e10 & statement10
;
; Implements the expression: ∀x∃y(isPlant(x) → ¬isClass(y))
;
; Parameters:
;    x (list): primary quantifier
;    y (list): secondary quantifier
;
; Returns:
;    A boolean value, corresponding to whether or not the
;    logical expression results in a value of true or false.
(define (e10 x y)
  (implies (is-plant? x)
           (not (is-course? y))))

(define statement10 (predicate-logic A (list 'forall 'exists) e10))