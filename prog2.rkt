#lang racket

; prog2.rkt
; Jaden Miguel
; Spring 2021
;
; This program implements a myriad of functions to 
; analyze and manipulate grammars in Racket. We understand
; formal grammars as tuples, in this case- lists of length four.

;; Grammar Representations:

; G1
; S → aS|ε
(define G1 '((S)
             (a)
             (((S) (a S) ()))
             S))

; G2
; S → aA|ε  
; aA → bS|c
(define G2 '((S A)
             (a b c)
             (((S) (a A) ())
              ((a A) (b S) (c)))
              S))

; G3
; S → AabcBdefC|ε
; A → aB|eC|a
; B → bC|d
; C → d|ε
(define G3 '((S A B C)
             (a b c d e f)
             (((S) (A a b c B d e f C) ())
              ((A) (a B) (e C) (a))
              ((B) (b C) (d))
              ((C) (d) ()))
             S))

; 1. get-variables
;
; Acquires the variables of a grammar G.
;
; Parameters:
;    G (list): a 4-tuple grammar.
;
; Returns:
;    A list containing the variables of the
;    grammar G.
(define (get-variables G)
  (car G))

; 2. get-alphabet
;
; Acquires the alphabet of a grammar G.
;
; Parameters:
;    G (list): a 4-tuple grammar.
;
; Returns:
;    A list containing the terminals of the
;    grammar G.
(define (get-alphabet G)
  (car (cdr G)))

; 3. get-rules
;
; Acquires the production rules of a grammar G.
;
; Parameters:
;    G (list): a complete 4-tuple grammar.
;
; Returns:
;    A list containing the production rules of the
;    grammar G.
(define (get-rules G)
  (car (cdr (cdr G))))

; 4. get-start-symbol
;
; Acquires the start symbol of a grammar G.
;
; Parameters:
;    G (list): a complete 4-tuple grammar.
;
; Returns:
;    The start symbol of the grammar G.
(define (get-start-symbol G)
  (car (cdr (cdr (cdr G)))))

; 5. is-formal-grammar?
;
; Determines whether or not a given grammar G is formal.
;
; Parameters:
;    G (list): a complete 4-tuple grammar.
;
; Returns:
;    A boolean value, returns true if G is a valid
;    formal grammar (false otherwise).
(define (is-formal-grammar? G)
  ; trivial length check
  (and (eq? (length G) 4)
       (andmap (lambda (a)
                 (list? a)) (remove-last G))
       ; check start symbol for list
       (not (list? (get-start-symbol G)))
       ; continue
       (andmap (lambda (r) (list? r)) (get-rules G))
       (equal? (s-intersect (get-variables G) (get-alphabet G)) '())
       (andmap (lambda (r) (andmap (lambda (x)
                                     (andmap (lambda (y) (s-in? y (s-union (get-alphabet G)
                                                                           (get-variables G)))) x)) r))
               (get-rules G))
       (s-in? (get-start-symbol G) (get-variables G))))

; 6. is-context-free?
;
; Determines if grammar G is
; context-free or not.
;
; Parameters:
;    G (list): a complete 4-tuple grammar.
;
; Returns:
;    A boolean value, correlating whether or not G is
;    context-free.
(define (is-context-free? G)
  (and (is-formal-grammar? G)
       (and (andmap (lambda (x) (s-in? (car (car x)) (get-variables G))) (get-rules G))
            (andmap (lambda (t) (andmap (lambda (l) (andmap (lambda (e) (s-in? e (s-union (get-alphabet G)
                                                                                          (get-variables G)))) l)) (cdr t)))
                    (get-rules G)))))

; 7. is-right-regular?
;
; Determines whether or not a given grammar G is
; right regular.
;
; Parameters:
;    G (list): a complete 4-tuple grammar.
;
; Returns:
;    A boolean value, correlating whether or not G is
;    right-regular.
(define (is-right-regular? G)
  ; context check
  (and (is-context-free? G)
       ; trivial length check
       (andmap (lambda (r) (andmap (lambda (e) (< (length e) 3))
                                   (cdr r))) (get-rules G))
       (andmap (lambda (r) (andmap (lambda (x) (cond [(eq? (length x) 1) (s-in? (car x) (get-alphabet G))]
                                                     [(eq? (length x) 2) (and (s-in? (car x) (get-alphabet G))
                                                                              (s-in? (cadr x) (get-variables G)))]
                                                     [else #t])) (cdr r))) (get-rules G))))

; 8. generate-random-string
;
; Generates a random string using the alphabet
; and rules of grammar G iff G is context-free.
;
; Parameters:
;    G (list): a complete 4-tuple grammar.
;
; Returns:
;    A random string generated using the grammar G, empty
;    list if grammar is not context-free.
(define (generate-random-string G)
  (cond [(not (is-context-free? G)) '()]
        [else (rand-string (list (get-start-symbol G)) G)]))

;; Helper Functions:

; is-in?
;
; Checks to see if value a is an element of set A.
;
; Parameters:
;    a (element): value in question
;    B (list): primary set
;
; Returns:
;    A boolean value, correlating whether
;    or not a is an element of A.
(define (s-in? a A)
  (ormap (lambda (x)
           (equal? x a)) A))

; rand-string
;
; Creates a random string from A using B.
;
; Parameters:
;    A (list): a list representing the start symbol of B.
;    B (list): a complete 4-tuple grammar.
;
; Returns:
;    A random string to be used in generate-random-string.
(define (rand-string A B)
  (cond [(ormap (lambda (x)
                  (s-in? x (get-variables B))) A)
         (cond [(and (s-in? (car A) (get-variables B)) (empty? (cdr A)))
                (rand-string (rewrite (car A) B) B)]
               [(and (s-in? (car A) (get-variables B)) (not (empty? (cdr A))))
                (rand-string (append (rewrite (car A) B) (cdr A)) B)]
               [else (cons (car A) (rand-string (cdr A) B))])] [else A]))

; rewrite
;
; Rewrites a given variable in X over Y.
;
; Parameters:
;    X (element): given variable in Y.
;    Y (list): a complete 4-tuple grammar.
;
; Returns:
;    A rewritten variable in Y, as a list.
(define (rewrite X Y)
  (let ([rules (append-rules X (get-rules Y))])
    ; empty case
    (cond [(empty? (cdr rules)) rules]
          [else (list-ref rules
                          (random (length rules)))])))

; append-rules
;
; Appends the given rules together using variable V.
;
; Parameters:
;    V (list): a list representing a given variable.
;    R (list): a list of rules from a given grammar.
;
; Returns:
;    A list containing the appended rules in R using the
;    variable V.
(define (append-rules V R)
  (append-map (lambda (r)
                (cdr r))
              (filter (lambda (l) (equal? (car (car l)) V)) R)))

; remove-last
;
; Remove the last element from a list L.
;
; Parameters:
;    L (list): a list of any amount of elements.
;
; Returns:
;    A list containing exactly L, but w/ the
;    last element removed.
(define (remove-last L)
  (cond [(null? (cdr L)) '()]
        [else (cons (car L)
                    (remove-last (cdr L)))]))

; s-union
;
; Calculates the union between sets A and B.
;
; Parameters:
;    A (list): primary set.
;    B (list): secondary set.
;
; Returns:
;    A set of elements in both sets A and B,
;    w/o duplicates.
(define (s-union A B)
  (list-to-set (append A B)))

; list-to-set
;
; Converts a list to a set (list with no duplicate
; elements).
;
; Parameters:
;    L (list): primary list.
;
; Returns:
;    A set representing list L converted to a set, i.e.
;    L without any duplicate elements.
(define (list-to-set L)
  (cond [(empty? L) '()]
        [(s-in? (car L) (cdr L)) (list-to-set (cdr L))]
        [else (cons (car L) (list-to-set (cdr L)))]))

; s-intersect
;
; Calculates the intersection between sets A and B.
;
; Parameters:
;    A (list): primary set.
;    B (list): secondary set.
;
; Returns:
;    A set of shared elements between sets A and B.
(define (s-intersect A B)
  (cond [(empty? A) '()]
        [(s-in? (car A) B) (cons (car A) (s-intersect (cdr A) B))]
        [else (s-intersect (cdr A) B)]))