#lang racket

; Name: Jaden Ji Miguel
; Date: Spring 2022
; Purpose: Lab 6 (Properties of Relations)


; start reflexive closure

; wrapper function that checks if initial L is reflexive, if so, no work needs to be done
(define Reflexive-Closure
  (lambda (L S)
    (if (reflexive? L S) L
        (real-reflexive-closure L L S S))))

; recursively finds which pairs are missing from L, leaves them in S, otherwise removes them
(define real-reflexive-closure
  (lambda (L fullL S fullS)
    (if (null? L) (p-reflexive-closure fullL S fullS)
        (if (equal? (car (car L)) (car (cdr (car L))))
            (real-reflexive-closure (cdr L) fullL (remove (car (car L)) S) fullS) ;;remove pair from L and S, since it's not missing
            (real-reflexive-closure (cdr L) fullL S fullS)))))

; called once all of L has been checked, and adds the missing pairs into L
(define p-reflexive-closure
  (lambda (L missing S)
    (if (null? missing)
        (if (reflexive? L S) L
            (displayln (append L )))
        (p-reflexive-closure (append L (list (append (list (car missing)) (list (car missing))))) (cdr missing) S))))


; helper functions for reflexive closure
(define reflexive?
  (lambda (L S)
    (if (not (relation? L S)) #f
        (real-reflexive? L S))))

(define real-reflexive?
  (lambda (L S)
    (if (null? S) #t
        (if (< (length L) (length S)) #f
            (if (equal? (car (car L)) (car (cdr (car L))))
                (real-reflexive? (cdr L) (remove (car (car L)) S))
                (real-reflexive? (cdr L) S))))))

; start symmetric closure ;

; symmetric-closure
; wrapper function that checks if initial L is symmetric, if so, no work needs to be done
(define Symmetric-Closure
  (lambda (L)
    (if (symmetric? L) L
        (real-symmetric-closure L L))))

; function recursively checks if reverse of pair in L, L is used to make progress
(define real-symmetric-closure
  (lambda (L fullL)
    (if (null? L) fullL
        (if (set-exists? (reverse (car L)) L)
            (real-symmetric-closure (cdr L) fullL) ;if symmetric pair is in L, nothing needs to be added
            (real-symmetric-closure (cdr L) (append (list (reverse (car L))) fullL))))))

; helper for symmetric closure, check reverse
(define symmetric?
  (lambda (L)
    (if (null? L) #t
        (if (set-exists? (reverse (car L)) L)
            (if (equal? (car (car L)) (car (cdr (car L))))
                (symmetric? (remove* (list (car L)) L)) ;removes only the pair that is reflexive (a, a)
                (symmetric? (remove* (list (reverse (car L))) (remove* (list (car L)) L)))) #f))))



; start transitive closure ;


; wrapper function checks if initial L is transitive, if so, no work needs to be done
(define Transitive-Closure
  (lambda (L)
    (if (transitive? L) L
        (real-transitive-closure L L))))

; function recursively makes progress and adds the missing pairs to fullL
(define real-transitive-closure
  (lambda (L fullL)
    (if (null? L) fullL
        (if (check-for-transitive fullL (car L) fullL)
            (real-transitive-closure (cdr L) fullL)
            (real-transitive-closure (cdr L) (append (add-missing-pairs fullL (car L) fullL '()) fullL))))))

; function finds missing pairs, adds them to a list, and returns them
(define add-missing-pairs
  (lambda (L pair resetL missingPairs)
    (if (null? resetL) missingPairs
        (if (equal? (car (car resetL)) (car (cdr pair)))
            ; start set exist check, recursively iterate
            (if (set-exists? (list (car pair) (car (cdr (car resetL)))) L) 
                (add-missing-pairs L pair (cdr resetL) missingPairs) 
                (add-missing-pairs L pair (cdr resetL) (append (list (list (car pair) (car (cdr (car resetL))))) missingPairs)))
            (add-missing-pairs L pair (cdr resetL) missingPairs)))))


; wrapper function for transitive
(define transitive?
  (lambda (L)
    (real-transitive? L L)))

; real-transitive? takes 2 lists, one is always the initial list L, the other is cdr L so that progress can be made
(define real-transitive?
  (lambda (L changeL)
    (if (null? changeL) #t
        (if (check-for-transitive L (car changeL) L)
            (real-transitive? L (cdr changeL)) ;;progress is made
            #f))))

; result code with recursive check
(define check-for-transitive
 (lambda (L pair resetL)
   (if (null? resetL) #t
       (if (equal? (car (car resetL)) (car (cdr pair)))
           (if (set-exists? (list (car pair) (car (cdr (car resetL)))) L) 
               (check-for-transitive L pair (cdr resetL)) ;keep checking resetL for possible links
               #f)
           (check-for-transitive L pair (cdr resetL))))))



; helper functions ;

; function checks if a set L is a relation of the set S
(define relation?
  (lambda (L S)
    (if (null? L) #t
        (if (and (member? (car (car L)) S) (member? (car (cdr (car L))) S))
            (relation? (cdr L) S)
            #f))))

; function checks if a single element is in a list L
(define member?
  (lambda (x L)
    (if (null? L) 
        #f
        (or (equal? x (car L)) (member? x (cdr L))))))
     
; does this set exist? uses l1 and l2
(define set-exists?
  (lambda (L1 L2)
    (if (null? L1) #t
        (if (null? L2) #f
            (if (equal? L1 (car L2)) #t
                (set-exists? L1 (cdr L2)))))))