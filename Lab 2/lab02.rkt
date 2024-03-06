#lang racket

; Name: Jaden Ji Miguel
; Date: Spring 2022
; Purpose: CSCI301 lab02.rkt


; gen-list: function will generate
; a list of consecutive integers, from start to end.
; if start > end, empty list is generated.
(define gen-list
  (lambda (start end)
    (if (> start end)
        '()
        (cons start (gen-list (+ 1 start) end)))))


; a recursive function named sum
; adds up all the elements in a list
(define (sum lst)
  (cond [(empty? lst) 0]
      [else (+ (car lst) (sum(cdr lst)))]))


; retrieve first n function, that return a list
; of the first n elements in a list. handles negatives and large
; n values.
(define retrieve-first-n
  (lambda (n lst)
    (cond ((< n 1) '())
          (( > n (length lst)) '())
          (else (cons (car lst) (retrieve-first-n (- n 1) (cdr lst)))))))
    


; recursive pair-sum function that takes an int sequence
; from gen-list above. tests whether any two adjacent values
; in the given list sum to the given value.
(define pair-sum?
  (lambda (lst n)
    (if (null? lst) #f ;; empty list
        (if (null? (cdr lst)) #f ;; no pairs
            (if (= (+ (car lst) (car (cdr lst))) n)
                #t
                (pair-sum? (cdr lst) n))))))

