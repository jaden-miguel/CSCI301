#lang racket

; Name: Jaden Miguel
; Date: Spring 2021
; Purpose: Lab 2 - Dalun Zhang partner


; implements the logical implication operator
(define (implies p q)
  (or (not p) q))

; expr1 function, implements the expression
;P implies (Q -> R).
(define expr1
  
