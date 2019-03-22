#lang racket

; Function definitions
(define (add a b) (+ a b))
(define (inc a) (add a 1))

; Display calculation 
(display (add (inc 1) 2))
