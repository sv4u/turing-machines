#lang racket

(define-syntax-rule (define-m f m ...) (define f (match-lambda m ... (x x))))
(define-syntax-rule (define-m* f m ...) (define f (match-lambda** m ...)))

(struct Tape (the-left-part the-current-record the-right-part))

(define-m initial-tape [(cons h t) (Tape '() h t)])