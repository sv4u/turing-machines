#lang racket

(define-syntax-rule (define-m f m ...) (define f (match-lambda m ... (x x))))
(define-syntax-rule (define-m* f m ...) (define f (match-lambda** m ...)))

(struct Tape (the-left-part the-current-record the-right-part))

(define-m initial-tape [(cons h t) (Tape '() h t)])

(define (snoc a b) (cons b a))
(define-m shift-right
  [(Tape '() '() (cons h t)) (Tape '() h t)]
  [(Tape l x '()) (Tape (snoc l x) '() '())]
  [(Tape l x (cons h t)) (Tape (snoc l x) h t)])
(define-m flip-tape [(Tape l x r) (Tape r x l)])
(define shift-left (compose flip-tape shift-right flip-tape))
(define-m get [(Tape _ v _) v])
(define-m* put [('() t) t] [(v (Tape l _ r)) (Tape l v r)])