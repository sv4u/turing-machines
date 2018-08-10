#lang racket

; Macros

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
(define (revappend a b) (foldl cons a b))
(define-m show-tape
  [(Tape '() '() '()) '()]
  [(Tape l '() r) (revappend l (cons '() r))]
  [(Tape l v r) (revappend l (cons (list v) r))])                                                        

; Interpreter

(define-m* interpreter
  [((list v 'right S) tape) (list S (shift-right (put v tape)))]
  [((list v 'left S) tape) (list S (shift-left (put v tape)))]
  [((list v 'stay S) tape) (list S (put v tape))]
  [((list S _) tape) (list S tape)])

(define ((fixed-point f) x)
  (let F ([x x] [fx (f x)])
    (if (equal? x fx)
        fx
        (F fx (f fx)))))

(define (run-turing prog t0 start)
  ((fixed-point
    (match-lambda
      [`(,S ,T) (begin
                  (printf "~a\t~a\n" S (show-tape T))
                  (interpreter (prog `(,S ,(get T))) T))]))
   (list start (initial-tape t0))))

(define-syntax-rule (Turing-Machine #:start start (a b c d e) ...)
  (lambda (l)
    (displayln "STATE\tTAPE")
    ((match-lambda [(list _ t) (flatten (show-tape t))]) 
     (run-turing 
      (match-lambda ['(a b) '(c d e)] ... [x x]) 
      l start))))

; Simple Incrementer

(define INC
  (Turing-Machine #:start 'q0
    [q0 1 1 right q0]
    [q0 () 1 stay qf]))

(INC '(1 1 1))

; 3-State Busy Beaver

(define BEAVER
  (Turing-Machine #:start 'a
   [a () 1 right b]
   [a  1 1 left  c]
   [b () 1 left  a]
   [b  1 1 right b]
   [c () 1 left  b]
   [c  1 1 stay  halt]))

(BEAVER '(()))