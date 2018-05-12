#lang racket

(require pict)

(provide game-over?)

(define NAME "tic-tac-toe")

;game state
(struct gs (board
            next-val)
  #:transparent
  #:mutable)

(define (empty-board)
  (make-vector 9 empty))

(define (print-board board)
  (apply (curry printf "~a ~a ~a\n~a ~a ~a\n~a ~a ~a")
         (vector->list board)))

(define (game-over? board)
  (match board
    [(vector a a a
             _ _ _
             _ _ _) #:when (not (empty? a)) a]
    [(vector _ _ _
             a a a
             _ _ _) #:when (not (empty? a)) a]
    [(vector _ _ _
             _ _ _
             a a a) #:when (not (empty? a)) a]
    [(vector a _ _
             a _ _
             a _ _) #:when (not (empty? a)) a]
    [(vector _ a _
             _ a _
             _ a _) #:when (not (empty? a)) a]
    [(vector _ _ a
             _ _ a
             _ _ a) #:when (not (empty? a)) a]
    [(vector a _ _
             _ a _
             _ _ a) #:when (not (empty? a)) a]
    [(vector _ _ a
             _ a _
             a _ _) #:when (not (empty? a)) a]
    [_ #f]))

(define (draw-cell val size)
  (match val
    [a #:when (not (empty? a))
       (text "x" (cons "monospace" 'default) size)]
    [_ (blank size)]))
