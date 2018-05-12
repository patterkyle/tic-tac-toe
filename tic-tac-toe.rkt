#lang racket

(require pict
         racket/gui)

(provide game-over?)

(define name         "tic-tac-toe")
(define frame-width  400)
(define frame-height 300)
(define cell-size    50)
(define grid-sep     10)

;game state
(struct gs (board
            next-move)
  #:transparent
  #:mutable)

(define (empty-board)
  (make-vector 9 empty))

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
    [a #:when (not (empty? a)) (text (symbol->string a)
                                     (cons "monospace" 'default)
                                     size)]
    [_ (blank size)]))

(define (draw-board board cell-size)
  (table 3
         (for/list ([val board])
           (draw-cell val cell-size))
         cc-superimpose
         cc-superimpose
         grid-sep
         grid-sep))

(define b0 (empty-board))
(define b1 (vector 'x    'x   'x
                   empty empty empty
                   empty empty empty))
(define b2 (vector 'x    'x    'o
                   empty 'o     empty
                   'o     empty 'x))

(define frame (new frame%
                   [label  name]
                   [width  frame-width]
                   [height frame-height]))

(define (handle-event event)
  (void))

(define (handle-char-event event)
  (void))

(define game-canvas% (class canvas%
                       (define/override (on-event event)
                         (handle-event event))
                       (define/override (on-char event)
                         (handle-char-event event))
                       (super-new)))

(new game-canvas% [parent frame])

(define (main)
  (send frame show #t))

(main)
