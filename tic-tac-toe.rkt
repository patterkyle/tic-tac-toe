#lang racket

(require pict
         racket/gui)

(provide game-over?)

(define name     "tic-tac-toe")
(define frame-w  400)
(define frame-h  300)

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

(define (draw-cell val w h)
  (cc-superimpose (match val
                    [s #:when (not (empty? s))
                       (scale-to-fit (text (symbol->string s)
                                           (cons "monospace" 'default))
                                     w h)]
                    [_ (blank w h)])
                  (rectangle w h)))

(define (game-board board w h)
  (scale-to-fit (table 3
                       (for/list ([val board])
                         (draw-cell val
                                    (floor (/ w 3))
                                    (floor (/ h 3))))
                       cc-superimpose
                       cc-superimpose
                       0 0)
                w h))

(define b0 (empty-board))
(define b1 (vector 'x    'x    'x
                   empty empty empty
                   empty empty empty))
(define b2 (vector 'x    'x    'o
                   empty 'o    empty
                   'o    empty 'x))

(define frame (new frame%
                   [label  name]
                   [width  frame-w]
                   [height frame-h]))

(define (handle-event event)
  (void))

(define (handle-char-event event)
  (void))

(define (paint-callback canvas dc)
  (draw-pict (game-board b2 frame-w frame-h)
             dc 0 0))

(define game-canvas% (class canvas%
                       (define/override (on-event event)
                         (handle-event event))
                       (define/override (on-char event)
                         (handle-char-event event))
                       (super-new)))

(new game-canvas%
     [parent         frame]
     [paint-callback paint-callback])

(define (main)
  (send frame show #t))

(main)
