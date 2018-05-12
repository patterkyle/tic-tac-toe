#lang racket

(require pict
         racket/gui)

(provide winner?)

(define name    "tic-tac-toe")
(define frame-w 400)
(define frame-h 300)

(define (empty-board)
  (make-vector 9 empty))

;game state
(struct gs (board
            next-move
            winner?); #f if game is ongoing, 'x or 'y if game is over
  #:transparent
  #:mutable)

(define game-state (gs (empty-board)
                       (list-ref '(x o) (random 2))
                       #f))

(define (winner? board)
  (match board
    [(or (vector a a a
                 _ _ _
                 _ _ _)
         (vector _ _ _
                 a a a
                 _ _ _)
         (vector _ _ _
                 _ _ _
                 a a a)
         (vector a _ _
                 a _ _
                 a _ _)
         (vector _ a _
                 _ a _
                 _ a _)
         (vector _ _ a
                 _ _ a
                 _ _ a)
         (vector a _ _
                 _ a _
                 _ _ a)
         (vector _ _ a
                 _ a _
                 a _ _)) #:when (not (empty? a)) a]
    [_ #f]))

(define (draw-cell val w h)
  (cc-superimpose (match val
                    [s #:when (not (empty? s))
                       (scale-to-fit (text (symbol->string s)
                                           (cons "monospace" 'default))
                                     w h)]
                    [_ (blank w h)])
                  (rectangle w h)))

;; (define (success-msg winner w h)
;;   (text ))

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

(define (update-cell! state i)
  (vector-set! (gs-board state) i (gs-next-move state))
  (set-gs-next-move! state (if (eq? (gs-next-move state) 'x)
                               'o
                               'x)))

(define (handle-char-event event canvas)
  (match (send event get-key-code)
    [a #:when (and (not (empty? a))
                   (not (symbol? a))
                   (char-numeric? a))
       (let ([num (string->number (string a))])
         (when (< 0 num 10)
           (update-cell! game-state (sub1 num))
           (match (winner? (gs-board game-state))
             ['x (set-gs-winner?! game-state 'x)]
             ['o (set-gs-winner?! game-state 'o)]
             [_ (void)])
           (send canvas refresh)))]
    [_ (void)]))

(define (paint-callback canvas dc)
  (send dc set-smoothing 'aligned)
  (match (gs-winner? game-state)
    ['x (draw-pict (text "x wins") dc 0 0)]
    ['o (draw-pict (text "o wins") dc 0 0)]
    [_ (draw-pict (game-board (gs-board game-state)
                              frame-w frame-h)
                  dc 0 0)]))

(define game-canvas% (class canvas%
                       (define/override (on-event event)
                         (handle-event event))
                       (define/override (on-char event)
                         (handle-char-event event this))
                       (super-new)))

(new game-canvas%
     [parent         frame]
     [paint-callback paint-callback])

(define (main)
  (send frame show #t))

(main)
