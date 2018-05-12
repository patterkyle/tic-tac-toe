#lang racket

(require rackunit
         rackunit/text-ui
         "tic-tac-toe.rkt")

(define-test-suite tic-tac-test
  (check-equal? (game-over? (vector empty empty empty
                                    empty empty empty
                                    empty empty empty))
                #f)
  (check-equal? (game-over? (vector 'x    'x    'x
                                    empty empty empty
                                    empty empty empty))
                'x)
  (check-equal? (game-over? (vector 'o    'o    'o
                                    empty empty empty
                                    empty empty empty))
                'o)
  (check-equal? (game-over? (vector empty empty empty
                                    'x    'x    'x
                                    empty empty empty))
                'x)
  (check-equal? (game-over? (vector empty empty empty
                                    empty empty empty
                                    'o    'o    'o))
                'o)
  (check-equal? (game-over? (vector 'x    empty 'x
                                    empty 'o    'x
                                    'o    'o    'o))
                'o)
  (check-equal? (game-over? (vector 'x     empty 'x
                                    empty 'o     'x
                                    'o    'x     'o))
                #f)
  (check-equal? (game-over? (vector 'o empty 'x
                                    'x 'x    'o
                                    'o 'o    'x))
                #f)
  (check-equal? (game-over? (vector 'o    empty 'x
                                    'x    'x    'x
                                    empty 'x    'o))
                'x)
  (check-equal? (game-over? (vector 'o empty 'x
                                    'o 'x    'o
                                    'o 'o    'x))
                'o)
  (check-equal? (game-over? (vector 'o 'x 'x
                                    'x 'x 'o
                                    'o 'x 'x))
                'x)
  (check-equal? (game-over? (vector empty 'x 'x
                                    empty 'x 'o
                                    empty 'x 'x))
                'x)
  (check-equal? (game-over? (vector 'o 'x 'o
                                    'x 'o 'o
                                    'x 'x 'o))
                'o)
  (check-equal? (game-over? (vector 'x 'x 'o
                                    'x 'o 'x
                                    'o 'x 'o))
                'o)
  (check-equal? (game-over? (vector 'x 'x 'o
                                    'x 'x 'o
                                    'o 'o 'x))
                'x))

(run-tests tic-tac-test)
