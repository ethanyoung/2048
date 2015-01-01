#lang racket

(define (make-board n)
  (make-list n( make-list n 0)))

(make-board 4)

(define PIECE_DIST '(2 2 2 2 2 2 2 2 2 4))
(define (choice l)
  (if (list? l) (list-ref l (random (length l)))
    (vector-ref l (random (vector-length l)))))

(define (get-a-piece)
  (choice PIECE_DIST))

(get-a-piece)
