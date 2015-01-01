#lang racket

(define (make-board n)
  (make-list n( make-list n 0)))

(make-board 4)
