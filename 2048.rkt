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

(define (avail? lst)
  (if (list? lst)
    (ormap avail? lst)
    (zero? lst)))

(define (get-empty-refs lst zero-fun?)
  (for/list ([item lst]
             [i (range (length lst))]
             #:when (zero-fun? item))
            i))

(define (put-random-piece lst)
  (if (avail? lst)
    (if (list? lst)
      (let* ([i (choice (get-empty-refs lst avail?))]
             [v (list-ref lst i)])
        (append (take lst i)
                (cons (put-random-piece v) (drop lst (add1 i)))))
      (get-a-piece))
    lst))

(put-random-piece '((0 2 0 0) (2 5 8 16) (0 4 4 8) (2 0 0 0)))

(define (init-board n)
  (put-random-piece (put-random-piece (make-board n))))

(init-board 4)

(define (merge row)
  (cond [(<= (length row) 1) row]
        [(= (first row) (second row))
         (cons (* 2 (first row)) (merge (drop row 2)))]
        [else (cons (first row) (merge (rest row)))]))

(merge '(2 2 2 4 4 4 8))

(define (move-row row v left?)
  (let* ([n (length row)]
         [l (merge (filter (λ (x) (not (zero? x))) row))]
         [padding (make-list (- n (length l)) v)])
    (if left?
      (append l padding)
      (append padding l))))

(define (move lst v left?)
  (map (λ (x) (move-row x v left?)) lst))

(move '((0 2 0 0) (2 4 8 16) (0 4 4 8) (2 0 0 0)) 0 #t)

(define (move-left lst)
  (put-random-piece (move lst 0 #t)))
(define (move-right lst)
  (put-random-piece (move lst 0 #f)))
(define (transpose lsts)
  (apply map list lsts))
(define (move-up lst)
  ((compose1 transpose move-left transpose) lst))
(define (move-down lst)
  ((compose1 transpose move-right transpose) lst))
