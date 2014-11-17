
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;; problem 1
(define (sequence low high stride)
  (if (> low high) null (cons low (sequence (+ low stride) high stride))))

;;problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;;problem 3
(define (list-nth-mod xs r)
  (if (< r 0) (error "list-nth-mod: negative number")
      (if (null? xs) (error "list-nth-mod: empty list")
          (if (= (remainder r (length xs)) 0) (car xs)
              (list-nth-mod (cdr xs) (- (remainder r (length xs)) 1))))))

;;problem 4
(define (stream-for-n-steps s n)
  (if (= n 0) null
  (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;;problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons 
                 (if (= (remainder x 5) 0)
                     (- 0 x)
                     x)
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;;problem 6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (cons
                 (if (= (remainder x 2) 1)
                     "dan.jpg"
                     "dog.jpg")
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;;problem 7
(define (stream-add-zero ps)
  (letrec ([ f (lambda (s)
                 (cons (cons 0 (car (s))) (lambda() (f (cdr (s))))))])
    (lambda () (f ps))))

;; problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons 
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda() (f (+ n 1)))))])
    (lambda () (f 0))))

;;problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
               (if (>= n (vector-length vec))
                   #f
                   (if (and (pair? (vector-ref vec n))
                            (equal? v (car (vector-ref vec n))))
                       (vector-ref vec n)
                       (f (+ n 1)))))])
    (f 0)))
