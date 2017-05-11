#lang racket

;Problem 1: Racket Warm-Up
(define (sum-all xs)
  (if (null? xs)
      0
      (if (pair? (car xs))
          (+ (sum-all (car xs)) (sum-all (cdr xs)))
          (if (null? (car xs))
              0
              (+ (car xs) (sum-all (cdr xs)))
              ))))


;Problem 2: Interpreter in Racket
(define (interp xs)
  (cond
    [(number? xs) xs]
    [(null? xs) xs]
    [(pair? xs) (cond
                  [(pair? (car xs)) (cons (interp (car xs)) (interp (cdr xs)))]
                  [(eq? (car xs) '+) (if (eq? (length xs) 3)
                                   (apply + (interp (cdr xs)))
                                   (error "Error: Not a valid E term"))]
                  [(eq? (car xs) '*) (if (eq? (length xs) 3)
                                   (apply * (interp (cdr xs)))
                                   (error "Error: Not a valid E term"))]
                  [(eq? (length xs) 2) (cons (car xs) (interp (cdr xs)))]
                  [(eq? (length xs) 1) (cons (car xs) (interp (cdr xs)))]
                  [else (error "Error: Not a Valid E Term")])]
    [else (error "Error: Not a valid E Term")]))
      
      
;Problem 3: Syntax Manipulation
(define (TR xs)
  (if (null? xs)
      xs
      (if (pair? (car xs))
          (quasiquote ((unquote (TR (car xs))) (unquote-splicing (TR (cdr xs)))))
          (cond
            [(eq? '+ (car xs)) (quasiquote (* (unquote-splicing (TR (cdr xs)))))]
            [(eq? '* (car xs)) (quasiquote (+ (unquote-splicing (TR (cdr xs)))))]
            [else (quasiquote ((unquote (car xs)) (unquote-splicing (TR (cdr xs)))))])
          )))


;Problem 4: Cons Cell Representations
;(a) |A|node|-> |B|C|
(fprintf (current-output-port) "4. (Answers write in the comment of code)\n\n(a)\n")
(fprintf (current-output-port) "A node-> B C\n\n")
;(b) (cons (cons 'A (cons 'B 'C)) (cons 'B 'C))
(fprintf (current-output-port) "(b)\n(cons (cons 'A (cons 'B 'C)) (cons 'B 'C))\nExecute above code: ")
(cons (cons 'A (cons 'B 'C)) (cons 'B 'C))
;(c)
;(define (func x)
; (cons (cons 'A x) x))
;(func (cons 'B 'C))
(fprintf (current-output-port) "\n(c)\n(define (func x)
  (cons (cons 'A x) x))
(func (cons 'B 'C))\nExecute above code: ")
(define (func x)
  (cons (cons 'A x) x))
(func (cons 'B 'C))

;Problem 5: Currying Revisited
;curry
(define curry (lambda (f)
                (lambda (arg1)
                  (lambda (arg2)
                    (f arg1 arg2)))))
;uncurry
(define uncurry (lambda (f)
                  (lambda (arg1 arg2)
                    ((f arg1) arg2))))


;Problem 6: Trees Revisited
(define (treemap f list)
  (if (null? list)
      list
      (if (pair? (car list))
          (cons (treemap f (car list)) (treemap f (cdr list)))
          (if (number? (car list))
              (cons (f (car list)) (treemap f (cdr list)))
              (cons (car list) (treemap f (cdr list)))
              ))))