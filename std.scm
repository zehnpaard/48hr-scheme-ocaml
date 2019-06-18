(define (not x) (if x #f #t))
(define (null? xs) (eqv? xs '()))

(define (list . xs) xs)

(define (id x) x)

(define (flip f) (lambda (x y) (f y x)))

(define (curry f x) (lambda (y) (apply f x y)))
(define (compose f g) (lambda (x) (f (apply g x))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))
