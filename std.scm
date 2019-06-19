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

(define (foldr f end xs)
  (if (null? xs)
      end
      (f (car xs) (foldr f end (cdr xs)))))

(define (foldl f accum xs)
  (if (null? xs)
      accum
      (foldl f (f accum (car xs)) (cdr xs))))

(define fold foldl)
(define reduce foldr)

(define (unfold f init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold f (f init) pred))))

(define (sum . xs) (fold + 0 xs))
(define (product . xs) (fold * 1 xs))
(define (and . xs) (fold && #t xs))
(define (or . xs) (fold || #f xs))

(define (max first . rest)
  (fold
    (lambda (old new)
      (if (> old new) old new))
    first rest))

(define (min first . rest)
  (fold
    (lambda (old new)
      (if (< old new) old new))
    first rest))

(define (length xs)
  (fold (lambda (x y) (+ x 1)) 0 xs))

(define (reverse xs)
  (fold (flip cons) '() xs))

(define (mem-helper pred op)
  (lambda (acc next)
    (if (and (not acc) (pred (op next)))
        next acc)))
(define (memq x xs)
  (fold (mem-helper (curry eq? x) id)
        #f xs))
(define (memv x xs)
  (fold (mem-helper (curry eqv? x) id)
        #f xs))
(define (member x xs)
  (fold (mem-helper (curry equal? x) id)
        #f xs))
(define (assq x xs)
  (fold (mem-helper (curry eq? x) car)
        #f xs))
(define (assv x xs)
  (fold (mem-helper (curry eqv? x) car)
        #f xs))
(define (assoc x xs)
  (fold (mem-helper (curry equal? x) car)
        #f xs))

(define (map f xs)
  (foldr (lambda (x y)
           (cons (f x) y))
         '() xs))

(define (filter pred xs)
  (foldr (lambda (x y)
           (if (pred x)
               (cons x y)
               y))
         '() xs))
