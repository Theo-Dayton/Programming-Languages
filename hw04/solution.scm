;;;
;;; Exercise 28 b
;;;

;; max*, takes a non-empty list of integers, and returns the maximum

(define max* (xs)
    (foldl max (car xs) (cdr xs)))

        ;; Unit tests
        (check-expect (max* '(0)) 0)
        (check-expect (max* '(9 30 12)) 30)

;;;
;;; Exercise 28 e
;;;

;; sum, takes a non-empty list of integers, and returns the sum

(define sum (xs)
    (foldl + (car xs) (cdr xs)))

        ;; Unit tests
        (check-expect (sum '(0)) 0)
        (check-expect (sum '(9 30 12)) 51)

;;;
;;; Exercise 28 f
;;;

;; product, takes a non-empty list of integers, and returns the product

(define product (xs)
    (foldl * (car xs) (cdr xs)))

        ;;Unit tests
        (check-expect (product '(0)) 0)
        (check-expect (product '(9 30 12)) 3240)

;;;
;;; Exercise 29 a
;;;

;; append, takes two lists and appends them

(define append (xs ys)
    (foldr cons xs ys))

        (check-expect (append '() '()) '())
        (check-expect (append '(d e f) '(a b c)) '(a b c d e f))

;;;
;;; Exercise 29 c
;;;

;; (reverse xs), which takes in a list and reverses it

(define reverse (xs)
    (foldl cons '() xs))

        (check-expect (reverse '()) '())
        (check-expect (reverse '(a b c)) '(c b a))

;;;
;;; Exercise 30
;;;

;; (map f xs), applies function f to every element of list xs and returns the
;; updated list

(define map (f xs)
    (foldl (lambda (y ys) (cons (f y) ys)) '() xs))

        (check-expect (map sum '()) '())
        (check-expect (map sum '((3 3 3) (5 5 10) (20 20 5))) '(45 20 9))

;; (filter (p? xs)) applies predicate p to every element of list xs and returns
;; the list of elements that return true for the predicate

(define filter (p? xs)
    (foldr (lambda (y ys)
                (if (p? y) (cons y ys) ys)) '() xs))

;; (odd? x) takes number x and returns true if it is odd
(define odd? (x)
    (!= (mod x 2) 0))

        (check-expect (filter odd? '()) '())
        (check-expect (filter odd? '(1 2 3 4 5)) '(1 3 5))

;; (exists? (p? xs)) applies predicate p to every element of list xs and
;; returns true if any element returned true for the predicate

(define exists? (p? xs)
    (foldl (lambda (x y)
                (if y y x)) #f (map p? xs)))

        (check-assert (not (exists? odd? '())))
        (check-assert (not (exists? odd? '(2 4 6 8))))
        (check-assert (exists? odd? '(1 2 3 4 5)))

;; (all? (p? xs)) applies predicate p to every element in list xs and returns
;; true if every element returned true for the predicate

(define all? (p? xs)
    (foldl (lambda (x y)
                (if y x y)) #t (map p? xs)))

        (check-assert (all? odd? '()))
        (check-assert (not (all? odd? '(1 3 5 7 9 10))))
        (check-assert (all? odd? '(1 3 5 7 9)))

;;;
;;; Exercise 38 a
;;;

 (define member? (x s) (s x))

;; (evens), set which containts all even integers

(val evens (lambda (x) (if (number? x) (= 0 (mod x 2)) #f)))

;;;
;;; Exercise 38 b
;;;

;; (two-digits), set which contains all positive two-digit numbers

(val two-digits (lambda (x) (if (number? x) (&& (< x 100) (> x 9) ) #f)))

;;;
;;; Exercise 38 c
;;;

;; (add-element (x xs)) adds element x to set xs and returns the result

;; (member? x (add-element x s)) == #t
;; (member? x (add-element y s)) == (x s), where (not (equal? y x))
;; (member? x (union s1 s2))     == (|| (s1 x) (s2 x))
;; (member? x (inter s1 s2))     == (&& (s1 x) (s2 x))
;; (member? x (diff  s1 s2))     == (&& (not (s2 x) (s1 x)))

(define add-element (x xs)
        (lambda (y) (|| (equal? x y) (xs y))))

        (check-assert (member? 9 (add-element 9 evens)))
        (check-assert (member? 9 (add-element 9 two-digits)))

;; (inter (s1 s2)), takes in two sets s1 and s2, then returns their intersection

(define inter (s1 s2)
        (lambda (x) (&& (s1 x) (s2 x))))

        (check-assert (member? 20 (inter two-digits evens)))
        (check-assert (not (member? 8 (inter two-digits evens))))

;; (diff (s1 s2)), takes in two sets s1 and s2, and returns every element in s1
;; that is not in s2

(define diff (s1 s2)
        (lambda (x) (&& (not (s2 x)) (s1 x))))

        (check-assert (member? 8 (diff evens two-digits)))
        (check-assert (not (member? 10 (diff evens two-digits))))

;;;
;;; Exercise 38 d
;;;

(record set-ops [emptyset member? add-element union inter diff])

(define set-ops-from (eq?)
  (let ([emptyset    (lambda (x) #f)]
        [member?     (lambda (x s) (s x))]
        [add-element (lambda (x s) (lambda (y) (|| (eq? x y) (xs y))))]
        [union       (lambda (s1 s2) (lambda (x) (|| (s1 x) (s2 x))))]
        [inter       (lambda (s1 s2) (lambda (x) (&& (s1 x) (s2 x))))]
        [diff        (lambda (s1 s2) (lambda (x) (&& (not (s2 x)) (s1 x))))])
    (make-set-ops emptyset member? add-element union inter diff)))

(val atom-set-ops     (set-ops-from =))
(val atom-emptyset    (set-ops-emptyset    atom-set-ops))
(val atom-member?     (set-ops-member?     atom-set-ops))
(val atom-add-element (set-ops-add-element atom-set-ops)) 
(val atom-union       (set-ops-union       atom-set-ops))
(val atom-inter       (set-ops-inter       atom-set-ops))
(val atom-diff        (set-ops-diff        atom-set-ops))

        (check-assert (function? set-ops-from))
        (check-assert (set-ops? atom-set-ops))
        (check-assert (not (set-ops? atom-emptyset)))
        (check-assert (atom-member? 2 (atom-add-element 2 '())))
        (check-assert (not (atom-member? 8 (atom-inter evens two-digits))))
        (check-assert (atom-member? 10 (atom-inter evens two-digits)))


;;;
;;; Exercise F
;;;

;; (flip f) takes function f and returns the exact same function, except it
;; expects its arguments in the opposite order

;; laws:
;;   (flip ((o f x) y)) == (f y x)

(define flip (f)
        (lambda (x y) (f y x)))

        (check-expect ((flip <) 3 4) (> 3 4))
        (check-expect ((flip <) 3 3) (> 3 3))
        (check-expect ((flip <) 3 2) (> 3 2))

;;;
;;; Exercise O
;;;

;; (ordered-by? f?) take in comparison function f and returns true if a list is
;; ordered by that comparison function

;; laws:
;;   (ordered-by? f? '()) == #t
;;   (ordered-by? f? (cons x xs)) == #t, when xs == '()
;;   (ordered-by? f? (cons x (cons y zs))) == (&& (f? x y) (ordered-by? (cons y
;;   zs)))

(define ordered-by? (f?)
        (lambda (xs)
                        (if (|| (null? xs) (null? (cdr xs)))
                                #t
                                (&& (f? (car xs) (cadr xs))
                                        (ordered-by? (cdr xs))))))

        (check-assert (ordered-by? <))
        (check-assert ((ordered-by? <) '(1)))
        (check-assert ((ordered-by? <=) '(1 2 3)))
        (check-assert (not((ordered-by? <) '(3 2 1)))) 
        (check-assert ((ordered-by? =) '(3 3 3))) 

;;;
;;; Exercise V
;;;

(define bound-in? (key pairs)
  (if (null? pairs)
      #f
      (|| (= key (alist-first-key pairs))
          (bound-in? key (cdr pairs)))))

(val the-fault list1) ; build a singleton fault set
(val no-faults '())   ; an empty fault set

; ((faults/none) response) == no-faults
(define faults/none ()
    no-faults)

; ((faults/always f) response) == (the-fault f)
(define faults/always (f)
    (the-fault f))

; ... law or laws for faults/equal ...
;;(define faults/equal (key value)
;;    ...)

; ... law or laws for faults/both ...
;;(val faults/both
;;    (let* ([member?  (lambda (x s) (exists? ((curry =) x) s))]
;;           [add-elem (lambda (x s) (if (member? x s) s (cons x s)))]
;;           [union    (lambda (faults1 faults2)
;;                        (foldr add-elem faults2 faults1))])
;;    ...))

; ... law or laws for faults/switch ...
(define faults/switch (key validators)
        (find key validators))

;; regrade validator to help unit test
(val regrade-validator
  (faults/switch 'why
    (bind         'photo
                  (faults/none) '())))

        ;;(check-expect (regrade-validator '([why photo]))'())
        ;;(regrade-validator '([why photo]))