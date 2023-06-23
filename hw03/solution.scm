;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 1


;; (prefix? xs ys) returns true if xs is a prefix of ys

;; laws:
;;   (prefix? '() (cons y ys)) == #t
;;   (prefix? (cons x xs) '()) == $f
;;   (prefix? (cons x xs) (cons y ys)) == #f, when x != y
;;   (prefix? (cons x xs) (cons y ys)) == (prefix? xs ys), when x = y 
;;   ...

(define prefix? (xs ys)
    (if (and (not (null? xs)) (null? ys))
        #f
        (if (null? xs)
            #t
                (if
                    (= (car xs) (car ys))
                    (prefix? (cdr xs) (cdr ys))
                    #f
                    )
            )
        )
    )

        ;; replace next line with good check-expect or check-assert tests
    ;(check-assert (prefix? '() '(1 2 3)))
        (check-assert (not (prefix? '(1 2 3) '())))
        (check-assert (prefix? '(1 2 3) '(1 2 3 4 5)))
        (check-assert (not (prefix? '(1 2 4) '(1 2 3 4 5))))

;; (contig-sublist? xs ys) Determines whether the list xs is a contiguous
;; subsequence of the list ys

(define contig-sublist? (xs ys)
    (if (and (not (null? xs)) (null? ys))
        #f
        (if (null? xs)
            #t
            (if (= (car xs) (car ys))
                (if (prefix? (cdr xs) (cdr ys))
                    #t
                    (contig-sublist? xs (cdr ys))
                    )
                (contig-sublist? xs (cdr ys))
                )
            )
        )
    )

        ;; replace next line with good check-expect or check-assert tests
        (check-assert (contig-sublist? '() '()))
        (check-assert (not (contig-sublist? '(1 3 4) '())))
        (check-assert (contig-sublist? '(4 5 6) '(1 2 3 4 5 6 7 8)))
        (check-assert (not (contig-sublist? '(2 4 6) '(1 2 3 4 5 6))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 8


;; (mirror xs) returns a list in which every list in xs is mirrored, as well
;; as reversed

;; laws:
;;   (mirror '()) == '()
;;   (mirror (cons x xs)) == (append (mirror (cdr xs)) car xs), where car xs is
;;   an atom
;;   (mirror (cons x xs)) == (append (mirror (cdr xs)) (mirror (car xs))), where
;;   xs is not an atom
;;   ...

(define mirror (xs)
    (if (null? xs)
        xs
        (append (mirror (cdr xs))
                    (list1 (if (atom? (car xs))
                                (car xs)
                                (mirror (car xs))
                                    )
                        )
            )
        )
    )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (mirror '(1 2 3 4 5)) '(5 4 3 2 1))
        (check-expect (mirror '((3 (b 9)) (x q) y)) '(y (q x) ((9 b) 3)))
        (check-expect (mirror '()) '())



;; (flatten xs) given a list xs, erases its internal brackets

;; laws:
;;   (flatten '()) == '()
;;   (flatten (cons x xs)) == (append (flatten x) (flatten xs)),
;;   where x is not an atom
;;   (flatten (cons x xs)) == (append (list1 x) (flatten xs)),
;;   where x is an atom
;;   ...

(define flatten (xs)
        (if (null? xs)
            '()
            (append (if (atom? (car xs))
                        (list1 (car xs))
                        (flatten (car xs))
                        )
                    (flatten (cdr xs))
                )
            )
    )

        ;; replace next line with good check-expect or check-assert tests
      (check-expect (flatten '((This Is) (A Unit) Test)) '(This Is A Unit Test))
        (check-expect (flatten '((((((((x))))))))) '(x))
        (check-expect (flatten '((1 2) (3 4) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 31


;; (takewhile p? xs) takes predicate p and list xs, and returns the longest
;; prefix of xs in which all elements satisfy p

;; laws:
;;   (takewhile p '()) == '()
;;   (takewhile p (cons x xs)) == '(), where x does not satisfy p
;;   (takewhile p (cons x xs)) == (append (list1 x) (takewhile p 
;;   xs)), where x satisfies p
;;   ...

(define takewhile (p xs)
    (if (null? xs)
        '()
        (if (p (car xs))
            (append (list1 (car xs)) (takewhile p (cdr xs)))
            '()
            )
        )
    )

;; (odd? x) takes number x and returns true if it is odd
(define odd? (x)
    (!= (mod x 2) 0)
    )

        ;; replace next line with good check-expect or check-assert test
        (check-expect (takewhile odd? '()) '())
        (check-expect (takewhile odd? '(1 3 5 7 10 12 14)) '(1 3 5 7))



;; (dropwhile p? xs) takes predicate p and list xs, and returns the list with
;; the longest prefix of xs (in which all elements satisfy p) removed

;; laws:
;;   (dropwhile '()) == '()
;;   (dropwhile p (cons x xs)) == (dropwhile p xs), where x satisfies p
;;   (dropwhile p (cons x xs)) == (cons x xs), where x does not satisfy p
;;   ...

(define dropwhile (p xs)
    (if (null? xs)
        '()
        (if (p (car xs))
            (dropwhile p (cdr xs))
            xs
            )
        )
    )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (dropwhile odd? '()) '())
        (check-expect (dropwhile odd? '(1 3 5 7 10 12 14)) '(10 12 14))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise B


;; (take n xs) takes natural number n and list xs. Returns the longest prefix of
;; xs which containst at most n elements

;; laws:
;;   (take n '()) == '()
;;   (take 0 (cons x xs)) == '()
;;   (take n (cons x xs)) == (cons x (take (- n 1) xs)), where n < length of xs
;;   (take n (cons x xs)) == (cons x xs), where n >= length of xs
;;   ...

(define take (n xs)
    (if (null? xs)
        '()
        (if (= 0 n)
            '()
            (cons (car xs) (take (- n 1) (cdr xs)))
            )
        )
    )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (take 0 '(1 2 3 4)) '())
        (check-expect (take 5 '()) '())
        (check-expect (take 3 '(1 2 3 4 5)) '(1 2 3))
        (check-expect (take 10 '(1 2 3 4 5 6)) '(1 2 3 4 5 6))



;; (drop n xs) <does what exactly> (replace with function contract)

;; laws:
;;   (drop n '()) == '()
;;   (drop 0 (cons x xs)) == (cons x xs)
;;   (drop n (cons x xs)) == (drop (- n 1) xs), where n < length of xs
;;   (drop n (cons x xs)) == '(), where n >= length of xs
;;   ...

(define drop (n xs)
    (if (null? xs)
        '()
        (if (= n 0)
            xs
            (drop (- n 1) (cdr xs))
            )
        )
    )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (drop 5 '()) '())
        (check-expect (drop 10 '(1 2 3 4 5)) '())
        (check-expect (drop 4 '(1 2 3 4 5 6 7)) '(5 6 7))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise C


;; (zip xs ys) Takes in two lists of equal length, xs ys. Converts xs ys into
;; a list of pairs by associating the values in both lists. 

;; laws:
;;   (zip '() '()) == '()
;;   (zip (cons x xs) (cons y ys)) == (append (bind x y '()) (zip xs ys)) 
;;   ...

(define zip (xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (append (bind (car xs) (car ys) '()) (zip (cdr xs) (cdr ys)))
        )
    )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (zip '() '()) '())
        (check-expect (zip '(1 3 5) '(2 4 6)) '((1 2) (3 4) (5 6)))



;; (unzip ps) Takes in a list of pairs ps and converts it to a pair of lists

(define unzip (ps)
    (list2 (map car ps) (map cadr ps))
    )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (unzip '()) (list2 '() '()))
        (check-expect (unzip '((1 2) (3 4) (5 6))) '((1 3 5) (2 4 6)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise D


;; (arg-max f xs) Takes in function f and list xs. f maps a value in set A to a
;; number, and a nonempty list xs of values in set A. Returns element x in xs
;; such that (f x) is as large as possible

;; laws:
;;   (arg-max f (cons x xs)) == (arg-max f xs), where x is not the largest (f x)
;;   (arg-max f (cons x xs)) == x, where x is the largest (f x)
;;   ...

(define arg-max (f xs)
    (if (= (max* (map f xs)) (f (car xs)))
        (car xs)
        (arg-max f (cdr xs))
        )
    )

;; (factor-of-20) takes in number x, if it is a factor of 20, returns the number
;; itself, else returns 0
(define factor-of-20 (x)
    (if (= (mod 20 x) 0)
        x
        0
        )
    )
        ;; replace next line with good check-expect or check-assert tests
        (check-expect (arg-max factor-of-20 '(15 18 5 10 2 4)) 10)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise E


;; (rightmost-point ps) Takes nonempty list ps of point records, and returns
;; the one with the largest x coordinate


(record point [x y])

(define point-x (ps)
    (ps-x it)
    )
;;
    (define rightmost-point (ps)
        (if (= (max* (map point-x ps)) (point-x (car ps)))
            (car ps)
            (rightmost-point point-x (cdr ps))
            )
        )

        ;; replace next line with good check-expect or check-assert tests
        ;;(check-expect (rightmost-point (list1 ((make-point '15 '15) 
        ;;(make-point '18 '18)))) '())
