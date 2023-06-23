;; Homework 7

;;
;; Exercise TD
;;


(val drop
    (type-lambda ['a]
        (letrec
            [([drop-mono : (int (list 'a) -> (list 'a))]
                (lambda ([x : int] [xs : (list 'a)])
                    (if ([@ = int] x 0)
                        xs
                        (drop-mono (- x 1) ([@ cdr 'a] xs)))))]
                drop-mono)))

(val takewhile
    (type-lambda ['a]
        (letrec
            [([takewhile-mono : (('a -> bool) (list 'a) -> (list 'a))]
                (lambda ([p? : ('a -> bool)] [xs : (list 'a)])
                    (if (p? ([@ car 'a] xs))
                        ([@ cons 'a] ([@ car 'a] xs)(takewhile-mono p?
                                                            ([@ cdr 'a] xs)))
                        [@ '() 'a])))]
                takewhile-mono)))


 (check-type drop (forall ['a] (int (list 'a)-> (list 'a))))
 (check-type takewhile (forall ['a] (('a -> bool) (list 'a)-> (list 'a))))