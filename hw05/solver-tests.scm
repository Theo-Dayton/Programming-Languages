; Template for SAT Solver Test Cases 

(record not [arg])   ;; OK if these are duplicates
(record or  [args])
(record and [args])


; Code testing formula using make-and,make-or and make-not
(val f1 (make-and
           (list3 (make-or (list3 'x 'y 'z))
                  (make-or (list3 (make-not 'x) (make-not 'y) (make-not 'z)))
                  (make-or (list3 'x 'y (make-not 'z))))))
(val s1 '((x #t) (y #f)))


; Test case for test with no solution
(val f2 (make-and (list2 'x (make-not 'x))))
(val s2 'no-solution)

; Case 
(val f3 (make-or (list2 (make-not 'c) (make-or 'd))))
(val s3 '((a #t) (b #t) (c #f) (d #f)))
