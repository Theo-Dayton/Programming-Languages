;; Homework 5

;;
;; Exercise 46
;;

;;
;; Exercise L
;;

;; (list-of? A? v) takes a predicate A? and arbitrary uScheme value v. returns
;; #t if v is a list of values, each of which satisfies A?. Else returns #f

;; laws:
;;   (list-of? A? '()) == #t
;;   (list-of? A? v) == #f, where v != a list of values
;;   (list-of? A? (cons x xs)) == (all? A? xs), where (A? x) holds true
;;   (listof? A? (cons x xs)) == #f, where (A? x) is not satisfied

(define list-of? (A? v)
    (if (null? v)
        #t
        (if (pair? v)
            (all? A? v)
            #f)))

        (check-assert (list-of? number? '()))
        (check-assert (list-of? number? '(13)))
        (check-assert (not (list-of? number? '13)))
        (check-assert (list-of? number? '(1 2 3)))
        (check-assert (not (list-of? number? '(1 2 3 4 5 6 a))))

;;
;; Exercise F
;;

;; (formula? v) takes arbitrary uscheme value v, if v represents a boolean
;; formula, return #t, else return #f

;; laws:
;;   (formula? 'v) == #t, where 'v is a symbol
;;   (formula? v) == #f, where f is not a symbol 
;;   (formula? (make-not v)) == #t, where v is a formula
;;   (formula? (make-or vs)) == #t, where vs is a list of formulas
;;   (formula? (make-and vs)) == #t, where vs is a list of formulas

(record not [arg])
(record or  [args])
(record and [args])

(define formula? (v)
    (if (symbol? v)
        #t
        (if (not? v)
            (formula? (not-arg v))
            (if (or? v)
                (list-of? formula? (or-args v))
                (if (and? v)
                    (list-of? formula? (and-args v))
                    #f)))))

        (check-assert (formula? 'v))
        (check-assert (formula? (make-or '(v x))))
        (check-assert (formula? (make-not 'v)))
        (check-assert (formula? (make-and '(v x))))
        (check-assert (not (formula? #t)))
        (check-assert (not (formula? (make-or '(v #t)))))
        (check-assert (not (formula? (make-not #t))))
        (check-assert (not (formula? (make-and '(v #t)))))
        
        
;;
;; Exercise E
;;

;; (eval-formula? f env) takes S-expression f and environment env. returns #t if
;; f is satisfied in the environment env, else returns #f 

;; laws:
;;   (eval-formula? v env) == (find v env), where v is a symbol
;;   (eval-formula? (make-not v) env) == (not (eval-formula ()))
;;   (eval-formula? (make-and vs) env) == (all? (eval-formula (make-and fs)))
;;   (eval-formula? (make-or vs) env) == (exists? (eval-formula (make-or vs)))

(define eval-formula (f env)
    (letrec
        ([eval-find
            (lambda (v) 
                (if (&& (not (null? f)) (symbol? f))
                    (find v env)
                    (eval-formula v env)))])
        (if (&& (not (null? f)) (symbol? f))
            (find f env)
                (if (not? f)
                    (not (eval-formula (not-arg f) env))
                    (if (or? f)
                        (exists? eval-find (or-args f))
                        (all? eval-find (and-args f)))))))

(val env-test '((a #t) (b #t) (c #f) (d #f)))

        (check-assert (eval-formula 'a env-test))
        (check-assert (eval-formula 'h env-test))
        (check-assert (eval-formula (make-not 'c) env-test))
        (check-assert (eval-formula (make-or '(b c)) env-test))
        (check-assert (eval-formula (make-and '(a b)) env-test))
        (check-assert (not (eval-formula (make-not 'a) env-test)))
        (check-assert (not (eval-formula (make-or '(c d)) env-test)))
        (check-assert (not (eval-formula (make-and '(b c)) env-test)))

;;
;; Exercise S
;;

;; (find-formula-true-asst (f fail succ)) takes formula f, failure continuation
;; fail and success continuation succ. Searches for an assignment that satisfies
;; f, if it does it calls succ, else it calls fail

;; laws:
;; (find-formula-asst x            bool cur fail succeed) == (symbol? x bool
;; cur fail succeed)
;;                                                   where x is a symbol
;; (find-formula-asst (make-not f)  bool cur fail succeed) ==
;; (find-formula-asst (not-args f) (not bool) cur fail succeed)
;; (find-formula-asst (make-or  fs) #t   cur fail succeed) == (find-any-asst
;; (make-or fs) #t cur fail succeed)
;; (find-formula-asst (make-or  fs) #f   cur fail succeed) == (find-all-asst
;; (make-or fs) #f cur fail succeed)
;; (find-formula-asst (make-and fs) #t   cur fail succeed) == (find-all-asst
;; (make-and fs) #t cur fail succeed)
;; (find-formula-asst (make-and fs) #f   cur fail succeed) == succeed cur fail
;;
;; (find-all-asst '()         bool cur fail succeed) == succed cur fail
;; (find-all-asst (cons f fs) bool cur fail succeed) == (find-formula-asst f cur
;; bool fail (solve-all fs bool cur fail succeed))
;;
;; (find-any-asst '()         bool cur fail succeed) == fail
;; (find-any-asst (cons f fs) bool cur fail succeed) == (find-formula-asst f cur
;; bool fail (find-any-asst fs bool cur fail succeed))
;;
;; (find-formula-symbol x bool cur fail succeed) == (succeed (bind x bool cur)
;; fail), where x is not bound in cur
;; (find-formula-symbol x bool cur fail succeed) == (succeed cur fail),
;; where x is bool in cur
;; (find-formula-symbol x bool cur fail succeed) == fail,
;; where x is (not bool) in cur

(define find-formula-true-asst (f fail succ)
    (letrec
        ([find-formula-asst
            (lambda (f bool cur fail succ)
                (if (symbol? f)
                    (find-formula-symbol f bool cur fail succ)
                    (if (not? f)
                        (find-formula-asst (not-arg f) (not bool) cur fail succ)
                        (if bool
                            (if (or? f)
                                (find-any-asst (or-args f) bool cur fail succ)
                                (find-all-asst (and-args f) bool cur fail succ))
                                (if (and? f)
                                    (find-any-asst (and-args f)
                                        bool cur fail succ)
                                    (succ cur fail))))))]
            [find-all-asst
                (lambda (fs bool cur fail succ)
                    (if (null? fs)
                        (succ cur fail)
                        (find-formula-asst (car fs) bool cur fail
                            (lambda (cont res) (find-all-asst (cdr fs) bool cont
                                                    res succ)))))]
            [find-any-asst
                (lambda (fs bool cur fail succ)
                    (if (null? fs)
                        (fail)
                        (find-formula-asst (car fs) bool cur fail
                            (lambda (succ fail) (find-any-asst (cdr fs) bool cur
                                                    fail succ)) succ)))]
            [find-formula-symbol
                (lambda (x bool cur fail succ)
                    (if (null? (find x cur))
                        (succ (bind x bool cur) fail)
                        (if (= (find x cur) bool)
                            (succ cur fail)
                            (fail))))])
            (find-formula-asst f #t '() fail succ)))


(check-assert (function? find-formula-true-asst))     ; correct name
(check-error (find-formula-true-asst))                ; not 0 arguments
(check-error (find-formula-true-asst 'x))             ; not 1 argument
(check-error (find-formula-true-asst 'x (lambda () 'fail)))   ; not 2 args
(check-error
   (find-formula-true-asst 'x (lambda () 'fail) (lambda (c r) 'succeed) 'z))
   ; not 4 args

(check-error (find-formula-true-asst 'x (lambda () 'fail) (lambda () 'succeed)))
    ; success continuation expects 2 arguments, not 0
(check-error (find-formula-true-asst 'x (lambda () 'fail) (lambda (_)
                                                            'succeed)))
    ; success continuation expects 2 arguments, not 1
(check-error (find-formula-true-asst
                   (make-and (list2 'x (make-not 'x)))
                   (lambda (_) 'fail)
                   (lambda (_) 'succeed)))
    ; failure continuation expects 0 arguments, not 1

(check-expect   ; x can be solved
   (find-formula-true-asst 'x
                           (lambda () 'fail)
                           (lambda (cur resume) 'succeed))
   'succeed)

(check-expect   ; x is solved by '((x #t))
   (find-formula-true-asst 'x
                           (lambda () 'fail)
                           (lambda (cur resume) (find 'x cur)))
   #t)

(check-expect   ; (make-not 'x) can be solved
   (find-formula-true-asst (make-not 'x)
                           (lambda () 'fail)
                           (lambda (cur resume) 'succeed))
   'succeed)

(check-expect   ; (make-not 'x) is solved by '((x #f))
   (find-formula-true-asst (make-not 'x)
                           (lambda () 'fail)
                           (lambda (cur resume) (find 'x cur)))
   #f)

(check-expect   ; (make-and (list2 'x (make-not 'x))) cannot be solved
   (find-formula-true-asst (make-and (list2 'x (make-not 'x)))
                           (lambda () 'fail)
                           (lambda (cur resume) 'succeed))
   'fail)