(class Natural ;; page 678
   [subclass-of Magnitude]

   ;; private class method
   (class-method base () ((set base 8) base)) ;; steps 2 and 35

   ;; private methods: internal consistency checking
   (method invariant () (self subclassResponsibility)) ;;; step 5
   (method validated ()                                ;;; step 6
      ((self invariant) ifFalse: {(self printrep)
                                  (self error: 'invariant-violated )})
      self)

   (method isZero  () (self subclassResponsibility))

   (class-method fromSmall: (anInteger) [locals x x0]  ;;; step 8
      (
         (isZero: anInteger) ifTrue:ifFalse:
      {(NatZero new)}
      {((anInteger < base)ifTrue:ifFalse:
      {((NatNonzero new) first:rest: anInteger 0)}
      {
         (set x (anInteger div: base)) (set x0 (anInteger - x))
         ((NatNonzero new) first:rest: x x0)
      }
      )})) 

   (class-method first:rest: (anInteger aNatural) ;;; step 9
      ( ((anInteger = 0) & (aNatural = 0))  ifTrue:ifFalse:
         {(NatZero new)}
         {((NatNonzero new) first:rest: anInteger aNatural)}
      ))

   ;;; step 10 onward: obligations start alternating
   ;;; between subclasses (below) and superclass

   (method printrep () (self subclassResponsibility))

   (method divBase   () (self subclassResponsibility))
   (method modBase   () (self subclassResponsibility))
   (method timesBase () (self subclassResponsibility))

   ;;;; ADDITION ;;;;

   (method plus:carry: (aNatural c)
      (self subclassResponsibility ))
   (method + (aNatural) (self leftAsExercise))  ;;; steps 18 and 19

   ;;;; DIVISION AND DECIMALS ;;;;


   (method sdivmod:with: (n aBlock) (self subclassResponsibility))
   (method sdiv: (n) (self sdivmod:with: n [block (q r) q]))
   (method smod: (n) (self sdivmod:with: n [block (q r) r]))

   (method decimal () (self leftAsExercise))  ;;; step 22
   (method print ()      ;;; steps 23 and 24:
      (self printrep))   ;;; change to print-with-decimal
   (method print-with-decimal ()
      ((self decimal) do: [block (x) (x print)]))

   ;;;; COMPARISON ;;;;

   (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock) 
      (self subclassResponsibility))
  (method compare-symbol: (aNat) ;; help debug comparisons
    (self compare:withLt:withEq:withGt: aNat {'LT} {'EQ} {'GT }))

   ;; obligations inherited from Magnitude (steps 26 and 27)
   (method = (aNatural) (self leftAsExercise))
   (method < (aNatural) (self leftAsExercise))

   ;;;; SUBTRACTION ;;;;

   (method minus:borrow: (aNatural c)
      (self subclassResponsibility))
   (method subtract:withDifference:ifNegative: (aNatural diffBlock exnBlock)
      (self leftAsExercise)) ;;; step 29
   ;; remaining arithmetic is built on key arithmetic
   (method - (aNatural) ;;; step 30
      (self subtract:withDifference:ifNegative:
            aNatural
            [block (x) x]
            {(self error: 'Natural-subtraction-went-negative )}))

   ;;;; MULTIPLICATION ;;;;

   (method timesDigit:plus: (d r)
      ;; answer `self * d + r`, where d and r are small integers < base
      (self subclassResponsibility))
   (method * (aNatural) (self subclassResponsibility))
)

(class NatZero 
   [subclass-of Natural]

   (method invariant () true) ;;; step 5

   (method isZero () true) ;;; steps 7 and 11

   (method printrep () (0 print)) ;;; steps 12, 13, and 14

   (method divBase   () 0) ;;; step 15
   (method modBase   () 0)
   (method timesBase () 0)

   (method * (aNatural) (self leftAsExercise)) ;;; step 16
   (method sdivmod:with: (n aBlock) (self leftAsExercise))
   (method plus:carry: (aNatural c)
      (self leftAsExercise))
   (method minus:borrow: (aNatural c)
      (self leftAsExercise))
   (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock) 
      (self leftAsExercise))

   (method timesDigit:plus: (d r) ;;; steps 31 and 32
      (self leftAsExercise))
)

(class NatNonzero 
   [subclass-of Natural]
   [ivars x x0] ;;; step 4

   (method invariant () 
         (((x0 > 0) & (x0 < base)) ifTrue:ifFalse:
         {(((x0 != 0) & (x != 0)) ifTrue:ifFalse: {true} {false})}
         {false})) ;;; step 5

   (method isZero () false) ;;; steps 7 and 11
   (method first:rest: (anInteger aNatural) ;;; step 10
      (set x anInteger) (set x0 aNatural)
      self)

   (method printrep () ;;; steps 12, 13, and 14
      (x print) (', print) (x0 print))

   (method divBase   () (self leftAsExercise)) ;;; step 15
   (method modBase   () (self leftAsExercise))
   (method timesBase () [locals xn]
                        (set xn (base * (x0 + (x * base))))
                        )

   (method plus:carry: (n c) ;;; step 17
      (self leftAsExercise))

   (method sdivmod:with: (n aBlock) ;;; steps 20 and 21
      (self leftAsExercise))

   (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock) 
      (self leftAsExercise)) ;;; steps 25 and 27

   (method minus:borrow: (aNatural c) ;; step 28
      (self leftAsExercise))

   (method timesDigit:plus: (d r) ;;; steps 31 and 32
      (self leftAsExercise))

   (method * (aNatural) (self leftAsExercise)) ;;; steps 33 and 34
)


; For testing naturals
(class DebugNat
  [subclass-of Object]
  [ivars nat] ; a natural number
  (class-method of: (aNat) ((self new) init: aNat))
  (method init: (n) (set nat n) self) ; private
  (method print () (nat printrep))
)


; additional methods for naturals just for testing
(Natural addSelector:withMethod: 'squared
  (compiled-method () (self * self)))
(Natural addSelector:withMethod: 'coerce:
  (compiled-method (i) (Natural fromSmall: i)))
(Natural addSelector:withMethod: 'raisedToInteger:
  (Number compiledMethodAt: 'raisedToInteger: ))




        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Put your unit tests for natural numbers here
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;(check-print (DebugNat of: (Natural fromSmall: 0))
        ;;       0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class LargeInteger ;; page 677
  [subclass-of Integer]
  [ivars magnitude]

  (class-method withMagnitude: (aNatural)
      ((self new) magnitude: aNatural))
  (method magnitude: (aNatural) ; private, for initialization
      (set magnitude aNatural)
      self)

  (method magnitude () magnitude)

  (class-method fromSmall: (anInteger) ;;; step 16
     ((anInteger isNegative) ifTrue:ifFalse: 
        {(((self fromSmall: 1) + (self fromSmall: ((anInteger + 1) negated)))
          negated)}
        {((LargePositiveInteger new) magnitude: 
                 (Natural fromSmall: anInteger))}))

  ;; methods implemented for you 

  (method asLargeInteger () self)
  (method = (anInteger) ((self - anInteger)     isZero))
  (method < (anInteger) ((self - anInteger) isNegative))

  (method div: (n) (self sdiv: n))
  (method mod: (n) (self smod: n))

  (method sdiv: (n) (self subclassResponsibility))

  ;; methods you will implement

  (method isZero () (self subclassResponsibility)) ;;; step 4

  ;;;; steps 5, 6, and 10 ;;;;

  (method isNegative         () (self subclassResponsibility))
  (method isNonnegative      () (self subclassResponsibility))
  (method isStrictlyPositive () (self subclassResponsibility))

  ;; private methods, page 676
  ;;    (you will implement each one in each subclass)

  ;;;; steps 12 and 13 ;;;;

  (method multiplyBySmallInteger: (aSmallInteger)
    (self subclassResponsibility))

  (method multiplyByLargePositiveInteger: (aLargePositiveInteger)
    (self subclassResponsibility))

  (method multiplyByLargeNegativeInteger: (aLargeNegativeInteger)
    (self subclassResponsibility))

  ;;;; steps 14 and 15 ;;;;

  (method addSmallIntegerTo: (aSmallInteger) 
    (self subclassResponsibility))

  (method addLargePositiveIntegerTo: (aLargePositiveInteger)
    (self subclassResponsibility))

  (method addLargeNegativeIntegerTo: (aLargeNegativeInteger)
    (self subclassResponsibility))

  ;;;; steps 17 and 18 (public method `smod:`) ;;;;

  (method smod: (n) (self leftAsExercise)) 

  ;; diagnostics
  (method invariant () (magnitude isKindOf: Natural))
  (method validated ()
     ((self invariant) ifFalse: {(self error: 'invariant-violated )})
     self)

)


(class LargePositiveInteger
  [subclass-of LargeInteger]

  ;; short division (already implemented for you)
  (method sdiv: (anInteger)
    ((anInteger isStrictlyPositive) ifTrue:ifFalse: 
       {(LargePositiveInteger withMagnitude:  (magnitude sdiv: anInteger))}
       {((((self - (LargeInteger fromSmall: anInteger)) -
                                                  (LargeInteger fromSmall: 1))
             sdiv: (anInteger negated))
            negated)}))

  ;; your chosen invariant for this subclass
  (method invariant ()  ;;; step 3
       ((super invariant) &
        (self leftAsExercise)))

  ;; you add other methods here

)

(class LargeNegativeInteger
  [subclass-of LargeInteger]

  ;; short division (already implemented for you)
  (method sdiv: (anInteger)
    ((self negated) sdiv: (anInteger negated)))

  ;; your chosen invariant for this subclass
  (method invariant ()    ;;; step 3
       ((super invariant) &
        (self leftAsExercise)))

  ;; you add other methods here
)



;;;;;;;; for testing addition with overflow

(Integer addSelector:withMethod: 'times10
  ;; answers the receiver times 10, using only addition
  (compiled-method () [locals two four]
     (set two (self + self))
     (set four (two + two))
     ((four + four) + two)))

        (check-print (((10101 times10) times10) times10) 10101000)

