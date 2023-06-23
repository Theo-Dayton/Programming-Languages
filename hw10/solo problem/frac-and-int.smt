;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Solution to Exercise 36 a)

(Fraction addSelector:withMethod: '+
        (compiled-method (aNumber) (aNumber addLargeIntegerTo: (self
                                                                asInteger))))

(SmallInteger addSelector:withMethod: 'addLargeIntegerTo:
        (compiled-method (anInteger)
                ((primitive addWithOverflow self anInteger
                        {((self asLargeInteger) + anInteger)}) value)))

((1 / 2) + 3)

((1 / 2) + 500)

((4 / 2) + 500)