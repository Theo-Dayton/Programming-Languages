; Summary: Natural class integer test
(check-print (Natural fromSmall: 10) 10)

; Summary: Natural class zero test
(check-print (Natural fromSmall: 0) 0)

; Summary: Natural class test
(check-print (Natural fromSmall: 500) 500)

; Summary: LargeInteger class integer test
(check-print (LargeInteger fromSmall: 10) 10)

; Summary: LargeInteger class zero test
(check-print (LargeInteger fromSmall: 0) 0)

; Summary: LargeInteger class addition test
(check-print ((LargeInteger fromSmall: 10)
              addLargePositiveIntegerTo (LargeInteger fromSmall: 5)) 15)
