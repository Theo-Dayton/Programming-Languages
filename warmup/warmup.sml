(*
Name: Theo Dayton
mulNats implemented
*)


(***** Problem A *****)

fun mynull [] = true
    | mynull (x::xs) = false

        val () =
            Unit.checkAssert "[] is null"
            (fn () => mynull [])
        val () =
            Unit.checkAssert "[] is not null"
            (fn () => not (mynull [1]))



(***** Problem B *****)

(*** Question 1 ***)

fun reverse xs = foldl op :: [] xs

        val checkExpectIntList = Unit.checkExpectWith (Unit.listString
                                                        Unit.intString)

        val () = checkExpectIntList "reverse empty list"
        (fn () => reverse [])
        []
        val () = checkExpectIntList "reverse list"
        (fn () => reverse [1,2,3,4,5])
        [5,4,3,2,1]

(*** Question 2 ***)

fun minlist (x::xs) = foldl Int.min x xs
    | minlist [] = raise Match

        val checkExpectInt = Unit.checkExpectWith Unit.intString
        val checkExnInt = Unit.checkExnWith Unit.intString

        val () = checkExnInt "smallest of empty list"
        (fn () => minlist [])
        val () = checkExpectInt "smallest of empty list"
        (fn () => minlist [5,10,3,7,2])
        2
        val () = checkExpectInt "smallest of empty list"
        (fn () => minlist [5,10,0,50,1])
        0

(***** Problem C *****)

exception Mismatch

fun zip (x::xs, y::ys) = (x,y) :: zip (xs,ys)
    | zip ([], []) = []
    | zip _ = raise Mismatch

    val IntPair = Unit.pairString Unit.intString Unit.intString
    val ListPair = Unit.listString IntPair
    val checkExpectListPair = Unit.checkExpectWith ListPair
    val checkExnListPair = Unit.checkExnWith ListPair
    
    val () = checkExpectListPair "zip pair of empty lists"
        (fn () => zip ([], []))
        []
    val () = checkExpectListPair "zip pair of int lists"
        (fn () => zip ([1,2,3], [4,5,6]))
        [(1,4), (2,5), (3,6)]
    val () = checkExnListPair "zip pair of uneven lists"
        (fn () => zip ([1,2,3], [4,5]))

(***** Problem D *****)

fun pairfoldrEq f x ([], []) = x
    | pairfoldrEq f x (y::ys, z::zs) = f (y, z, pairfoldrEq f x (ys, zs))
    | pairfoldrEq f x _ = raise Match

    (* function to unit test *)
    fun addfun (a,b,c) = a + b + c
    
    val () = checkExpectInt "pairfoldrEq on pair of empty lists"
        (fn () => pairfoldrEq addfun 1 ([], []))
        1
    val () = checkExpectInt "pairfoldrEq on pair of int lists"
        (fn () => pairfoldrEq addfun 1 ([0,10], [100,1000]))
        1111
    val () = checkExnInt "pairfoldrEq on pair of uneven lists"
        (fn () => pairfoldrEq addfun 1 ([1], [1,2]))

fun ziptoo (x::xs, y::ys) = pairfoldrEq (fn (a,b,c) => (a,b) :: c) []
                                        (x::xs, y::ys)
    | ziptoo _ = raise Mismatch

    val () = checkExpectListPair "ziptoo pair of int lists"
        (fn () => ziptoo ([1,2,3], [4,5,6]))
        [(1,4), (2,5), (3,6)]
    val () = checkExnListPair "ziptoo pair of uneven lists"
        (fn () => ziptoo ([1,2,3], [4,5]))

(***** Problem E *****)

fun concat xs = foldr (op @) [] xs
    

    val () = checkExpectIntList "concat empty lists"
    (fn () => concat [])
    []
    val () = checkExpectIntList "concat int lists"
    (fn () => concat [[1], [2,3], [4,5,6]])
    [1,2,3,4,5,6]

(***** Problem F *****)

datatype sx
  = SYMBOL of string
  | NUMBER of int
  | BOOL   of bool
  | SXLIST of sx list

fun sxString (SYMBOL s) = s
  | sxString (NUMBER n) = Int.toString n
  | sxString (BOOL b)   = if b then "true" else "false"
  | sxString (SXLIST sxs) = "(" ^ String.concatWith " " (map sxString sxs) ^ ")"

val numbersSx = SXLIST o map NUMBER



fun flattenSyms (BOOL b) = []
    | flattenSyms (NUMBER n) = []
    | flattenSyms (SYMBOL s) = [s]
    | flattenSyms (SXLIST []) = []
    | flattenSyms (SXLIST (x::xs)) = []


(***** Problem G *****)

datatype nat = ZERO
             | TIMES10PLUS of nat * int
            
fun times10plus (ZERO, 0) = ZERO
  | times10plus (m, d)    = TIMES10PLUS (m, d)

(* times10 : nat -> nat *)
fun times10 n = times10plus (n, 0)

(* natOfDigit : int -> nat *)
fun natOfDigit d = times10plus (ZERO, d)

fun rawNatString ZERO = "ZERO"
  | rawNatString (TIMES10PLUS (m, d)) = 
      "(" ^ rawNatString m ^ " * 10 + " ^ Int.toString d ^ ")"

fun flip f (x, y) = f (y, x)

(* natOfDigits : int list -> nat *)
fun natOfDigits ds = foldl (flip times10plus) ZERO ds

fun intOfNat ZERO = 0
    | intOfNat (TIMES10PLUS(nat, int)) = (intOfNat nat * 10) + int

    val natNum = natOfDigit 0

    val () = checkExpectInt "int to nat"
    (fn () => intOfNat natNum)
    0

    val natNum = natOfDigit 123

    val () = checkExpectInt "nat to int"
    (fn () => intOfNat natNum)
    123

fun natOfInt 0 = ZERO
    | natOfInt n = times10plus(natOfInt(n div 10),n mod 10)

fun natString ZERO = ""
    | natString (TIMES10PLUS(nat, int)) = natString nat ^ Int.toString int

    
    val natNum = natOfDigit 123
    val () = Unit.checkExpectWith Unit.stringString "nat to string"
    (fn () => natString natNum)
    "123"



(***** Problem H *****)


fun carryIntoNat (n,0) = n
    | carryIntoNat (ZERO,c) = natOfInt c
    | carryIntoNat (TIMES10PLUS(m, d), c) = 
        times10plus (carryIntoNat (m, (d + c) div 10), 
                                        (d + c) mod 10)

    val natNum4 = natOfDigit 1

    val () = Unit.checkExpectWith rawNatString "carry into nat"
    (fn () => carryIntoNat (natOfDigit 1,0))
    natNum4

fun addWithCarry (n1, ZERO, c) = carryIntoNat (n1,c)
    | addWithCarry (ZERO, n2, c) = carryIntoNat (n2,c)
    | addWithCarry (TIMES10PLUS(m1,d1), TIMES10PLUS(m2,d2), c) =
        let val d = (d1 + d2 + c) mod 10
            val c' = (d1 + d2 + c) div 10
        in times10plus(addWithCarry(m1, m2, c'), d)
        end

    val addWithNum = natOfInt 2
    val addWithNum2 = natOfInt 5
    val addWithNum3 = natOfInt 8
    val () = Unit.checkExpectWith rawNatString "carry add"
    (fn () => addWithCarry(addWithNum, addWithNum2,1))
    addWithNum3

fun addNats (n1, n2) = addWithCarry (n1, n2, 0)

    val addNum = natOfInt 2
    val addNum2 = natOfInt 5
    val addNum3 = natOfInt 7
    val () = Unit.checkExpectWith rawNatString "add nats"
    (fn () => addNats(addNum, addNum2))
    addNum3

exception Negative

fun borrowFromNat (n, 0) = n
    | borrowFromNat ((TIMES10PLUS(m,0)),1) = times10plus(borrowFromNat(m,1),9)
    | borrowFromNat ((TIMES10PLUS(m,d)),1) = times10plus(m,d-1)
    | borrowFromNat _ = raise Negative

    val borrowNum = natOfInt 7
    val borrowNum2 = natOfInt 6
    val () = Unit.checkExpectWith rawNatString "borrow from nat"
    (fn () => borrowFromNat(borrowNum, 1))
    borrowNum2

fun subWithBorrow(n1, ZERO, b) = borrowFromNat(n1,b)
    | subWithBorrow(TIMES10PLUS(m1,d1), TIMES10PLUS(m2,d2), b) =
        let val d = (d1 - d2 - b) mod 10
            val b' = if d1 - d2 - b < 0 then 1 else 0
        in times10plus(subWithBorrow(m1,m2,b'), d)
        end
    | subWithBorrow _ = raise Negative

    val subBorrowNum = natOfInt 7
    val subBorrowNum2 = natOfInt 1
    val subBorrowNum3 = natOfInt 5
    val () = Unit.checkExpectWith rawNatString "sub borrow"
    (fn () => subWithBorrow(subBorrowNum, subBorrowNum2,1))
    subBorrowNum3

fun subNats(n1,n2) = subWithBorrow(n1,n2,0)

val () =
  Unit.checkExnSatisfiesWith natString "1 - 5"
  (fn () => subNats (natOfDigits [1], natOfDigits [5]))
  ("Negative", fn Negative => true | _ => false)





fun mulNats (ZERO, n) = ZERO
    | mulNats (n, ZERO) = ZERO
    | mulNats (TIMES10PLUS(m1,d1), TIMES10PLUS(m2,d2)) = 
        addNats(addNats(natOfInt(d1 * d2), mulNats(natOfInt
         10,addNats(mulNats(m1,m2),mulNats(m2,natOfInt d1)))), mulNats(natOfInt
         100,mulNats(m1,m2)))

    val mulNum = natOfInt 5
    val mulNum2 = natOfInt 25
    val () = Unit.checkExpectWith rawNatString "nat to string"
    (fn () => mulNats(mulNum, mulNum))
    mulNum2


val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)