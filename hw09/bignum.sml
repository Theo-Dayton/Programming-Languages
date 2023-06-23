signature BIGNUM = sig
   type bigint
   exception BadDivision                (* contract violation for sdivmod *)

   val ofInt   : int -> bigint
   val negated : bigint -> bigint       (* "unary minus" *)
   val <+>     : bigint * bigint -> bigint
   val <->     : bigint * bigint -> bigint
   val <*>     : bigint * bigint -> bigint
   val sdivmod : bigint * int -> { quotient : bigint, remainder : int }

     (* Contract for "short division" sdivmod, which is defined only on
        *nonnegative* integers:

        Provided 0 < d <= K and n >= 0,
          sdivmod (n, d) returns { quotient = q, remainder = r }
        such that
          n == q /*/ ofInt d /+/ ofInt r
          0 <= r < d

        Given a negative n or d <= 0, sdivmod (n, d) raises BadDivision.

        Given d > K, the program halts with a checked run-time error.
        The constant K depends on the number of bits in a machine integer,
        so it is not specified, but it is known to be at least 10.
     *)
        
   val compare : bigint * bigint -> order

   val toString : bigint -> string

     (* toString n returns a string giving the natural 
        representation of n in the decimal system.  If n is
        negative, toString should use the conventional minus sign "-".

        And when toString returns a string containing two or more digits,
        the first digit must not be zero.
     *)

  val invariant : bigint -> bool  (* representation invariant---instructions
  below *)

end

signature NATURAL = sig
   type nat
   exception Negative     (* the result of an operation is negative *)
   exception BadDivisor   (* divisor zero or negative *)

   val ofInt   : int -> nat          (* could raise Negative *)
   val /+/     : nat * nat -> nat
   val /-/     : nat * nat -> nat    (* could raise Negative *)
   val /*/     : nat * nat -> nat
   val sdivmod : nat * int -> { quotient : nat, remainder : int }

     (* Contract for "Short division" sdivmod:

        Provided 0 < d <= K,
          sdivmod (n, d) returns { quotient = q, remainder = r }
        such that
          n == q /*/ ofInt d /+/ ofInt r
          0 <= r < d

        Given a d <= 0, sdivmod (n, d) raises BadDivisor.

        Given d > K, the program halts with a checked run-time error.
        The constant K depends on the number of bits in a machine integer,
        so it is not specified, but it is known to be at least 10.
     *)
        
   val compare : nat * nat -> order

   val decimal : nat -> int list

     (* decimal n returns a list giving the natural decimal
        representation of n, most significant digit first.
        For example,  decimal (ofInt 123) = [1, 2, 3]
                      decimal (ofInt 0)   = [0]
        It must never return an empty list.
        And when it returns a list of two or more digits,
        the first digit must not be zero.
     *)

  val invariant : nat -> bool  (* representation invariant---instructions
  below *)

end