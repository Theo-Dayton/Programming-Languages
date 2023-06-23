signature PLAYER = sig
  datatype player  = X | O    (* 2 players called X and O *)
  datatype outcome = WINS of player | TIE

  val otherplayer : player -> player
  val unparse     : player -> string 
  val outcomeString : outcome -> string
end

structure Player :> PLAYER = struct
  datatype player  = X | O
  datatype outcome = WINS of player | TIE

  fun otherplayer X = O
    | otherplayer O = X

  fun unparse X = "X"
    | unparse O = "O"

  fun outcomeString TIE = "a tie"
    | outcomeString (WINS p) = unparse p ^ " wins"
end

signature MOVE = sig
  type move               (* A move---perhaps an (x,y) location *)
  exception Move          (* Raised for invalid moves *)

  (* CREATOR *)
  val parse : string -> move
        (* Converts the given string to a move; If the string 
           doesn't correspond to a valid move, parse raises Move *)

  (* OBSERVERS *)
  val prompt : Player.player -> string
        (* A request for a move from the given player *)
        (* Example: "What square does player O choose?" *)

  val visualize : Player.player -> move -> string
        (* A short string describing a move. 
           Example: "Player X picks up ...".
           The string may not contain a newline. *)
end

signature GAME = sig
  structure Move : MOVE
  type state 

  (* CREATORS *)

  val initial : Player.player -> state
    (* Initial state for a game in which 
       the given player moves first. *)

  (* PRODUCERS *)

  val makemove: state -> Move.move -> state
    (* Returns the state obtained by making the 
       given move in the given state.  Raises Move.Move
       if the state is final or if the given move is not
       legal in the given state. *)

  (* OBSERVERS *)

  val visualize : state -> string
    (* A string representing the given state.  
       The string must show whose turn it is, 
       and it must end in a newline. *)

  val whoseturn  : state -> Player.player
    (* Given a _non-final_ state, returns the player
       whose turn it is to move.    When given a
       final state, behavior is unspecified. *)

  val isOver : state -> bool
    (* Tells if the given state is final. *)

  val outcome : state -> Player.outcome option
    (* When given a final state, returns SOME applied to the outcome.
       When given a non-final state, returns NONE. *)

  val legalmoves : state -> Move.move list
    (* Lists all moves that are valid inputs to `makemove` in the
       given state.  The list is empty if and only if the given
       state is final. *)

end

signature AGS = sig
  structure Game : GAME
  type advice = { recommendation : Game.Move.move option
                , expectedOutcome : Player.outcome
                }
  val advice : Game.state -> advice
    (* Given a non-final state, returns a recommended move,
       plus the expected outcome if the recommendation is followed.
       Given a final state, returns no recommendation,
       plus the outcome that has already been achieved. *)
end

functor SticksFun(val N : int) :> GAME = struct
  (* state is represented by a pair of Player and int *)
  datatype state =
    Empty |
    pair of Player * int 

  val invariant : state -> bool = true

  structure Move = struct
     (* pick one of these two ways to define type `move` *)
     datatype move = ONE | TWO | THREE
  end

  exception Move

  val isOver : state -> bool = 
  
end

structure TestSticks = SticksFun(val N = 14)