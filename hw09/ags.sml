functor AgsFun (structure Game : GAME) :> 
   AGS 
     where type Game.Move.move = Game.Move.move
     and   type Game.state     = Game.state
= struct
  structure Game = Game

  ... helper functions, if any ...

  fun advice state = ...
end