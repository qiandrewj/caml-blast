open Blockblast

type t = {
  board : Board.t;
  active_blocks : (int * int, Block.t) Hashtbl.t;
  mutable queued_blocks : Block.t option array;
  dragged_block : (Block.t * (int * int)) option;
  mouse_pos : int * int;
  score : int;
  game_over : bool;
}
(**The abstract type representing the playing game state.*)

val init : unit -> t
(**[init ()] creates an initial game state.*)

val loop : t -> unit
(**[loop state] updates [state] for a time step.*)
