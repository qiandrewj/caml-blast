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

val init : unit -> t
val loop : t -> unit
