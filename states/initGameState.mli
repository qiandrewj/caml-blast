open Blockblast

type t = {
  board : Board.t;
  active_blocks : (int * int, Block.t) Hashtbl.t;
  queued_blocks : Block.t list;
  dragged_block : (Block.t * (int * int)) option;
  mouse_pos : int * int;
  score : int;
  game_over : bool;
}

val init : unit -> t
val loop : t -> unit
