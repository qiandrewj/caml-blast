open Blockblast

type t = {
  board : Board.t;
  available_blocks : (int * int, Block.t) Hashtbl.t;
  queued_blocks : Block.t list;
}

val init : unit -> t
val loop : t -> unit
