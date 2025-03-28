open Blockblast
open Raylib

type props = {
  active_blocks : (int * int, Block.t) Hashtbl.t;
  queued_blocks : Block.t list;
  board : Board.t;
}

type t = {
  props : props;
  drag_block : (int * Block.t * Vector2.t) option;
  score : int;
  game_over : bool;
}

let init props = { props; drag_block = None; score = 0; game_over = false }
