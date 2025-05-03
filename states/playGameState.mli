open Blockblast

type clear_animation = {
  rows : int list;
  cols : int list;
  progress : float;
  center_pos : int * int;
}
(**The type representing a clearing animation for the board.*)

type t
(**The abstract type representing the playing game state.*)
(* type t = { board : Board.t; active_blocks : (int * int, Block.t) Hashtbl.t;
   mutable queued_blocks : Block.t option array; dragged_block : (Block.t * (int
   * int)) option; mouse_pos : int * int; score_state : Scoring.t; game_over :
   bool; clear_animation : clear_animation option; } *)

val init : unit -> t
(**[init ()] creates an initial game state.*)

val loop : t -> unit
(**[loop state] updates [state] for a time step.*)
