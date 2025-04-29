open Block

type t
(**The abstract type representing the game board.*)

type cell =
  | Empty
  | Block of Block.color  (**The type representing a board cell.*)

val create_board : unit -> t
(**[create_board ()] creates an empty 8x8 board.*)

val get_cell : t -> int * int -> cell
(**[get_cell board (r, c)] is the cell at [(r, c)].*)

val set_cell : t -> int * int -> cell -> unit
(**[set_cell board (r, c) cell] sets the cell at [(r, c)] in [board] to [cell].*)

val is_valid_pos : t -> int * int -> bool
(**[is_valid_pos board (r, c)] is true if [(r, c)] is a valid position in
   [board].*)

val is_empty_cell : t -> int * int -> bool
(**[is_empty_cell board (r, c)] is true if the cell at [(r, c)] in [board] is
   [Empty].*)

val place_block : t -> Block.t -> int * int -> unit
(**[place_block board block (r, c)] places [block] in [board] at position
   [(r, c)], aligning with the top left of [block], if [block] fits in that
   spot. *)

val can_place_block : t -> Block.t -> int * int -> bool
(**[can_place_block board block (r, c)] is true if [block] can be placed in
   [board] at position [(r, c)], aligning with the top left of [block].*)
