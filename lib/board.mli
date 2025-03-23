open Block

type t
(**The abstract type representing the game board.*)

type cell = | Empty | Block of Block.color
(**The type representing a board cell.*)

val create_board : unit -> t
(**[create_board ()] creates an empty 8x8 board.*)

val get_cell : t -> (int * int) -> cell
(**[get_cell board (x, y)] is the cell at [(x, y)].*)

val set_cell : t -> (int * int) -> cell -> unit
(**[set_cell board (x, y) cell] sets the cell at [(x, y)] in [board] to [cell].*)

val is_valid_pos : t -> (int * int) -> bool
(**[is_valid_pos board (x, y)] is true if [(x, y)] is a valid position in [board].*)

val is_empty_cell : t -> (int * int) -> bool
(**[is_empty_cell board (x, y)] is true if the cell at [(x, y)] in [board] is [Empty].*)