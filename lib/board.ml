type cell =
  | Empty
  | Block of Block.color

type t = {
  grid : cell array array;
  size : int;
}

let board_size = 8

let create_board () =
  let b = Array.make_matrix board_size board_size Empty in
  { grid = b; size = board_size }

let size board = board.size

let is_valid_pos board (r, c) =
  r >= 0 && r < board.size && c >= 0 && c < board.size

let get_cell board (r, c) =
  if is_valid_pos board (r, c) then board.grid.(r).(c)
  else raise (Invalid_argument "Out of bounds")

let set_cell board (r, c) cell =
  if is_valid_pos board (r, c) then board.grid.(r).(c) <- cell

let is_empty_cell board (r, c) =
  if is_valid_pos board (r, c) then
    match board.grid.(r).(c) with
    | Empty -> true
    | _ -> false
  else raise (Invalid_argument "Out of bounds")
