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

let place_block board block (r, c) =
  let shape = Block.get_shape block in
  let color = Block.get_color block in
  if
    List.for_all
      (fun (dr, dc) ->
        let pos = (r + dr, c + dc) in
        is_valid_pos board pos && is_empty_cell board pos)
      shape
  then
    List.iter
      (fun (dr, dc) ->
        let pos = (r + dr, c + dc) in
        set_cell board pos (Block color))
      shape
  else failwith "Block cannot be placed there"
