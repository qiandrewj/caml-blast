type cell =
  | Empty
  | Block of Block.color

type t = {
  grid : cell array array;
  size : int;
}

let create_board board_size =
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
  else raise (Invalid_argument "Out of bounds")

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
  then (
    List.iter
      (fun (dr, dc) ->
        let pos = (r + dr, c + dc) in
        set_cell board pos (Block color))
      shape)
  else failwith "Block cannot be placed there"

let clear_full_lines board =
  let size = board.size in
  let grid = board.grid in
  let is_row_full r =
    Array.for_all
      (function
        | Block _ -> true
        | Empty -> false)
      grid.(r)
  in
  let is_col_full c =
    Array.for_all
      (fun row ->
        match row.(c) with
        | Block _ -> true
        | Empty -> false)
      grid
  in
  let full_rows =
    List.filter (fun r -> is_row_full r) (List.init size (fun x -> x))
  in
  let full_cols =
    List.filter (fun c -> is_col_full c) (List.init size (fun x -> x))
  in
  List.iter
    (fun r ->
      for c = 0 to size - 1 do
        grid.(r).(c) <- Empty
      done)
    full_rows;
  List.iter
    (fun c ->
      for r = 0 to size - 1 do
        grid.(r).(c) <- Empty
      done)
    full_cols;
  (full_rows, full_cols)

let no_moves board blocks =
  let can_place block =
    let shape = Block.get_shape block in
    List.exists
      (fun r ->
        List.exists
          (fun c ->
            List.for_all
              (fun (dr, dc) ->
                let pos = (r + dr, c + dc) in
                is_valid_pos board pos && is_empty_cell board pos)
              shape)
          (List.init board.size (fun x -> x)))
      (List.init board.size (fun x -> x))
  in
  not (List.exists can_place blocks)
