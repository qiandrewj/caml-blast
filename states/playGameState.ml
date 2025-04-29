open Raylib
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

let init () =
  let board = Board.create_board 8 in
  let active_blocks = Hashtbl.create 16 in
  let queued_blocks =
    [
      Block.create_random_block ();
      Block.create_random_block ();
      Block.create_random_block ();
    ]
  in
  {
    board;
    active_blocks;
    queued_blocks;
    dragged_block = None;
    mouse_pos = (0, 0);
    score = 0;
    game_over = false;
  }

let color_to_raylib = function
  | Block.R -> Color.red
  | G -> Color.green
  | B -> Color.blue
  | Y -> Color.yellow
  | P -> Color.purple

let draw_cell x y = function
  | Board.Empty -> draw_rectangle x y 50 50 (Color.create 245 245 245 255)
  | Board.Block color ->
      draw_rectangle x y 50 50 (color_to_raylib color);
      draw_rectangle_lines x y 50 50 (Color.create 50 50 50 255)

let draw_board board =
  draw_rectangle 200 80 (8 * 50) (8 * 50) (Color.create 245 245 245 255);

  for r = 0 to 7 do
    for c = 0 to 7 do
      let x = 200 + (c * 50) in
      let y = 80 + (r * 50) in
      let cell = Board.get_cell board (r, c) in
      (match cell with
      | Board.Empty -> Printf.printf "Cell (%d, %d): Empty\n" r c
      | Board.Block color -> Printf.printf "Cell (%d, %d): Block\n" r c);
      flush stdout;
      draw_cell x y cell
    done
  done;
  for c = 0 to 8 do
    draw_line
      (200 + (c * 50))
      80
      (200 + (c * 50))
      (80 + (8 * 50))
      (Color.create 0 0 0 255)
  done;

  for r = 0 to 8 do
    draw_line 200
      (80 + (r * 50))
      (200 + (8 * 50))
      (80 + (r * 50))
      (Color.create 0 0 0 255)
  done

let draw_block_with_shape x y block =
  let color = color_to_raylib (Block.get_color block) in
  let shape = Block.get_shape block in

  List.iter
    (fun (dr, dc) ->
      draw_rectangle (x + (dc * 30)) (y + (dr * 30)) 28 28 color;
      draw_rectangle_lines (x + (dc * 30)) (y + (dr * 30)) 28 28 Color.white)
    shape

let draw_block_queue blocks =
  List.iteri
    (fun i block ->
      let x = 200 + (i * 150) in
      draw_block_with_shape x 550 block)
    blocks

let draw_dragged_block state =
  match state.dragged_block with
  | Some (block, (offset_x, offset_y)) ->
      let x = fst state.mouse_pos - offset_x in
      let y = snd state.mouse_pos - offset_y in
      draw_block_with_shape x y block
  | None -> ()

let get_block_at_pos state (x, y) =
  if y >= 550 && y < 550 + 30 then
    let index = (x - 200) / 150 in
    if index >= 0 && index < List.length state.queued_blocks then
      Some
        ( List.nth state.queued_blocks index,
          (x - (200 + (index * 150)), y - 550) )
    else None
  else None

(* let board_pos_to_cell (x, y) = let col = (x - 200) / 50 in let row = (y - 80)
   / 50 in (row, col) *)
let can_place_block board block (r, c) =
  let shape = Block.get_shape block in
  List.for_all
    (fun (dr, dc) ->
      let pos = (r + dr, c + dc) in
      Board.is_valid_pos board pos && Board.is_empty_cell board pos)
    shape

let handle_input state =
  if state.game_over then state
  else
    let mouse_pos = (get_mouse_x (), get_mouse_y ()) in
    let state = { state with mouse_pos } in

    Printf.printf "Mouse position: (%d, %d)\n" (fst mouse_pos) (snd mouse_pos);
    flush stdout;

    if is_mouse_button_pressed MouseButton.Left then (
      match get_block_at_pos state mouse_pos with
      | Some (block, offset) ->
          Printf.printf "Block selected for dragging.\n";
          flush stdout;
          { state with dragged_block = Some (block, offset) }
      | None ->
          Printf.printf "No block selected.\n";
          flush stdout;
          state)
    else if is_mouse_button_released MouseButton.Left then (
      match state.dragged_block with
      | Some (block, _) ->
          let board_x = fst mouse_pos - 200 in
          let board_y = snd mouse_pos - 80 in
          Printf.printf "Mouse released at board position: (%d, %d)\n" board_x
            board_y;
          flush stdout;

          if board_x >= 0 && board_y >= 0 then (
            let col = board_x / 50 in
            let row = board_y / 50 in
            Printf.printf "Attempting to place block at cell: (%d, %d)\n" row
              col;
            flush stdout;

            if can_place_block state.board block (row, col) then (
              Printf.printf "Block can be placed. Placing block.\n";
              flush stdout;
              Board.place_block state.board block (row, col);
              let cleared = Board.clear_full_lines state.board in
              let new_score = state.score + (cleared * 100) in

              let new_queued_blocks =
                List.filter (fun b -> b <> block) state.queued_blocks
              in

              let final_queued_blocks =
                if List.length new_queued_blocks = 0 then
                  [
                    Block.create_random_block ();
                    Block.create_random_block ();
                    Block.create_random_block ();
                  ]
                else new_queued_blocks
              in

              let game_over = Board.no_moves state.board final_queued_blocks in

              {
                state with
                dragged_block = None;
                queued_blocks = final_queued_blocks;
                mouse_pos;
                score = new_score;
                game_over;
              })
            else (
              Printf.printf "Block cannot be placed.\n";
              flush stdout;
              { state with dragged_block = None }))
          else (
            Printf.printf "Mouse released outside board boundaries.\n";
            flush stdout;
            { state with dragged_block = None })
      | None ->
          Printf.printf "No block was being dragged.\n";
          flush stdout;
          state)
    else state

let draw_ui state =
  draw_text (Printf.sprintf "SCORE: %d" state.score) 350 40 25 Color.white

let rec loop state =
  if window_should_close () then ()
  else
    let state = handle_input state in
    begin_drawing ();
    clear_background (Color.create 70 130 180 255);
    draw_ui state;
    draw_board state.board;
    draw_block_queue state.queued_blocks;
    draw_dragged_block state;
    end_drawing ();
    if not state.game_over then loop state
