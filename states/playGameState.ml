open Raylib
open Raygui
open Blockblast

type clear_animation = {
  rows : int list;
  cols : int list;
  progress : float;
  center_pos : int * int;
}

module S = Scoring.MakeScoring (Scoring.DefaultRules)

type t = {
  board : Board.t;
  active_blocks : (int * int, Block.t) Hashtbl.t;
  mutable queued_blocks : Block.t option array;
  dragged_block : (Block.t * (int * int) * int) option;
  mouse_pos : int * int;
  score_state : S.t;
  game_over : bool;
  clear_animation : clear_animation option;
}

(**[create_random_level_block score] is a randomly-colored block with a shape
   that is probabilistically determined by the player's current score. Simpler
   shapes are more common at the beginning, and afterwards blocks are completely
   random.*)
let create_random_level_block score =
  let roll = Random.float 1.0 in
  if score <= 1000 then
    if roll < 0.6 then Block.create_easy_random_block ()
    else if roll < 0.9 then Block.create_medium_random_block ()
    else Block.create_random_block ()
  else if score <= 2000 then
    if roll < 0.4 then Block.create_easy_random_block ()
    else if roll < 0.8 then Block.create_medium_random_block ()
    else Block.create_random_block ()
  else if score <= 5000 then
    if roll < 0.25 then Block.create_easy_random_block ()
    else if roll < 0.5 then Block.create_medium_random_block ()
    else Block.create_random_block ()
  else Block.create_random_block ()

(**[init_block_queue ()] is an array of three random blocks.*)
let init_block_queue score =
  Array.of_list
    [
      Some (create_random_level_block score);
      Some (create_random_level_block score);
      Some (create_random_level_block score);
    ]

let init () =
  let board = Board.create_board 8 in
  let active_blocks = Hashtbl.create 32 in
  let queued_blocks = init_block_queue 0 in
  let score_state = S.create () in
  {
    board;
    active_blocks;
    queued_blocks;
    dragged_block = None;
    mouse_pos = (0, 0);
    score_state;
    game_over = false;
    clear_animation = None;
  }

(**[color_to_raylib block_color] is the Raylib color of [block_color].*)
let color_to_raylib = function
  | Block.R -> Color.red
  | G -> Color.green
  | B -> Color.blue
  | Y -> Color.yellow
  | P -> Color.purple
  | Pi -> Color.pink
  | O -> Color.orange

(**[draw_cell x y cell] draws the board [cell] at [x, y].*)
let draw_cell x y = function
  | Board.Empty -> draw_rectangle x y 50 50 (Color.create 245 245 245 255)
  | Board.Block color -> draw_rectangle x y 50 50 (color_to_raylib color)

(**[draw_board board] draws the game board.*)
let draw_board board =
  draw_rectangle 200 80 (8 * 50) (8 * 50) (Color.create 245 245 245 255);

  for r = 0 to 7 do
    for c = 0 to 7 do
      let x = 200 + (c * 50) in
      let y = 80 + (r * 50) in
      let cell = Board.get_cell board (r, c) in
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

(**[draw_block_with_shape x y block] draws [block] at [x, y].*)
let draw_block_with_shape x y block =
  let color = color_to_raylib (Block.get_color block) in
  let shape = Block.get_shape block in

  List.iter
    (fun (dr, dc) ->
      draw_rectangle (x + (dc * 30)) (y + (dr * 30)) 28 28 color;
      draw_rectangle_lines (x + (dc * 30)) (y + (dr * 30)) 28 28 Color.white)
    shape

(**[draw_block_queue blocks] draws the upcoming queue [blocks].*)
let draw_block_queue blocks =
  Array.iteri
    (fun i block_opt ->
      match block_opt with
      | Some block ->
          let x = 200 + (i * 150) in
          draw_block_with_shape x 550 block
      | None -> ())
    blocks

(**[draw_dragged_block state] draws the currently dragged block in [state].*)
let draw_dragged_block state =
  match state.dragged_block with
  | Some (block, (offset_x, offset_y), _) ->
      let x = fst state.mouse_pos - offset_x in
      let y = snd state.mouse_pos - offset_y in
      draw_block_with_shape x y block
  | None -> ()

(**[random_color ()] is a random rainbow color.*)
let random_color () =
  let rainbow =
    [|
      Color.red;
      Color.orange;
      Color.yellow;
      Color.green;
      Color.blue;
      Color.purple;
      Color.pink;
    |]
  in
  let index = Random.int (Array.length rainbow) in
  rainbow.(index)

let draw_clear_animation state =
  match state.clear_animation with
  | Some a ->
      let effect_size = int_of_float (50.0 *. a.progress) in
      List.iter
        (fun row ->
          let y = 80 + (row * 50) in
          draw_rectangle
            (200 - (effect_size / 2))
            y (400 + effect_size) 50
            (Color.create 255 255 255
               (int_of_float (255.0 *. (1.0 -. a.progress))));
          for i = 0 to 7 do
            let angle = float_of_int i *. (2.0 *. Float.pi /. 8.0) in
            let center_x, center_y = a.center_pos in
            let px = center_x + int_of_float (float effect_size *. cos angle) in
            let py = center_y + int_of_float (float effect_size *. sin angle) in
            draw_rectangle px py 8 8 (random_color ())
          done)
        a.rows;

      List.iter
        (fun col ->
          let x = 200 + (col * 50) in
          draw_rectangle x
            (80 - (effect_size / 2))
            50 (400 + effect_size)
            (Color.create 255 255 255
               (int_of_float (255.0 *. (1.0 -. a.progress))));
          for i = 0 to 5 do
            let angle = float_of_int i *. (2.0 *. Float.pi /. 8.0) in
            let center_x, center_y = a.center_pos in
            let px = center_x + int_of_float (float effect_size *. sin angle) in
            let py = center_y + int_of_float (float effect_size *. cos angle) in
            draw_rectangle px py 8 8 (random_color ())
          done)
        a.cols
  | None -> ()

(**[get_block_at_pos state (x, y)] gets the queued block in [state] at position
   [x, y].*)
let get_block_at_pos state (x, y) =
  if y >= 520 && y < 520 + 160 then
    let index = (x - 200) / 150 in
    if index >= 0 && index < Array.length state.queued_blocks then
      match state.queued_blocks.(index) with
      | Some block -> (
          let base_x = 200 + (index * 150) in
          let base_y = 550 in
          let shape = Block.get_shape block in
          let cell_opt =
            List.find_opt
              (fun (dr, dc) ->
                let cell_x = base_x + (dc * 30) in
                let cell_y = base_y + (dr * 30) in
                x >= cell_x && x < cell_x + 28 && y >= cell_y && y < cell_y + 28)
              shape
          in
          match cell_opt with
          | Some _ -> Some (block, (x - base_x, y - base_y))
          | None -> None)
      | None -> None
    else None
  else None

(**[can_place_block board block (r, c)] is [true] if and only if [block] can be
   placed in [board] at [r, c].*)
let can_place_block board block (r, c) =
  let shape = Block.get_shape block in
  List.for_all
    (fun (dr, dc) ->
      let pos = (r + dr, c + dc) in
      Board.is_valid_pos board pos && Board.is_empty_cell board pos)
    shape

(**[handle_input state] is the updated state after handling mouse input.*)
let handle_input state =
  if state.game_over then state
  else
    let mouse_pos = (get_mouse_x (), get_mouse_y ()) in
    let state = { state with mouse_pos } in
    if is_mouse_button_pressed MouseButton.Left then
      match get_block_at_pos state mouse_pos with
      | Some (block, (click_x, click_y)) ->
          let index = (fst mouse_pos - 200) / 150 in
          state.queued_blocks.(index) <- None;
          { state with dragged_block = Some (block, (click_x, click_y), index) }
      | None -> state
    else if is_mouse_button_released MouseButton.Left then
      match state.dragged_block with
      | Some (block, (offset_x, offset_y), orig_index) ->
          let board_x = fst mouse_pos - 200 in
          let board_y = snd mouse_pos - 80 in

          if board_x >= 0 && board_y >= 0 then
            let col = (board_x - offset_x) / 50 in
            let row = (board_y - offset_y) / 50 in

            if can_place_block state.board block (row, col) then (
              Board.place_block state.board block (row, col);
              let cleared_rows, cleared_cols =
                Board.clear_full_lines state.board
              in
              S.add_block_score state.score_state block;
              let cleared_count =
                List.length cleared_rows + List.length cleared_cols
              in
              S.add_line_clear_score state.score_state cleared_count;
              let new_animation =
                if cleared_count > 0 then
                  Some
                    {
                      rows = cleared_rows;
                      cols = cleared_cols;
                      progress = 0.0;
                      center_pos = mouse_pos;
                    }
                else None
              in
              let all_empty = Array.for_all (( = ) None) state.queued_blocks in
              let score = S.get_score state.score_state in
              if all_empty then state.queued_blocks <- init_block_queue score;
              let game_over =
                Board.no_moves state.board
                  (Array.to_list state.queued_blocks
                  |> List.filter_map (fun x -> x))
              in
              {
                state with
                dragged_block = None;
                game_over;
                clear_animation = new_animation;
              })
            else (
              if
                orig_index >= 0 && orig_index < Array.length state.queued_blocks
              then state.queued_blocks.(orig_index) <- Some block;
              { state with dragged_block = None })
          else (
            if orig_index >= 0 && orig_index < Array.length state.queued_blocks
            then state.queued_blocks.(orig_index) <- Some block;
            { state with dragged_block = None })
      | None -> state
    else state

(**[draw_ui state] draws additional UI elements for [state].*)
let draw_ui state =
  draw_text (S.to_string state.score_state) 350 40 25 Color.white

let rec loop state =
  if window_should_close () then ()
  else
    let state = handle_input state in
    let state =
      match state.clear_animation with
      | Some anim when anim.progress < 1.0 ->
          {
            state with
            clear_animation =
              Some { anim with progress = min 1.0 (anim.progress +. 0.05) };
          }
      | Some anim when anim.progress >= 1.0 ->
          { state with clear_animation = None }
      | _ -> state
    in
    begin_drawing ();
    clear_background (Color.create 70 130 180 255);
    draw_ui state;
    draw_board state.board;
    draw_block_queue state.queued_blocks;
    draw_dragged_block state;
    draw_clear_animation state;
    end_drawing ();
    if not state.game_over then loop state
