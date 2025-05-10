open Raylib
open Raygui
open Blockblast

exception Invalid_game of string

type blast_particle = {
  x: float;
  y: float;
  dx: float;
  dy: float;
  size: float;
  color: Color.t;
  spawn_time : float;
}

type clear_animation = {
  rows : int list;
  cols : int list;
  progress : float;
  center_pos : int * int;
  particles : blast_particle list;
}

type drag_info = {
  block: Block.t;
  index: int;
  offset_x: int;
  offset_y: int;
}

module S = Scoring.MakeScoring (Scoring.DefaultRules)

type t = S.t

let name = "play"
let set_default = false
let buffer : t option ref = ref None
let set_buffer (scorer: t) = buffer := Some scorer
let board = ref (Board.create_board 8)
let active_blocks = ref (Hashtbl.create 32)

(**[color_to_raylib block_color] is the Raylib color of [block_color].*)
let color_to_raylib = function
  | Block.R -> Color.red
  | G -> Color.green
  | B -> Color.blue
  | Y -> Color.yellow
  | P -> Color.purple
  | Pi -> Color.pink
  | O -> Color.orange

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

let queued_blocks = ref (init_block_queue 0)
let dragged_block : drag_info option ref = ref None
let mouse_pos = ref (0, 0)
let scorer = ref (S.create ())
let game_over = ref false
let clear_animation = ref None

(**[draw_cell x y cell] draws the board [cell] at [x, y].*)
let draw_cell x y = function
  | Board.Empty -> draw_rectangle x y 50 50 (Color.create 245 245 245 180)
  | Board.Block color -> draw_rectangle x y 50 50 (color_to_raylib color)

let board_size = 8 * 50
(** [board_size] is the [number of squares in the grid] * [size of square in grid]. *)
let bx = (Constants.width - board_size) / 2
(** [bx] is the left coordinate of the board. *)
let by = ((Constants.height - board_size) / 2) - 50
(** [by] is the top coordinate of the board. *)
let queue_y = by + board_size + 20
(** [queue_y] is the top coordinate of the new blocks. *)

(**[draw_board board] draws the game board.*)
let draw_board board =
  draw_rectangle bx by board_size board_size (Color.create 245 245 245 100);

  for r = 0 to 7 do
    for c = 0 to 7 do
      let x = bx + (c * 50) in
      let y = by + (r * 50) in
      let cell = Board.get_cell board (r, c) in
      draw_cell x y cell
    done
  done;
  for c = 0 to 8 do
    draw_line
      (bx + (c * 50))
      by
      (bx + (c * 50))
      (by + (8 * 50))
      (Color.create 0 0 0 100)
  done;

  for r = 0 to 8 do
    draw_line bx
      (by + (r * 50))
      (bx + (8 * 50))
      (by + (r * 50))
      (Color.create 0 0 0 100)
  done

(**[draw_block_with_shape x y block] draws [block] at [x, y].*)
let draw_block_with_shape x y block =
  let color = color_to_raylib (Block.get_color block) in
  let shape = Block.get_shape block in

  List.iter
    (fun (dr, dc) ->
      let rect_x = x + (dc * 30) in
      let rect_y = y + (dr * 30) in
      let glow_color = Color.create 255 255 255 50 in

      for i = 1 to 3 do 
        let size = 28 + (i * 2) in 
        let offset = i in 
        draw_rectangle (rect_x - offset) (rect_y - offset) size size glow_color
      done; 

      draw_rectangle rect_x rect_y 30 30 (Color.create 0 0 0 80);
      draw_rectangle rect_x rect_y 28 28 color;
      draw_rectangle_lines rect_x rect_y 28 28 Color.white)
    shape

(**[draw_block_queue blocks] draws the upcoming queue [blocks].*)
let draw_block_queue blocks =
  Array.iteri
    (fun i block_opt ->
      match block_opt with
      | Some block ->
          let x = bx + (i * 150) in
          draw_block_with_shape x queue_y block; 
      | None -> ())
    blocks

(**[draw_dragged_block dragged_block mouse_pos] draws the currently dragged
   block. *)
let draw_dragged_block dragged_block mouse_pos =
  match dragged_block with
  | Some drag ->
      let mx, my = mouse_pos in
      draw_block_with_shape (mx - drag.offset_x) (my - drag.offset_y) drag.block
      | None -> ()

let create_blast_particles rs cs =
  let speed = 2.0 +. Random.float 3.0 in
  let make_particle x y dx dy dist =
    {
      x = float_of_int x;
      y = float_of_int y;
      dx = dx *. speed;
      dy = dy *. speed;
      size = 4.0 +. Random.float 4.0;
      color = random_color ();
      spawn_time = dist *. 0.03;
    }
  in
  let row_particles = 
    List.flatten (
      List.map (fun r ->
        let y = by + r * 50 + 25 in
        List.init 8 (fun c ->
          let x = bx + c * 50 + 25 in
          let center_dist = float_of_int (abs (c - 4)) in 
          [ make_particle x y (-1.0) 0.0 center_dist;
            make_particle x y 1.0 0.0 center_dist]
        ) |> List.flatten
      ) rs
    )
  in
  let col_particles =
    List.flatten (
      List.map (fun c ->
        let x = bx + c * 50 + 25 in
        List.init 8 (fun r ->
          let y = by + r * 50 + 25 in
          let center_dist = float_of_int (abs (r - 4)) in
          [ make_particle x y 0.0 (-1.0) center_dist;
            make_particle x y 0.0 1.0 center_dist]
        ) |> List.flatten
      ) cs
    ) 
  in
  row_particles @ col_particles
        

let draw_clear_animation () =
  match !clear_animation with
  | Some a ->
      let alpha = int_of_float (255.0 *. (1.0 -. a.progress)) in
      let center_x, center_y = a.center_pos in
      let radius = 300.0 *. a.progress in
      draw_circle_lines center_x center_y radius (Color.create 255 255 255 alpha);

      List.iter (fun p ->
        let size = int_of_float p.size in
        draw_rectangle (int_of_float p.x) (int_of_float p.y) size size p.color;
      ) a.particles;

      let effect_size = int_of_float (50.0 *. a.progress) in
      List.iter (fun row ->
        let y = by + (row * 50) in
        draw_rectangle (bx - (effect_size / 2)) y (400 + effect_size) 50
          (Color.create 255 255 255 alpha);
      ) a.rows;

      List.iter (fun col ->
        let x = bx + (col * 50) in
        draw_rectangle x (by - (effect_size / 2)) 50 (400 + effect_size)
          (Color.create 255 255 255 alpha);
      ) a.cols;
  | None -> ()

(**[get_block_at_pos queued_blocks (x, y)] gets the queued block at position
   [x, y].*)
let get_block_at_pos queued_blocks (x, y) =
  if y >= queue_y && y < queue_y + 150 then
    let index = (x - bx) / 150 in
    if index >= 0 && index < Array.length queued_blocks then
      match queued_blocks.(index) with
      | Some block -> (
          let shape = Block.get_shape block in
          let base_x = bx + (index * 150) in
          let base_y = queue_y in
          let cell_opt =
            List.find_opt
              (fun (dr, dc) ->
                let cell_x = base_x + (dc * 30) in
                let cell_y = base_y + (dr * 30) in
                x >= cell_x && x < cell_x + 28 && y >= cell_y && y < cell_y + 28)
              shape
          in
          match cell_opt with
          | Some (r, c) ->  Some block
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

(**[update_game buffer] is the updated data after handling mouse input.*)
let update_game () =
  if !game_over then ()
  else
   mouse_pos := (get_mouse_x (), get_mouse_y ()); 
    match !clear_animation with
    | Some anim when anim.progress < 1.0 ->
      let updated_particles = 
        List.map (fun p -> 
          {
            p with 
            x = p.x +. p.dx; 
            y = p.y +. p.dy; 
            size = max 0.0 (p.size -. 0.2); 
          }) anim.particles in
        let updated = { anim with 
        progress = anim.progress +. 0.05;
        particles = updated_particles; } in
        clear_animation := Some updated
        | Some _ ->  clear_animation := None
    | None -> (
    if is_mouse_button_pressed MouseButton.Left then
      match get_block_at_pos !queued_blocks !mouse_pos with
      | Some block ->
          let index = (fst !mouse_pos - bx) / 150 in
          let base_x = bx + (index * 150) in
          let base_y = queue_y in
          let mx, my = !mouse_pos in
          let offset_x = mx - base_x in 
          let offset_y = my - base_y in
          !queued_blocks.(index) <- None;
          dragged_block := Some {
            block;
            index;
            offset_x;
            offset_y
          }
      | None -> ()
    else if is_mouse_button_released MouseButton.Left then
      match !dragged_block with
      | Some drag ->
          let mx, my = !mouse_pos in
          let r = (my - drag.offset_y - by) / 50 in
          let c = (mx - drag.offset_x - bx) / 50 in
          if r >= 0 && c >= 0 then
            if can_place_block !board drag.block (r, c) then (
              Board.place_block !board drag.block (r, c);
              let cleared_rows, cleared_cols =
                Board.clear_full_lines !board
              in
              S.add_block_score !scorer drag.block;
              let cleared_count =
                List.length cleared_rows + List.length cleared_cols
              in
              S.add_line_clear_score !scorer cleared_count;
              let new_animation =
                if cleared_count > 0 then
                  Some {
                    rows = cleared_rows;
                    cols = cleared_cols;
                    progress = 0.0;
                    center_pos = !mouse_pos;
                    particles = create_blast_particles cleared_rows cleared_cols;
                  }
                else None
              in
              let all_empty =
                Array.for_all (( = ) None) !queued_blocks
              in
              let curr_score = S.get_score !scorer in
              if all_empty then queued_blocks := init_block_queue curr_score;
              let check_game_over =
                Board.no_moves !board
                  (Array.to_list !queued_blocks
                  |> List.filter_map (fun x -> x))
              in
              dragged_block := None; 
              game_over := check_game_over; 
              clear_animation := new_animation)
            else (
              if
                drag.index >= 0
                && drag.index < Array.length !queued_blocks
              then !queued_blocks.(drag.index) <- Some drag.block;
              dragged_block := None)
          else (
            if
              drag.index >= 0 && drag.index < Array.length !queued_blocks
            then !queued_blocks.(drag.index) <- Some drag.block;
            dragged_block := None)
      | None -> ()
    else ()
    )

let init () = set_buffer !scorer

let update () =
  update_game (); 
  if !game_over then 
    (OverState.set_buffer (S.get_score !scorer); Some "over") else None

let render () =
  begin_drawing ();
  let top_color = Color.create 60 90 130 255 in
  let bottom_color = Color.create 30 60 90 255 in
  draw_rectangle_gradient_v 0 0 Constants.width Constants.height top_color
    bottom_color;
  let score_text = S.to_string !scorer in
  let text_width = measure_text score_text 25 in
  let text_height = 25 in
  let score_x = (Constants.width - text_width) / 2 in
  let score_y = by - 50 in
  draw_rectangle_rounded (Rectangle.create 
  (float_of_int (score_x - 15)) 
  (float_of_int (score_y - 10)) 
  (float_of_int (text_width + 30)) 
  (float_of_int (text_height + 20))) 0.2 10 Color.gray;
  draw_text score_text score_x score_y 25 Color.white;
 
  draw_board !board;
  draw_block_queue !queued_blocks;
  draw_dragged_block !dragged_block !mouse_pos;
  draw_clear_animation (); 

  end_drawing ()

let reset () = (
  board := Board.create_board 8;
  Hashtbl.reset !active_blocks;
  queued_blocks := (init_block_queue 0);
  dragged_block := None;
  mouse_pos := (0, 0);
  scorer := (S.create ());
  game_over := false;
  clear_animation := None
)