open Raylib
open Raygui
open Blockblast
module S = Scoring.MakeScoring (Scoring.DefaultRules)

type t = int

type falling_block = {
  mutable rect : Rectangle.t;
  color : Color.t;
  speed : float;
}

let name = "over"
let set_default = false
let game_restarted = ref false
let buffer : t option ref = ref (Some 0)
let set_buffer (t : t) = buffer := Some t
let button_width = 200.
let button_height = 50.

let restart_button_x =
  (float_of_int Constants.width /. 2.) -. (button_width /. 2.)

let restart_button_y =
  (float_of_int Constants.height /. 2.) -. (button_height /. 2.) +. 50.

let restart_button_rect =
  Rectangle.create restart_button_x restart_button_y button_width button_height

let falling_block = ref []
let max_falling_blocks = 50
let block_spawn_timer = ref 0.0
let block_spawn_interval = 0.1

let create_falling_block () =
  let size = float_of_int (10 + Random.int 21) in
  let x = float_of_int (Random.int (Constants.width - int_of_float size)) in
  let y = -.size -. float_of_int (Random.int 100) in
  let speed = float_of_int (50 + Random.int 101) in
  let color =
    Color.create
      (100 + Random.int 156)
      (100 + Random.int 156)
      (100 + Random.int 156)
      255
  in
  { rect = Rectangle.create x y size size; speed; color }

let init () =
  Random.self_init ();

  for _ = 1 to max_falling_blocks / 2 do
    let block = create_falling_block () in
    Rectangle.set_y block.rect (float_of_int (Random.int Constants.height));
    falling_block := block :: !falling_block
  done;
  block_spawn_timer := 0.0

let restart_game () =
  if not !game_restarted then
    if button restart_button_rect "RESTART" then game_restarted := true else ()

let draw_end_screen time =
  let text = "GAME OVER" in
  let base_font_size = 70 in
  let pulse = sin (time *. 5.0) in
  let f_size = base_font_size + int_of_float (pulse *. 5.0) in
  let t_width = measure_text text f_size in
  let t_x = (Constants.width - t_width) / 2 in
  let t_y = Constants.height / 4 in
  draw_text text t_x t_y f_size Color.gold;
  draw_text text (t_x + 3) (t_y + 3) f_size (Color.create 0 0 0 100);
  let sub_text =
    match !buffer with
    | Some s -> Printf.sprintf "Total score: %d" s
    | None -> raise Not_found
  in
  let sub_f_size = 40 in
  let sub_t_width = measure_text sub_text sub_f_size in
  let sub_t_x = (Constants.width - sub_t_width) / 2 in
  let sub_t_y = t_y + f_size + 20 in
  draw_text sub_text sub_t_x sub_t_y sub_f_size Color.lightgray;

  let is_hovered =
    check_collision_point_rec (get_mouse_position ()) restart_button_rect
  in
  let button_color = if is_hovered then Color.gold else Color.gray in
  draw_rectangle_rounded restart_button_rect 0.2 10 button_color;
  let play_text = "RESTART" in
  let text_width = measure_text play_text 30 in
  let text_height = 30 in
  let text_x =
    restart_button_x +. (button_width /. 2.) -. (float_of_int text_width /. 2.)
  in
  let text_y =
    restart_button_y +. (button_height /. 2.) -. (float_of_int text_height /. 2.)
  in
  draw_text play_text (int_of_float text_x) (int_of_float text_y) 30 Color.black;
  end_drawing ()

let update () = if !game_restarted then Some "reset" else None

let render () =
  let delta_time = get_frame_time () in
  let time = get_time () in
  block_spawn_timer := !block_spawn_timer +. delta_time;
  if
    !block_spawn_timer >= block_spawn_interval
    && List.length !falling_block < max_falling_blocks
  then (
    falling_block := create_falling_block () :: !falling_block;
    block_spawn_timer := 0.0);

  falling_block :=
    List.filter_map
      (fun block ->
        let curr_y = Rectangle.y block.rect in
        let new_y = curr_y +. (block.speed *. delta_time) in
        Rectangle.set_y block.rect new_y;
        if new_y > float_of_int Constants.height then None else Some block)
      !falling_block;

  let top_color = Color.create 60 90 130 255 in
  let bottom_color = Color.create 30 60 90 255 in
  draw_rectangle_gradient_v 0 0 Constants.width Constants.height top_color
    bottom_color;

  List.iter
    (fun block -> draw_rectangle_rec block.rect block.color)
    !falling_block;
  if !game_restarted then clear_background Color.raywhite
  else (
    draw_end_screen time;
    restart_game ())

let reset () =
  buffer := None;
  game_restarted := false;
  falling_block := [];

  for _ = 1 to max_falling_blocks / 2 do
    let block = create_falling_block () in
    Rectangle.set_y block.rect (float_of_int (Random.int Constants.height));
    falling_block := block :: !falling_block
  done;

  block_spawn_timer := 0.0
