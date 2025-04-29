open Raylib
open Raygui
open Blockblast

type t = {
  board : Board.t;
  active_blocks : (int * int, Block.t) Hashtbl.t;
  queued_blocks : Block.t list;
}

type falling_block = {
  mutable rect : Rectangle.t;
  color : Color.t;
  speed : float;
}

let name = "Blockblast!"
let set_default = true
let game_started = ref false
let buffer = ref None
let set_buffer (t : t) = buffer := Some t

(* Button Positioning *)
let button_width = 200.
let button_height = 50.
let start_button_x = (float_of_int Constants.width /. 2.) -. (button_width /. 2.)

let start_button_y =
  (float_of_int Constants.height /. 2.) -. (button_height /. 2.) +. 50.

let start_button_rect =
  Rectangle.create start_button_x start_button_y button_width button_height

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
      255 (* Brighter random colors *)
  in
  { rect = Rectangle.create x y size size; speed; color }

let init () =
  init_window Constants.width Constants.height name;
  set_target_fps 60;
  Random.self_init ();

  for _ = 1 to max_falling_blocks / 2 do
    let block = create_falling_block () in
    Rectangle.set_y block.rect (float_of_int (Random.int Constants.height));
    falling_block := block :: !falling_block
  done

let start_game () =
  if not !game_started then
    if button start_button_rect "PLAY" then game_started := true else ()

let state = PlayGameState.init ()

let update () =
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

  begin_drawing ();

  let top_color = Color.create 40 60 80 255 in
  let bottom_color = Color.create 80 100 120 255 in
  draw_rectangle_gradient_v 0 0 Constants.width Constants.height top_color
    bottom_color;

  List.iter
    (fun block -> draw_rectangle_rec block.rect block.color)
    !falling_block;

  (if !game_started then (
     clear_background Color.raywhite;
     draw_text "[GAME_BOARD]" 300 200 30 Color.black)
   else
     let text = "BLOCKBLAST!" in
     let base_font_size = 70 in
     let pulse = sin (time *. 5.0) in
     let f_size = base_font_size + int_of_float (pulse *. 5.0) in
     let t_width = measure_text text f_size in
     let t_x = (Constants.width - t_width) / 2 in
     let t_y = Constants.height / 4 in
     draw_text text t_x t_y f_size Color.gold;
     draw_text text (t_x + 3) (t_y + 3) f_size (Color.create 0 0 0 100);

     let sub_text = "Click PLAY to start" in
     let sub_f_size = 20 in
     let sub_t_width = measure_text sub_text sub_f_size in
     let sub_t_x = (Constants.width - sub_t_width) / 2 in
     let sub_t_y = t_y + f_size + 20 in
     draw_text sub_text sub_t_x sub_t_y sub_f_size Color.lightgray;
     start_game ());
  end_drawing ()

let rec loop () =
  if not (window_should_close ()) then (
    update ();
    loop ())
  else close_window ()

let run () =
  init ();
  loop ()

let reset () =
  buffer := None;
  game_started := false;
  falling_block := [];

  for _ = 1 to max_falling_blocks / 2 do
    let block = create_falling_block () in
    Rectangle.set_y block.rect (float_of_int (Random.int Constants.height));
    falling_block := block :: !falling_block
  done;
  block_spawn_timer := 0.0
