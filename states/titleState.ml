open Raylib
open Raygui
open Blockblast

type t = {
  board : Board.t;
  active_blocks : (int * int, Block.t) Hashtbl.t;
  queued_blocks : Block.t list;
}

let name = "title"
let set_default = true
let game_started = ref false
let buffer = ref None
let set_buffer (t : t) = buffer := Some t
let start_button_x = (Constants.width / 2) - 100
let start_button_y = (Constants.height / 2) - 150

let start_button_rect =
  Rectangle.create
    (float_of_int start_button_x)
    (float_of_int start_button_y)
    150. 50.

let init () =
  init_window Constants.width Constants.height name;
  set_target_fps 60

let start_game () =
  if not !game_started then
    if button start_button_rect "PLAY" then game_started := true else ()


let state = InitGameState.init () 
let update () =
  let col = get_style (Control.Default `Background_color) |> get_color in
  begin_drawing ();
  clear_background col;

  start_game ();

  (if !game_started then  
    (** somehow transition/set new states. *)
     draw_text "[GAME BOARD]" 300 200 30 (Color.create 0 0 0 255)
   else
     let text = "WELCOME TO BLOCKBLAST" in
     let f_size = 50 in
     let t_width = measure_text text f_size in
     let t_x = (Constants.width - t_width) / 2 in
     let t_y = 100 in
     draw_text text t_x t_y f_size (Color.create 0 0 0 255));
  end_drawing ()

let rec loop () =
  if not (window_should_close ()) then (
    update ();
    loop ())
  else close_window ()

let run () =
  init ();
  loop ()

let reset () = buffer := None

(* let reset () = buffer := None

   open Raylib open Raygui open Blockblast

   type t = { board : Board.t; active_blocks : (int * int, Block.t) Hashtbl.t;
   queued_blocks : Block.t list; }

   let name = "title" let set_default = true let game_started = ref false let
   buffer = ref None let set_buffer (t : t) = buffer := Some t

   let start_button_x = (Constants.width / 2) - 150 let start_button_y =
   (Constants.height / 2) - 150

   (* Initialize the start button rectangle *) let start_button_rect =
   Rectangle.create (float_of_int start_button_x) (float_of_int start_button_y)
   200. 50.

   let init () = init_window Constants.width Constants.height name;
   set_target_fps 60; (* Set the game loop to run at 60 FPS *)

   let start_game () = (* Check if the button is clicked using Raygui's button
   function *) if button start_button_rect "PLAY" then game_started := true

   let update () = let col = get_style (Control.Default `Background_color) |>
   get_color in begin_drawing (); clear_background col;

   (* Check if game has started and render accordingly *) if !game_started then
   draw_text "[GAME BOARD]" 300 200 30 (Color.create 0 0 0 255) else let text =
   "WELCOME TO BLOCKBLAST" in let f_size = 70 in let t_width = measure_text text
   f_size in let t_x = (Constants.width - t_width) / 2 in let t_y = 100 in
   draw_text text t_x t_y f_size (Color.create 0 0 0 255);

   end_drawing ()

   let rec loop () = if not (window_should_close ()) then ( update (); loop ())
   else close_window () (* Close the window cleanly when the game ends *)

   let run () = init (); (* Initialize Raylib *) loop () (* Start the game loop
   *)

   let reset () = buffer := None

   (* Call the run function to start the game *) let () = run () *)
