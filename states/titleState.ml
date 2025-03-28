open Raylib
open Raygui
open Blockblast

let width = 800
let height = 600

type state = {
  show_text_input_box : bool;
  screen_changed : bool;
}

let setup () =
  init_window width height "raygui - controls test suite";
  set_target_fps 60;
  { show_text_input_box = false; screen_changed = false }

let rec loop s =
  match window_should_close () with
  | true -> close_window ()
  | false ->
      let col = get_style (Control.Default `Background_color) |> get_color in
      begin_drawing ();
      clear_background col;

      (if s.screen_changed then
         draw_text "[GAME BOARD]" 300 200 30 (Color.create 0 0 0 255)
       else
         let text = "BLOCKBLAST" in
         let f_size = 70 in
         let t_width = measure_text text f_size in
         let t_x = (width - t_width) / 2 in
         let t_y = 100 in
         draw_text text t_x t_y f_size (Color.create 0 0 0 255);

         let button_rect = Rectangle.create 300. 300. 200. 50. in
         if button button_rect "Press me!" then
           loop { s with screen_changed = true });

      end_drawing ();
      loop s

let () = setup () |> loop
