open Raylib
open Blockblast

type t = {
  board : Board.t;
  available_blocks : (int * int, Block.t) Hashtbl.t;
  queued_blocks : Block.t list;
}

let init () =
  let board = Board.create_board () in
  let available_blocks = Hashtbl.create 16 in
  let queued_blocks =
    [
      Block.create_random_block ();
      Block.create_random_block ();
      Block.create_random_block ();
    ]
  in
  { board; available_blocks; queued_blocks }

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
  draw_rectangle 200 100 (8 * 50) (8 * 50) (Color.create 230 230 230 255);

  for c = 0 to 8 do
    draw_line
      (200 + (c * 50))
      100
      (200 + (c * 50))
      (100 + (8 * 50))
      (Color.create 200 200 200 255)
  done;

  for r = 0 to 8 do
    draw_line 200
      (100 + (r * 50))
      (200 + (8 * 50))
      (100 + (8 * 50))
      (Color.create 200 200 200 255)
  done;

  for r = 0 to 7 do
    for c = 0 to 7 do
      let x = 200 + (c * 50) in
      let y = 100 + (r * 50) in
      let cell = Board.get_cell board (r, c) in
      draw_cell x y cell
    done
  done

let draw_block_with_shape x y block =
  let color = color_to_raylib (Block.get_color block) in
  let shape = Block.get_shape block in

  draw_rectangle_rounded
    (Rectangle.create x y 60. 60.)
    0.2 10
    (Color.create 220 220 220 255);

  draw_rectangle_rounded
    (Rectangle.create (x +. 5.) (y +. 5.) (60. -. 10.) (60. -. 10.))
    0.2 10 color;

  List.iter
    (fun (dr, dc) ->
      draw_rectangle
        (int_of_float x + 15 + (dc * 10))
        (int_of_float y + 15 + (dr * 10))
        8 8
        (Color.create 255 255 255 200))
    shape

let draw_block_queue blocks =
  draw_rectangle (600 - 15) (100 - 25)
    ((3 * 60) + (2 * 15) + 30)
    (60 + 50)
    (Color.create 240 240 255 255);

  draw_text "Next Blocks" (600 + 10) (100 - 20) 20 Color.darkblue;

  List.iteri
    (fun i block ->
      let x = 600 + (i * (60 + 15)) in
      draw_block_with_shape (float_of_int x) 100. block)
    blocks

let draw_ui () =
  draw_rectangle_rounded
    (Rectangle.create 600. 250. 180. 100.)
    0.1 10
    (Color.create 230 230 240 255);

  draw_text "SCORE" 620 270 25 Color.darkgray;
  draw_text "0" 620 310 40 Color.black
