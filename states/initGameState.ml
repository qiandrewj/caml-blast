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
  draw_rectangle 200 80 (8 * 50) (8 * 50) (Color.create 230 230 230 255);

  for c = 0 to 8 do
    draw_line
      (200 + (c * 50))
      80
      (200 + (c * 50))
      (80 + (8 * 50))
      (Color.create 200 200 200 255)
  done;

  for r = 0 to 8 do
    draw_line 200
      (80 + (r * 50))
      (200 + (8 * 50))
      (80 + (r * 50))
      (Color.create 200 200 200 255)
  done;

  for r = 0 to 7 do
    for c = 0 to 7 do
      let x = 200 + (c * 50) in
      let y = 80 + (r * 50) in
      let cell = Board.get_cell board (r, c) in
      draw_cell x y cell
    done
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

let draw_ui () = draw_text "SCORE: 0" 350 40 25 Color.white
