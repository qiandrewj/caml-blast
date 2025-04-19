type color =
  | R
  | G
  | B
  | Y
  | P

type shape = (int * int) list

type t = {
  color : color;
  shape : shape;
}

let create_block c s = { color = c; shape = s }
let get_color block = block.color
let get_shape block = block.shape

(*SHAPE DEFS*)
let one = [ (0, 0) ]
let sqr = [ (0, 0); (1, 0); (0, 1); (1, 1) ]

let big_sqr =
  [ (0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1); (0, 2); (1, 2); (2, 2) ]

let hor_line = [ (0, 0); (1, 0); (2, 0); (3, 0) ]
let vert_line = [ (0, 0); (0, 1); (0, 2); (0, 3) ]
let big_l = [ (0, 0); (1, 0); (2, 0); (2, 1); (2, 2) ]
let inv_big_l = [ (0, 0); (0, 1); (0, 2); (1, 2); (2, 2) ]
let t_up = [ (0, 0); (0, 1); (0, 2); (-1, 1) ]
let t_down = [ (0, 0); (0, 1); (0, 2); (1, 1) ]
let t_left = [ (0, 0); (-1, 1); (0, 1); (1, 1) ]
let t_right = [ (0, 0); (1, 0); (2, 0); (1, 1) ]
let s_right = [ (0, 0); (0, 1); (-1, 1); (-1, 2) ]
let s_left = [ (0, 0); (0, 1); (1, 1); (1, 2) ]
let s_down = [ (0, 0); (1, 0); (0, 1); (-1, 1) ]
let l_up = [ (0, 0); (1, 0); (2, 0); (2, 1) ]
let l_down = [ (0, 0); (1, 0); (2, 0); (0, 1) ]
let l_left = [ (0, 0); (0, 1); (0, 2); (1, 2) ]
let l_right = [ (0, 0); (1, 0); (0, 1); (0, 2) ]

(*END SHAPE DEFS*)

let () = Random.self_init ()

let create_random_block () =
  let colors = [ R; G; B; Y; P ] in
  let shapes =
    [
      one;
      sqr;
      big_sqr;
      hor_line;
      vert_line;
      big_l;
      inv_big_l;
      t_up;
      t_down;
      t_left;
      t_right;
      s_right;
      s_left;
      s_down;
      l_up;
      l_down;
      l_left;
      l_right;
    ]
  in
  let random_color = List.nth colors (Random.int (List.length colors)) in
  let random_shape = List.nth shapes (Random.int (List.length shapes)) in
  create_block random_color random_shape
