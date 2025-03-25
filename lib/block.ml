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

(*TODO: make this actually random*)
let create_random_block () = create_block R []
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
