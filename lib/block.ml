type color =
  | R
  | G
  | B
  | Y
  | P
  | Pi
  | O

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
let hor_line2 = [ (0, 0); (1, 0) ]
let vert_line2 = [ (0, 0); (0, 1) ]
let hor_line3 = [ (0, 0); (1, 0); (2, 0) ]
let vert_line3 = [ (0, 0); (0, 1); (0, 2) ]
let small_l1 = [ (0, 0); (0, 1); (1, 1) ]
let small_l2 = [ (0, 0); (1, 0); (1, 1) ]
let small_l3 = [ (0, 0); (1, 0); (0, 1) ]
let small_l4 = [ (1, 0); (1, 1); (0, 1) ]
let sqr = [ (0, 0); (1, 0); (0, 1); (1, 1) ]

let big_sqr =
  [ (0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1); (0, 2); (1, 2); (2, 2) ]

let hor_line4 = [ (0, 0); (1, 0); (2, 0); (3, 0) ]
let vert_line4 = [ (0, 0); (0, 1); (0, 2); (0, 3) ]
let big_l = [ (0, 0); (1, 0); (2, 0); (2, 1); (2, 2) ]
let inv_big_l = [ (0, 0); (0, 1); (0, 2); (1, 2); (2, 2) ]
let t_up = [ (1, 0); (1, 1); (1, 2); (0, 1) ]
let t_down = [ (0, 0); (0, 1); (0, 2); (1, 1) ]
let t_left = [ (1, 0); (0, 1); (1, 1); (2, 1) ]
let t_right = [ (0, 0); (1, 0); (2, 0); (1, 1) ]
let s_right = [ (1, 0); (1, 1); (0, 1); (0, 2) ]
let s_left = [ (0, 0); (0, 1); (1, 1); (1, 2) ]
let s_down = [ (1, 0); (2, 0); (1, 1); (0, 1) ]
let l_up = [ (0, 0); (1, 0); (2, 0); (2, 1) ]
let l_down = [ (0, 0); (1, 0); (2, 0); (0, 1) ]
let l_left = [ (0, 0); (0, 1); (0, 2); (1, 2) ]
let l_right = [ (0, 0); (1, 0); (0, 1); (0, 2) ]

(*END SHAPE DEFS*)

let () = Random.self_init ()

let create_random_block () =
  let colors = [ R; G; B; Y; P; Pi; O ] in
  let shapes =
    [
      one;
      sqr;
      hor_line2;
      vert_line2;
      hor_line3;
      vert_line3;
      small_l1;
      small_l2;
      small_l3;
      small_l4;
      big_sqr;
      hor_line4;
      vert_line4;
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

let create_easy_random_block () =
  let colors = [ R; G; B; Y; P; Pi; O ] in
  let shapes =
    [
      one;
      sqr;
      hor_line2;
      vert_line2;
      hor_line3;
      vert_line3;
      small_l1;
      small_l2;
      small_l3;
      small_l4;
    ]
  in
  let random_color = List.nth colors (Random.int (List.length colors)) in
  let random_shape = List.nth shapes (Random.int (List.length shapes)) in
  create_block random_color random_shape

let create_medium_random_block () =
  let colors = [ R; G; B; Y; P; Pi; O ] in
  let shapes =
    [
      big_sqr;
      hor_line4;
      vert_line4;
      big_l;
      inv_big_l;
      t_up;
      t_down;
      t_left;
      t_right;
    ]
  in
  let random_color = List.nth colors (Random.int (List.length colors)) in
  let random_shape = List.nth shapes (Random.int (List.length shapes)) in
  create_block random_color random_shape

let get_bounds shape =
  let r = List.map fst shape in
  let c = List.map snd shape in
  let min_r = List.fold_left min max_int r in
  let max_r = List.fold_left max min_int r in
  let min_c = List.fold_left min max_int c in
  let max_c = List.fold_left max min_int c in
  (min_r, max_r, min_c, max_c)

let named_shapes =
  [
    ("one", one);
    ("hor_line2", hor_line2);
    ("vert_line2", vert_line2);
    ("hor_line3", hor_line3);
    ("vert_line3", vert_line3);
    ("small_l1", small_l1);
    ("small_l2", small_l2);
    ("small_l3", small_l3);
    ("small_l4", small_l4);
    ("sqr", sqr);
    ("big_sqr", big_sqr);
    ("hor_line4", hor_line4);
    ("vert_line4", vert_line4);
    ("big_l", big_l);
    ("inv_big_l", inv_big_l);
    ("t_up", t_up);
    ("t_down", t_down);
    ("t_left", t_left);
    ("t_right", t_right);
    ("s_right", s_right);
    ("s_left", s_left);
    ("s_down", s_down);
    ("l_up", l_up);
    ("l_down", l_down);
    ("l_left", l_left);
    ("l_right", l_right);
  ]

let same_shape s1 s2 = List.sort compare s1 = List.sort compare s2

let find_shape_name shape =
  match List.find_opt (fun (_, s) -> same_shape shape s) named_shapes with
  | Some (name, _) -> name
  | None -> "unknown_shape"

let block_to_string shape =
  let shape_name = find_shape_name shape in
  Printf.sprintf "Block {shape = %s }" shape_name
