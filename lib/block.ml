type color = R | G | B | Y | P

type shape = (int * int) list

type t = { color: color; shape: shape}

let create_block c s = { color = c; shape = s }

(*TODO: make this actually random*)
let create_random_block () = create_block R []

let get_color block = block.color

let get_shape block = block.shape

