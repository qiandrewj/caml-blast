type t
(**The abstract type representing a block.*)

type color = R | G | B | Y | P
(**The type representing a block's color. *)

type shape = (int * int) list
(**The type representing a block's shape as relative coordinates from the top right.*)

val create_block : color -> shape -> t
(**[create_block color shape] is a new block of [color] and [shape].*)

val create_random_block : unit -> t
(**[create_random_block ()] is a block with random color and shape.*)

val get_color : t -> color
(**[get_color block] is the color of [block].*)

val get_shape : t -> shape
(**[get_shape block] is the shape of [block].*)