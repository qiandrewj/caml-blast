open Block

type t

val create : unit -> t
val get_score : t -> int
val get_combos : t -> int
val reset : t -> unit
val block_pts : Block.t -> int
val line_pts : int -> int -> int
val score_block : t -> Block.t -> int
val score_lines : t -> int -> int
val to_string : t -> string
