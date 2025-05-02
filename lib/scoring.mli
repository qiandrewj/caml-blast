module type ScoringRules = sig
  val pts_per_block : int
  val pts_per_line : int
  val combo_base_mult : float
  val special_bonus_threshold : int
  val special_bonus_pts : int
end

module DefaultRules : ScoringRules

module MakeScoring : functor (R : ScoringRules) -> sig
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
end
