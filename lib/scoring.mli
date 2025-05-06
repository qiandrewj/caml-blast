(**[ScoringRules] is the type of different scoring rules/difficulties.*)
module type ScoringRules = sig
  val pts_per_block : int
  val pts_per_line : int
  val combo_base_mult : float
  val special_bonus_threshold : int
  val special_bonus_pts : int
end

(**[ScoringType] is the type of the scoring system.*)
module type ScoringType = sig
  type t

  val create : unit -> t
  val block_pts : Block.t -> int
  val line_pts : int -> int -> int
  val score_block : t -> Block.t -> int
  val score_lines : t -> int -> int
  val get_score : t -> int
  val get_combos : t -> int
  val reset : t -> unit
  val add_block_score : t -> Block.t -> unit
  val add_line_clear_score : t -> int -> unit
  val to_string : t -> string
end

module DefaultRules : ScoringRules
(**[DefaultRules] is the default difficulty and rule settings.*)

module HardCoreRules : ScoringRules
(**[HardCoreRules] is the harder difficulty of scoring and rule settings.*)

(**[MakeScoring] is the functor that produces a scoring system given a set of
   scoring rules.*)
module MakeScoring : functor (R : ScoringRules) -> ScoringType
