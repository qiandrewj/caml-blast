(**[ScoringRules] is the type of different scoring rules/difficulties.*)
module type ScoringRules = sig
  val pts_per_block : int
  (**[pts_per_block] is the amount of points awarded per individual square
     (within a larger block shape) placed.*)

  val pts_per_line : int
  (**[pts_per_line] is the amount of points awared for a fully cleared line.*)

  val combo_base_mult : float
  (**[combo_base_mult] is the multiplier applied for a combo clear (i.e. more
     than one line cleared at a time.)*)

  val special_bonus_threshold : int
  (**[special_bonus_threshold] is the threshold amount of lines that need to be
     cleared at a time to achieve the [special_bonus_pts].*)

  val special_bonus_pts : int
  (**[special_bonus_pts] is the amount of bonus points awarded when the
     [special_bonus_threshold] is reached.*)
end

(**[ScoringType] is the type of the scoring system.*)
module type ScoringType = sig
  type t
  (**[t] is the type of the internal scoring data.*)

  val create : unit -> t
  (**[create ()] creates the scoring system.*)

  val block_pts : Block.t -> int
  (**[block_pts block] is the amount of points awarded for placing [block].*)

  val line_pts : int -> int -> int
  (**[line_pts lines cleared] is the amount of points awarded for clearing
     [lines] lines.*)

  val score_block : t -> Block.t -> int
  (**[score_block t block] is the amount of points added for placing blocks by
     scoring the block placed and updating the internal score.*)

  val score_lines : t -> int -> int
  (**[score_lines t lines_cleared] is the amount of points awarded for clearing
     lines and updates the internal combo and score amounts.*)

  val get_score : t -> int
  (**[get_score t] is the current score of the game.*)

  val get_combos : t -> int
  (**[get_combos t] is the current amount of combos in the game.*)

  val reset : t -> unit
  (**[reset t] resets the scoring system.*)

  val add_block_score : t -> Block.t -> unit
  (**[add_block_score t block] adds the score of [block] to the current score of
     [t].*)

  val add_line_clear_score : t -> int -> unit
  (**[add_line_clear_score t lines_cleared] adds the score of clearing
     [lines_cleared] lines to the current score of [t].*)

  val to_string : t -> string
  (**[to_string t] is the string representation of the scoring system.*)
end

module DefaultRules : ScoringRules
(**[DefaultRules] is the default difficulty and rule settings.*)

module HardCoreRules : ScoringRules
(**[HardCoreRules] is the harder difficulty of scoring and rule settings.*)

(**[MakeScoring] is the functor that produces a scoring system given a set of
   scoring rules.*)
module MakeScoring : functor (R : ScoringRules) -> ScoringType
