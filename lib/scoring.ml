open Block

module type ScoringRules = sig
  val pts_per_block : int
  val pts_per_line : int
  val combo_base_mult : float
  val special_bonus_threshold : int
  val special_bonus_pts : int
end

module DefaultRules : ScoringRules = struct
  let pts_per_block = 10
  let pts_per_line = 100
  let combo_base_mult = 1.5
  let special_bonus_threshold = 3
  let special_bonus_pts = 300
end

module MakeScoring (R : ScoringRules) = struct
  type t = {
    mutable score : int;
    mutable combos : int;
  }

  let create () = { score = 0; combos = 0 }
  let block_pts block = List.length (Block.get_shape block) * R.pts_per_block

  let line_pts combos lines_cleared =
    let base = lines_cleared * R.pts_per_line in
    let combo_bonus =
      int_of_float (float base *. (R.combo_base_mult ** float combos))
    in
    let special_bonus =
      if lines_cleared >= R.special_bonus_threshold then R.special_bonus_pts
      else 0
    in
    base + combo_bonus + special_bonus

  let score_block t block =
    let pts = block_pts block in
    t.score <- t.score + pts;
    pts

  let score_lines t lines_cleared =
    if lines_cleared > 0 then (
      t.combos <- t.combos + 1;
      let pts = line_pts t.combos lines_cleared in
      t.score <- t.score + pts;
      pts)
    else (
      t.combos <- 0;
      0)

  let get_score t = t.score
  let get_combos t = t.combos

  let reset t =
    t.score <- 0;
    t.combos <- 0

  let add_block_score s block = s.score <- s.score + block_pts block

  let add_line_clear_score s lines_cleared =
    if lines_cleared > 0 then s.combos <- s.combos + 1 else s.combos <- 0;
    s.score <- s.score + line_pts s.combos lines_cleared

  let to_string t = Printf.sprintf "Score %d" t.score
end
