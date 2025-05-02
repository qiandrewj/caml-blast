open Block

type scoring_rules_t = {
  pts_per_block : int;
  pts_per_line : int;
  combo_base_mult : float;
  special_bonus_threshold : int;
  special_bonus_pts : int;
}

let scoring_rules =
  {
    pts_per_block = 10;
    pts_per_line = 100;
    combo_base_mult = 1.5;
    special_bonus_threshold = 2;
    special_bonus_pts = 300;
  }

type t = {
  mutable score : int;
  mutable combos : int;
}

let create () = { score = 0; combos = 0 }
let get_score t = t.score
let get_combos t = t.combos

let reset t =
  t.score <- 0;
  t.combos <- 0

let block_pts block =
  List.length (Block.get_shape block) * scoring_rules.pts_per_block

let line_pts combos lines_cleared =
  let base = lines_cleared * scoring_rules.pts_per_line in
  let combo_bonus =
    int_of_float (float base *. (scoring_rules.combo_base_mult ** float combos))
  in
  let special_bonus =
    if lines_cleared >= scoring_rules.special_bonus_threshold then
      scoring_rules.special_bonus_pts
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

let to_string t = Printf.sprintf "Score %d" t.score
