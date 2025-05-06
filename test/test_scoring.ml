open OUnit2
open Blockblast
module TestScoring = Scoring.MakeScoring (Scoring.DefaultRules)

let test_score_block =
  "test_score_block" >:: fun _ ->
  let s = TestScoring.create () in
  let b = Block.create_block Block.B Block.sqr in
  let pts = TestScoring.score_block s b in
  assert_equal 40 pts ~printer:string_of_int

let test_score_lines =
  "test_score_lines" >:: fun _ ->
  let s = TestScoring.create () in
  let pts = TestScoring.score_lines s 2 in
  let expected = TestScoring.line_pts 1 2 in
  assert_equal expected pts ~printer:string_of_int;
  assert_equal expected (TestScoring.get_score s) ~printer:string_of_int

let test_combo_mult =
  "test_combo_mult" >:: fun _ ->
  let s = TestScoring.create () in
  ignore (TestScoring.score_lines s 1);
  let pts2 = TestScoring.score_lines s 1 in
  let expected = TestScoring.line_pts 2 1 in
  assert_equal expected pts2 ~printer:string_of_int

let test_combo_reset =
  "test_combo_reset" >:: fun _ ->
  let s = TestScoring.create () in
  ignore (TestScoring.score_lines s 2);
  ignore (TestScoring.score_lines s 0);
  let pts = TestScoring.score_lines s 2 in
  let expected = TestScoring.line_pts 1 2 in
  assert_equal expected pts ~printer:string_of_int

let test_bonus =
  "test_bonus" >:: fun _ ->
  let s = TestScoring.create () in
  ignore (TestScoring.score_lines s 2);
  ignore (TestScoring.score_lines s 3);
  let pts = TestScoring.score_lines s 4 in
  let expected = TestScoring.line_pts 3 4 in
  assert_equal expected pts ~printer:string_of_int

let test_reset =
  "test_reset" >:: fun _ ->
  let s = TestScoring.create () in
  ignore (TestScoring.score_lines s 2);
  ignore (TestScoring.score_block s (Block.create_block Block.G Block.sqr));
  TestScoring.reset s;
  assert_equal 0 (TestScoring.get_score s) ~printer:string_of_int;
  assert_equal 0 (TestScoring.get_combos s) ~printer:string_of_int

let test_to_string =
  "test_to_string" >:: fun _ ->
  let s = TestScoring.create () in
  ignore (TestScoring.score_lines s 1);
  let str = TestScoring.to_string s in
  assert_equal "Score 250" str ~printer:(fun s -> s)

let test_add_block_score =
  "test_add_block_score" >:: fun _ ->
  let s = TestScoring.create () in
  let b = Block.create_block Block.P Block.sqr in
  TestScoring.add_block_score s b;
  let expected = TestScoring.block_pts b in
  assert_equal expected (TestScoring.get_score s) ~printer:string_of_int

let test_add_line_clear_score =
  "test_add_line_clear_score" >:: fun _ ->
  let s = TestScoring.create () in
  TestScoring.add_line_clear_score s 2;
  let expected = TestScoring.line_pts 1 2 in
  assert_equal expected (TestScoring.get_score s) ~printer:string_of_int;
  assert_equal 1 (TestScoring.get_combos s) ~printer:string_of_int

let test_add_line_clear_score_zero =
  "test_add_line_clear_score_zero" >:: fun _ ->
  let s = TestScoring.create () in
  TestScoring.add_line_clear_score s 0;
  assert_equal 0 (TestScoring.get_combos s) ~printer:string_of_int;
  assert_equal 0 (TestScoring.get_score s) ~printer:string_of_int

let tests =
  "test_scoring"
  >::: [
         test_score_block;
         test_score_lines;
         test_combo_mult;
         test_combo_reset;
         test_bonus;
         test_reset;
         test_to_string;
         test_add_block_score;
         test_add_line_clear_score;
         test_add_line_clear_score_zero;
       ]

let _ = run_test_tt_main tests
