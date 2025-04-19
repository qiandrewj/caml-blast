open OUnit2
open Blockblast

let test_block _ =
  let b = Block.create_block Block.R Block.sqr in
  assert_equal Block.R (Block.get_color b);
  assert_equal Block.sqr (Block.get_shape b)

let tests = "test suite" >::: [ "block tests" >:: test_block ]
let _ = run_test_tt_main tests
