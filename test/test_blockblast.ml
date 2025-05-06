open OUnit2
open Blockblast

let test_block _ =
  let b = Block.create_block Block.R Block.sqr in
  assert_equal Block.R (Block.get_color b);
  assert_equal Block.sqr (Block.get_shape b)

let test_random_blocks _ =
  let blocks = List.init 20 (fun _ -> Block.create_random_block ()) in
  List.iter
    (fun block ->
      let color = Block.get_color block in
      assert_bool "Valid color"
        (List.mem color
           [ Block.R; Block.G; Block.B; Block.Y; Block.P; Block.Pi; Block.O ]);
      let shape = Block.get_shape block in
      assert_bool "Non-empty shape" (shape <> []))
    blocks

let test_easy_blocks _ =
  let blocks = List.init 20 (fun _ -> Block.create_easy_random_block ()) in
  List.iter
    (fun block ->
      let color = Block.get_color block in
      assert_bool "Valid color"
        (List.mem color
           [ Block.R; Block.G; Block.B; Block.Y; Block.P; Block.Pi; Block.O ]);
      let shape = Block.get_shape block in
      assert_bool "Easy shape"
        (List.mem shape
           [
             Block.one;
             Block.sqr;
             Block.hor_line2;
             Block.vert_line2;
             Block.hor_line3;
             Block.vert_line3;
             Block.small_l1;
             Block.small_l2;
             Block.small_l3;
             Block.small_l4;
           ]))
    blocks

let test_medium_blocks _ =
  let blocks = List.init 20 (fun _ -> Block.create_medium_random_block ()) in
  List.iter
    (fun block ->
      let color = Block.get_color block in
      assert_bool "Valid color"
        (List.mem color
           [ Block.R; Block.G; Block.B; Block.Y; Block.P; Block.Pi; Block.O ]);
      let shape = Block.get_shape block in
      assert_bool "Medium shape"
        (List.mem shape
           [
             Block.big_sqr;
             Block.hor_line4;
             Block.vert_line4;
             Block.big_l;
             Block.inv_big_l;
             Block.t_up;
             Block.t_down;
             Block.t_left;
             Block.t_right;
           ]))
    blocks

let test_create_board _ =
  let board = Board.create_board 8 in
  assert_equal 8 (Board.size board);
  assert_equal Board.Empty (Board.get_cell board (0, 0));
  assert_equal Board.Empty (Board.get_cell board (7, 7))

let test_size _ =
  let board = Board.create_board 10 in
  assert_equal 10 (Board.size board);
  let board = Board.create_board 5 in
  assert_equal 5 (Board.size board)

let test_is_valid_pos _ =
  let board = Board.create_board 8 in
  assert_bool "Valid position (0, 0)" (Board.is_valid_pos board (0, 0));
  assert_bool "Valid position (7, 7)" (Board.is_valid_pos board (7, 7));
  assert_bool "Invalid position (-1, 0)"
    (not (Board.is_valid_pos board (-1, 0)));
  assert_bool "Invalid position (8, 8)" (not (Board.is_valid_pos board (8, 8)))

let test_cells _ =
  let board = Board.create_board 2 in
  Board.set_cell board (0, 0) (Block R);
  Board.set_cell board (1, 0) (Block P);
  assert_equal (Board.Block R) (Board.get_cell board (0, 0));
  assert_equal Board.Empty (Board.get_cell board (0, 1));
  assert_equal (Board.Block P) (Board.get_cell board (1, 0));
  assert_equal Board.Empty (Board.get_cell board (1, 1));
  assert_raises (Invalid_argument "Out of bounds") (fun () ->
      Board.set_cell board (2, 2) (Block B))

let test_block_placement _ =
  let board = Board.create_board 2 in

  let sqr_block = Block.create_block Block.R Block.sqr in
  Board.place_block board sqr_block (0, 0);
  assert_equal (Board.Block R) (Board.get_cell board (0, 0));
  assert_equal (Board.Block R) (Board.get_cell board (0, 1));
  assert_equal (Board.Block R) (Board.get_cell board (1, 0));
  assert_equal (Board.Block R) (Board.get_cell board (1, 1));

  let one_block = Block.create_block Block.G Block.big_sqr in
  assert_raises (Failure "Block cannot be placed there") (fun () ->
      Board.place_block board one_block (0, 0));

  let board = Board.create_board 2 in
  let big_sqr_block = Block.create_block Block.P Block.big_sqr in
  assert_raises (Failure "Block cannot be placed there") (fun () ->
      Board.place_block board big_sqr_block (0, 0))

let test_clear _ =
  let board = Board.create_board 2 in
  assert_equal ([], []) (Board.clear_full_lines board);
  let sqr_block = Block.create_block Block.R Block.sqr in
  Board.place_block board sqr_block (0, 0);
  assert_equal ([ 0; 1 ], [ 0; 1 ]) (Board.clear_full_lines board);
  assert_equal Board.Empty (Board.get_cell board (0, 0));
  assert_equal Board.Empty (Board.get_cell board (0, 1));
  assert_equal Board.Empty (Board.get_cell board (1, 0));
  assert_equal Board.Empty (Board.get_cell board (1, 1))

let test_no_moves _ =
  let board = Board.create_board 3 in
  let b1 = Block.create_block R Block.big_sqr in
  let b2 = Block.create_block R Block.big_l in
  let blocks = [ b1; b2 ] in
  assert_equal false (Board.no_moves board blocks);
  Board.place_block board b2 (0, 0);
  assert_equal true (Board.no_moves board [ b1 ])

let tests =
  "test suite"
  >::: [
         "test_block" >:: test_block;
         "test_random_block" >:: test_random_blocks;
         "test_easy_random_block" >:: test_easy_blocks;
         "test_medium_random_block" >:: test_medium_blocks;
         "test_create_board" >:: test_create_board;
         "test_size" >:: test_size;
         "test_is_valid_pos" >:: test_is_valid_pos;
         "test_cells" >:: test_cells;
         "test_block_placement" >:: test_block_placement;
         "test_clear" >:: test_clear;
         "test_no_moves" >:: test_no_moves;
       ]

let _ = run_test_tt_main tests
