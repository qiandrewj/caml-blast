open Blockblast
open States
open Raylib
open Raygui

(* let () = Random.self_init () let () = States.TitleState.(setup () |> loop) *)

let () =
  States.TitleState.run ();
  init_window 800 900 "BlockBlast - Init State Test";
  set_target_fps 60;
  let init_blocks = InitGameState.init () in
  InitGameState.loop init_blocks;
  close_window ()
