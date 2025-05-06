open Blockblast
open States
open Raylib
open Raygui

module SM1 =
  StateMachine.AddState (StateMachine.EmptyStateMachine ()) (TitleState)

module SM = StateMachine.AddState (SM1) (PlayGameState)

let init () =
  init_window Constants.width Constants.height "BLOCKBLAST";
  set_target_fps Constants.fps;
  SM.init ()

let rec loop () =
  (if window_should_close () then close_window ()
   else
     let transition = SM.update () in
     SM.render ();
     match transition with
     | None -> ()
     | Some "reset" ->
         SM.reset ();
         SM.init ();
         SM.set_state "initial"
     | Some "close" -> close_window ()
     | Some s ->
         SM.set_state s;
         SM.init ());
  loop ()

let () =
  init ();
  loop ()

(* let () = Random.self_init () let () = States.TitleState.(setup () |> loop) *)

(* let () = States.TitleState.run (); init_window 800 900 "BlockBlast - Init
   State Test"; set_target_fps 60; let init_blocks = PlayGameState.init () in
   PlayGameState.loop init_blocks; close_window () *)
