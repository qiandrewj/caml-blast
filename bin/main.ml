open Blockblast
open States
open Raylib
open Raygui

module SM1 =
  StateMachine.AddState (StateMachine.EmptyStateMachine ()) (PlayEasyState)

module SM2 = StateMachine.AddState (SM1) (PlayHardState)
module SM3 = StateMachine.AddState (SM2) (OverState)
module SM = StateMachine.AddState (SM3) (TitleState)

let init () =
  init_window Constants.width Constants.height "BLOCKBLAST";
  set_target_fps Constants.fps;
  SM.set_state "title";
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
         SM.set_state "title"
     | Some "close" -> close_window ()
     | Some s ->
         SM.set_state s;
         SM.init ());
  loop ()

let () =
  init ();
  loop ()
