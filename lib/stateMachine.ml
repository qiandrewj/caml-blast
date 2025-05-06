exception Invalid_transition of string
exception Empty_state_machine

let array_to_string f arr =
  "[|" ^ Array.fold_left (fun acc x -> acc ^ f x) "" arr ^ "]|"

let raise_invalid_state state states =
  let exn =
    match !state with
    | None -> "Empty state machine"
    | Some x ->
        "Invalid transition to " ^ x ^ ": only possible states "
        ^ array_to_string (fun x -> x) states
  in
  raise (Invalid_transition exn)

module type State = sig
  type t

  val name : string
  val set_default : bool
  val set_buffer : t -> unit
  val init : unit -> unit
  val update : unit -> string option
  val render : unit -> unit
  val reset : unit -> unit
end

module type StateMachineSig = sig
  val current_state : string option ref
  val get_states : unit -> string array
  val get_state : unit -> string option
  val set_state : string -> unit
  val init : unit -> unit
  val update : unit -> string option
  val render : unit -> unit
  val reset : unit -> unit
end

module EmptyStateMachine () : StateMachineSig = struct
  let states = [||]
  let get_states () = Array.copy states
  let current_state = ref None
  let get_state () = !current_state
  let set_state state = raise_invalid_state (ref (Some state)) states
  let init () = ()
  let update () = raise_invalid_state current_state states
  let render () = raise_invalid_state current_state states
  let reset () = ()
end

module AddState (M : StateMachineSig) (S : State) : StateMachineSig = struct
  let states = Array.append (M.get_states ()) [| S.name |]
  let get_states () = Array.copy states

  let current_state =
    let () =
      match !M.current_state with
      | None -> M.current_state := Some S.name
      | Some _ -> if S.set_default then M.current_state := Some S.name
    in
    M.current_state

  let get_state () = !current_state
  let set_state state = current_state := Some state

  let init () =
    S.init ();
    try M.init () with
    | Failure _ -> ()
    | Empty_state_machine -> ()

  let reset () =
    S.reset ();
    try M.reset () with
    | Failure _ -> ()
    | Empty_state_machine -> ()

  let update () =
    let curr_state =
      match !current_state with
      | None -> raise Empty_state_machine
      | Some s -> s
    in
    if curr_state = S.name then S.update () else M.update ()

  let render () =
    let curr_state =
      match !current_state with
      | None -> raise Empty_state_machine (* Should never reach here *)
      | Some s -> s
    in
    if curr_state = S.name then S.render () else M.render ()
end
