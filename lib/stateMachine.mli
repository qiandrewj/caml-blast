open State

exception Invalid_transition of string
(** [Invalid_transition str] is thrown when a state attempts an
    invalid/impossible transition as designed by the state machine. *)

exception Empty_state_machine
(** [Empty_state_machine] is trhown when attempting to update an empty state
    machine. *)

(** [StateMachine] is collection of states with distinct logic and supports
    transitions between them. It is one of the input signature and the output
    signature of the functor [AddState]. *)
module type StateMachineSig = sig
  val current_state : string option ref
  (** [current_state] is the current state of the state machine. *)

  val get_states : unit -> string array
  (** [get_states ()] is a an array of the supported states of the state
      machine. *)

  val get_state : unit -> string option
  (** [get_state ()] is the current state of the state machine. [None] if it is
      the empty state machine. *)

  val set_state : string -> unit
  (** [set_state state] changes the current state of the state machine to
      [state]. Requires: [set_state] is an element of [states]. *)

  val init : unit -> unit
  (** [init ()] initializes all states in the state machine. *)

  val update : unit -> string option
  (** [update ()] updates the state whose name matches [current_state] and
      returns the next state to transition to. This is [None] if the state
      machine remains in its current state. Raises: [Invalid_transition] if the
      function is ran while [current_state] is not an element of [states]. *)

  val render : unit -> unit
  (** [render ()] draws the state whose name matches [current_state]. Raises:
      [Invalid_transition] if the function is ran while [current_state] is not
      an element of [states]. *)

  val reset : unit -> unit
  (**[reset ()] resets all states in the state machine. *)
end

(** [EmptyStateMachine ()] is a state machine with no states. [current_state] is
    [None]. Raises: [Empty_state_machine] if [init], [update], and [render] are
    called. *)
module EmptyStateMachine : functor () -> StateMachineSig

(** [AddState (M) (S)] is a functor that adds state [S] to state machine [M].
    The resulting state machine with have the state [S] as well as the states in
    [M.states]. The resulting current state will be [S.name] if and only if
    [S.set_default] is [true] or [M = EmptyStateMachine]. Requires: [S.name] is
    not in [M.states]. *)
module AddState : functor (_ : StateMachineSig) (_ : State) -> StateMachineSig
