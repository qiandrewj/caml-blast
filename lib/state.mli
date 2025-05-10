(** [State] is a state in a state machine. It is one of the input signatures for
    the functor [AddState]. *)

module type State = sig
  type t
  (** [t] is the type of data states can receive and transmit. *)

  val name : string
  (** [name] is the name of the state. *)

  val set_default : bool
  (** [set_default] is true if the state is the default state of the state
      mahcine, meaning it overrides the current state when added to a machine.
  *)

  val set_buffer : t -> unit
  (** [set_buffer data] sets the buffer to some data value, which is data to
      send between states. *)

  val init : unit -> unit
  (** [init ()] initializes the graphics, board, blocks, etc. necessary for this
      state. This state should be called exactly once for each state. *)

  val update : unit -> string option
  (** [update ()] updates the state. This includes changing object positions,
      handling user input, scoring, etc. This function returns [Some state] to
      transition to state [state] or [None] to remain in the current state. *)

  val render : unit -> unit
  (** [render ()] draws to GUI. *)

  val reset : unit -> unit
  (** [reset ()] resets the state, which means setting up the game shoudl the
      user play again. *)
end
