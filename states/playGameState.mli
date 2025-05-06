open Blockblast

type t
(** [t] is the type of data another state can send to the play state. *)

val name : string
(** [name] is ["play"]. *)

val set_default : bool
(** [set_default] is [flase] because it is not the state the game starts. *)

val set_buffer : t -> unit
(** [set_buffer data] sets the buffer of the play state. *)

val init : unit -> unit
(** [init ()] initializes the play state. *)

val update : unit -> string option
(** [update ()] checks for keyboard input. This state may transition into the
    [pause] state. *)

val render : unit -> unit
(* val show : unit -> unit * [show ()] draws the game board, current score,
   background, and active blocks. *)

val reset : unit -> unit
(** [show ()] resets the state to its inital state, including the score. *)

(* val run : unit -> unit *)