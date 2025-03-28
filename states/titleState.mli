val width : int
val height : int

type t = int
(** [t] is the type of data another state can send to the title state. *)

val name : string
(** [name] is ["title"]. *)

val set_default : bool
(** [set_default] is [true] because it is the state the game starts. *)

val set_buffer : t -> unit
(** [set_buffer data] sets the buffer of the title state. *)

val init : unit -> unit
(** [init ()] initializes the title state. *)

val update : unit -> string option
(** [update ()] checks for keyboard input. This state may transition into the
    [initGame] state. *)

val show : unit -> unit
(** [show ()] draws the title screen. *)

val reset : unit -> unit
(** [show ()] resets the state to its inital state. *)

val load : unit -> unit
(** [load ()] loads sprites for the title state. *)
