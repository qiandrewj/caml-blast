type t = int
(** [t] is the type of data another state can send to the over state. *)

val name : string
(** [name] is ["over"]. *)

val set_default : bool
(** [set_default] is [false] because it is the state the game starts. *)

val set_buffer : t -> unit
(** [set_buffer data] sets the buffer of the over state. *)

val init : unit -> unit
(** [init ()] initializes the over state. *)

val update : unit -> string option
(** [update ()] checks for keyboard input. This state may transition into the
    [title] or [over] state. *)

val render : unit -> unit
(** [render ()] draws the output of the over screen. *)

val reset : unit -> unit
(** [show ()] resets the state to its inital state. *)
