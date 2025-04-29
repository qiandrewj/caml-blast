type t
(** [t] is the type of button. *)

val make_rect_button : int * int -> int -> int -> t
(** [make_rect_button (x, y) w h] makes a rectangular button with the upper left
    corner at [(x, y)] and of width [w] and height [h]. *)

(* val make_circ_button : int * int -> int -> t (** [make_circ_button (x, y) r]
   makes a circular button with the center at [(x, y)] and of radius [r]. *) *)

val button_clicked : t -> int * int -> bool
(** [button_clicked b (mx, my)] returns if button [b] was left-clicked by user
    cursor at [(mx, my)]. *)

val show_button : t -> Raylib.Color.t -> unit
(** [show_button b] draws the button onto the user screen. *)
