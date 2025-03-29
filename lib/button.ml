open Raylib

type t =
  | Rect of {
      x : int;
      y : int;
      w : int;
      h : int;
    }

let make_rect_button (x, y) w h = Rect { x; y; w; h }

let button_clicked b (mx, my) =
  if is_mouse_button_pressed MouseButton.Left then
    match b with
    | Rect { x; y; w; h } -> (x <= mx && mx <= x + w) && y <= my && my <= y + h
  else false

let show_button b clr =
  match b with
  | Rect { x; y; w; h } ->
      let rect =
        Rectangle.create (float_of_int x) (float_of_int y) (float_of_int w)
          (float_of_int h)
      in
      draw_rectangle_rec rect clr
