open Graphics

(** [moveto (x,y)] is the same as graphic's [moveto], except this takes an
    explicit pair. Makes code cleaner. *)
let moveto (x, y) = moveto x y

(** [draw_image sp (x,y)] is the same as graphic's [draw_image], except this
    takes an explicit pair. Makes code cleaner. *)
let draw_image sp (x, y) = draw_image sp x y

(** [draw_image (x,y) w h)] is the same as graphic's [draw_rect], except
    this takes an explicit pair. Makes code cleaner. *)
let draw_rect (x, y) w h = draw_rect x y w h

(** [fill_rect (x,y) (w,h)] is the same as graphic's [draw_rect], except
    this takes an explicit pair. Makes code cleaner. *)
let fill_rect (x, y) w h = fill_rect x y w h

let fill_draw_rect (x, y) w h fill border =
  set_color fill;
  fill_rect (x, y) w h;
  set_color border;
  draw_rect (x, y) w h

(** [draw_square (x, y) f b] draws a 25x25 pixel square with the bottom-left
    corner at [(x, y)], with a fill color [f] and border color [b]. *)
let draw_square (x, y) fill border =
  set_color fill;
  fill_rect (x, y) 25 25;
  set_color border;
  draw_rect (x, y) 25 25

let draw_circle (x, y) r = draw_circle x y r

let draw_text (x, y) s =
  moveto (x, y);
  draw_string s

let print_bold s = ANSITerminal.print_string [ Bold ] s

let print_emph col s = ANSITerminal.print_string [ Bold; Foreground col ] s

let print s = print_endline s

let flush_kp () =
  while key_pressed () do
    let _ = read_key () in
    ()
  done