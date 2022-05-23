open Graphics
open Drawing
open Pokemon
open Trainer

type menu_data = { undone : int }

let init_menu trainer =
  set_color 0xf1c40f;
  fill_rect (0, 0) 500 500;
  set_color white
