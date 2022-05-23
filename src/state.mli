open Room
open Trainer
open Pokemon
open Menu

type battle_menu =
  | Main of int
  | Fight of int
  | Bag of int
  | OCamlMon of int
  | Flee

type battle_data = {
  friend : pokemon;
  friend_team : pokemon list;
  enemy : pokemon;
  enemy_team : pokemon list;
  menu : battle_menu;
  items : potion * pokeball;
  is_gym : bool;
}

type action =
  | Battle of battle_data
  | Walk
  | Menu of menu_data

type t

val current_room : t -> string

val current_coord : t -> int * int

val current_action : t -> action

val current_trainer : t -> trainer

val current_battle : t -> battle_data

val init_state : trainer -> t

val update_state : string -> int * int -> action -> trainer -> t

val update_action : t -> action -> t

val update_trainer : t -> trainer -> t
