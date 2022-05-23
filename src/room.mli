open Tile

type exit = {
  name : string;
  coordinates : int * int;
  player_coord : int * int;
}

type room = {
  name : string;
  layout : tile array array;
  exits : exit list;
}

val exit_to_room : exit -> room
(** This might go in trainer.ml instead *)

val get_tile : int * int -> string -> tile
(**Gets the tile of the room to draw over the tile after moving to a new
   tile*)

val room_layout : string -> tile array array

val room_of_string : string -> room