open Pokemon

type t = {
  color : int;
  encounters : species list;
}

type tile =
  | Grass of t
  | Sand of t
  | Water of t
  | Bushes of t
  | Rocks of t
  | Path of t
  | Unwalkable of t
  | Pokecenter of t
  | Beachgym of t
  | Cavegym of t
  | Towngym of t
  | NPC of t

val generate_encounter : unit -> bool
(** [generate_encounter x] is whether or not stepping on a tile will create
    a random encounter based on a generated random number *)

val generate_pokemon : tile -> pokemon option
(** [generate_pokemon n] is the pokemon that is generated based on the
    random number generated and the tile that the trainer is currently on,
    taking into account the tile's encounters list and the rarity of the
    pokemon within the list NOTE: Planning to use conditionals and define
    ranges for rarity within this method. *)

val get_color : tile -> int

val string_of_tile : tile -> string
