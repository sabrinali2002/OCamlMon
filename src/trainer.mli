open Pokemon

type potion = int

type pokeball = int

type trainer = {
  name : string;
  team : pokemon list;
  bag : potion * pokeball;
}

val init_trainer : string -> pokemon -> trainer

val current_team : trainer -> pokemon list

val add_pokemon : trainer -> pokemon -> trainer
(** [add_pokemon t p] is a trainer with pokemon [p] added to the team of [t] *)
