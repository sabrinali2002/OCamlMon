open Pokemon

type potion = int

type pokeball = int

type trainer = {
  name : string;
  team : pokemon list;
  bag : potion * pokeball;
}

let init_trainer name starter = { name; team = [ starter ]; bag = (5, 5) }

let current_team t = t.team

let add_pokemon t p = { t with team = t.team @ [ p ] }
