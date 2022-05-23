open Pokemon
(* type color = int *)

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

let grass = Grass { color = 0x2ecc71; encounters = [ Caterpie; Bulbasaur ] }

let sand = Sand { color = 0xf1c40f; encounters = [ Sandshrew; O ] }

let water = Water { color = 0x3498db; encounters = [ Squirtle ] }

let bushes = Bushes { color = 0x1c390f; encounters = [ Bulbasaur ] }

let rocks = Rocks { color = 0x808080; encounters = [ Geodude ] }

let path = Path { color = 0xffbd66; encounters = [] }

let unwalkable = Unwalkable { color = 0x2ecc71; encounters = [] }

let pokecenter = Pokecenter { color = 0x2ecc71; encounters = [] }

let beachgym = Beachgym { color = 0xf1c40f; encounters = [] }

let cavegym = Cavegym { color = 0x808080; encounters = [] }

let towngym = Towngym { color = 0x2ecc71; encounters = [] }

let npc = NPC { color = 0x817c91; encounters = [] }

(**Generates a random number from 0 <= x <= 10 *)
let generate_randomNum x = Random.int 11

(*Determines whether an encounter happens based on the random num generator,
  1/010 chance *)
let generate_encounter () = generate_randomNum () > 9

(**generates a random pokemon tuple based on the tile*)
let type_pokemon = function
  | Grass _ ->
      if generate_randomNum () < 8 then
        Some (Caterpie, string_of_species Caterpie)
      else Some (Bulbasaur, string_of_species Bulbasaur)
  | Sand _ ->
      if generate_randomNum () < 9 then
        Some (Sandshrew, string_of_species Sandshrew)
      else Some (O, string_of_species O)
  | Water _ -> Some (Squirtle, string_of_species Squirtle)
  | Bushes _ -> Some (Bulbasaur, string_of_species Bulbasaur)
  | Rocks _ -> Some (Geodude, string_of_species Geodude)
  | Path _ -> None
  | Unwalkable _
  | Pokecenter _
  | Beachgym _
  | Cavegym _
  | Towngym _
  | NPC _ ->
      None

(**Generates a random moves based on the pokemon move list*)
let random_move l = List.nth l (Random.int (List.length l))

(**Generates a list of pokemon moves*)
let rec num_moves n l acc =
  match n with
  | 0 -> acc
  | x -> num_moves (n - 1) l (random_move l :: acc)
(**generates the list of moves the pokemon has*)

let generate_move level = function
  | Bulbasaur ->
      if level < 3 then [ Tackle ]
      else if level < 7 then num_moves 2 bulb_moves []
      else if level < 9 then num_moves 3 bulb_moves []
      else bulb_moves
  | Squirtle ->
      if level < 3 then [ Tackle ]
      else if level < 7 then num_moves 2 squirtle_moves []
      else if level < 9 then num_moves 3 squirtle_moves []
      else squirtle_moves
  | O ->
      if level < 5 then num_moves 1 o_moves []
      else if level < 9 then num_moves 2 o_moves []
      else o_moves
  | Caterpie ->
      if level < 5 then [ Tackle ]
      else if level < 8 then num_moves 2 caterpie_moves []
      else caterpie_moves
  | Sandshrew ->
      if level < 3 then [ Tackle ]
      else if level < 7 then num_moves 2 sand_moves []
      else if level < 9 then num_moves 3 sand_moves []
      else sand_moves
  | Geodude ->
      if level < 3 then [ Tackle ]
      else if level < 7 then num_moves 2 geodude_moves []
      else if level < 9 then num_moves 3 geodude_moves []
      else geodude_moves

let extract = function
  | Some x -> x
  | None -> failwith "error in tile extract"

(**Generates the random pokemon*)
let generate_pokemon tileT =
  match tileT with
  | Path _
  | Unwalkable _ ->
      None
  | _ ->
      Some
        (let typeP = type_pokemon tileT in
         let l = generate_randomNum () in
         let l = if l < 5 then 5 else l in
         let move_list = generate_move l (fst (extract typeP)) in
         let elt = element_of_species (fst (extract typeP)) in
         {
           name = snd (extract typeP);
           species = fst (extract typeP);
           element = elt;
           moves = move_list;
           stats =
             {
               health = 30;
               maxhealth = 30;
               speed = 30 + (10 * l);
               level = l;
               xp = (l * 10) + Random.int 10;
             };
         })

let string_of_tile = function
  | Grass t -> "Grass"
  | Sand t -> "Sand"
  | Water t -> "Water"
  | Bushes t -> "Bushes"
  | Rocks t -> "Rocks"
  | Path t -> "Path"
  | Unwalkable t -> "Unwal"
  | Pokecenter _ -> "Pokecenter"
  | Beachgym _ -> "Beachgym"
  | Cavegym _ -> "Cavegym"
  | Towngym _ -> "Towngym"
  | NPC _ -> "NPC"

let get_color = function
  | Grass t -> t.color
  | Sand t -> t.color
  | Water t -> t.color
  | Bushes t -> t.color
  | Rocks t -> t.color
  | Path t -> t.color
  | Unwalkable t -> t.color
  | Pokecenter t -> t.color
  | Beachgym t -> t.color
  | Cavegym t -> t.color
  | Towngym t -> t.color
  | NPC t -> t.color