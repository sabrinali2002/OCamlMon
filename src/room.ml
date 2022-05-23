open Yojson.Basic.Util
open Tile
open Parser

type exit_names = string

type room_id = string

type exit = {
  name : string;
  coordinates : int * int;
  player_coord : int * int; (*inital coordinate of player in new room*)
}

type room = {
  name : string;
  layout : tile array array;
  exits : exit list;
}

let grass = Grass { color = 0x2ecc71; encounters = [ Caterpie; Bulbasaur ] }

let sand = Sand { color = 0xf1c40f; encounters = [ Sandshrew; O ] }

let water = Water { color = 0x3498db; encounters = [ Squirtle ] }

let bushes = Bushes { color = 0x1c390f; encounters = [ Bulbasaur ] }

let rocks = Rocks { color = 0x808080; encounters = [ Geodude ] }

let path = Path { color = 0xffbd66; encounters = [] }

let unwal = Unwalkable { color = 0x2ecc71; encounters = [] }

let pokecenter = Pokecenter { color = 0x2ecc71; encounters = [] }

let beachgym = Beachgym { color = 0xf1c40f; encounters = [] }

let cavegym = Cavegym { color = 0x808080; encounters = [] }

let towngym = Towngym { color = 0x2ecc71; encounters = [] }

let npc = NPC { color = 0x817c91; encounters = [] }

(**TODO*)
let exit_to_room e = { name = ""; layout = [| [||] |]; exits = [] }

(* TODO: Add new parsing functions to .mli *)

let roomdata = "data/rooms.json" |> Yojson.Basic.from_file

(** [exit_record e] is an exit (record) representing a specific exit [e] in
    a room *)
let exit_record exit =
  let exitcoords = exit |> member "coordinates" in
  let playercoords = exit |> member "player_coord" in
  {
    name = exit |> member "name" |> to_string;
    coordinates =
      (index 0 exitcoords |> to_int, index 1 exitcoords |> to_int);
    player_coord =
      (index 0 playercoords |> to_int, index 1 playercoords |> to_int);
  }

(** [parse_exits r] is the list of exit (records) corresponding to the room
    [r]*)
let rec parse_exits = function
  | [] -> []
  | h :: t -> exit_record h :: parse_exits t

(** [from_json j r] is the room (record) of the room [r] from the JSON [j] *)
let from_json j room_name =
  let room = j |> member room_name in
  {
    name = room |> member "name" |> to_string;
    layout =
      room |> member "layout" |> to_list |> parse_list parse_tile
      |> Array.of_list;
    exits = parse_exits (room |> member "exits" |> to_list);
  }

let home = from_json roomdata "home"

let forest = from_json roomdata "forest"

let town = from_json roomdata "town"

let cave = from_json roomdata "cave"

let beach = from_json roomdata "beach"

let pokecenter = from_json roomdata "pokecenter"

let beachgym = from_json roomdata "beachgym"

let cavegym = from_json roomdata "cavegym"

let towngym = from_json roomdata "towngym"

let room_layout = function
  | "home" -> home.layout
  | "forest" -> forest.layout
  | "town" -> town.layout
  | "cave" -> cave.layout
  | "beach" -> beach.layout
  | "pokecenter" -> pokecenter.layout
  | "beachgym" -> beachgym.layout
  | "cavegym" -> cavegym.layout
  | "towngym" -> towngym.layout
  | _ -> failwith "No"

(* let get_tile coord room_name = (room_layout room_name).((fst
   coord)/25).((snd coord)/25) *)
let get_tile coord room_name =
  (room_layout room_name).(20 - (snd coord / 25)).(fst coord / 25)

let room_of_string = function
  | "home" -> home
  | "forest" -> forest
  | "town" -> town
  | "cave" -> cave
  | "beach" -> beach
  | "pokecenter" -> pokecenter
  | "beachgym" -> beachgym
  | "cavegym" -> cavegym
  | "towngym" -> towngym
  | _ -> failwith "No"
