open Yojson.Basic.Util
open Graphics
open Tile

(* TODO Add .mli file*)

(** [parse_color c] is the Graphics library color type corresponding to an
    identically-named string [c]. Defaults to white if there is no match. *)
let parse_color c =
  let c = to_string c in
  match c with
  | "transp" -> transp
  | "black" -> black
  | "white" -> white
  | "red" -> red
  | "green" -> red
  | "blue" -> blue
  | "yellow" -> yellow
  | "cyan" -> cyan
  | "magenta" -> magenta
  | others -> int_of_string others

(** [parse_tile t] is the Tile type correspnding to an identically-named
    string [t]. Defaults to grass if there is no match. *)
let parse_tile t =
  let t = to_string t in
  match t with
  | "grass" ->
      Grass { color = 0x2ecc71; encounters = [ Caterpie; Bulbasaur ] }
  | "path" -> Path { color = 0xffbd66; encounters = [] }
  | "water" -> Water { color = 0x3498db; encounters = [ Squirtle ] }
  | "sand" -> Sand { color = 0xf1c40f; encounters = [ Sandshrew; O ] }
  | "bushes" -> Bushes { color = 0x1c390f; encounters = [ Bulbasaur ] }
  | "rocks" -> Rocks { color = 0x808080; encounters = [ Geodude ] }
  | "unwal" -> Unwalkable { color = 0x2ecc71; encounters = [] }
  | "poke" -> Pokecenter { color = 0x2ecc71; encounters = [] }
  | "bgym" -> Beachgym { color = 0x2ecc71; encounters = [] }
  | "cgym" -> Cavegym { color = 0x808080; encounters = [] }
  | "tgym" -> Towngym { color = 0x2ecc71; encounters = [] }
  | "npc" -> NPC { color = 0x817c91; encounters = [] }
  | _ -> Grass { color = 0x2ecc71; encounters = [ Caterpie; Bulbasaur ] }

(** [parse_inner_list f j] is a list of [j] parsed according to [f], where
    [j] is a Yojson.Basic.t list of strings, essentially for an inner-list
    of the 2D list from the JSON file. [f] is the criteria to parse the
    strings. The order of [f] and [j] as arguments are for pipelining
    purposes.*)
let rec parse_inner_list f j =
  match j with
  | [] -> []
  | h :: t -> f h :: parse_inner_list f t

(** [parse_list f j] is the 2D array of the Yojson.Basic.t list [j], where
    [j] is a 2D list containing more Yojson.Basic.t lists of values for the
    sprite, where [f] is some function to parse the inner values. The order
    of [f] and [j] as arguments are for pipelining purposes. *)
let rec parse_list f j =
  match j with
  | [] -> []
  | h :: t ->
      (h |> to_list |> parse_inner_list f |> Array.of_list)
      :: parse_list f t

let image_from_json json field =
  make_image
    ("data/" ^ json |> Yojson.Basic.from_file |> member field |> to_list
   |> parse_list parse_color |> Array.of_list)
