open Yojson.Basic.Util

type species =
  | Pikachu
  | Bulbasaur
  | Squirtle
  | Charmander
  | Caterpie
  | Sandshrew
  | Geodude
  | O
  | Eevee

type element =
  | Fire
  | Water
  | Grass
  | Electric
  | Normal
  | Ground
  | Bug
  | Rock
  | Camel

type move =
  | TailWhip
  | Slam
  | ThunderShock
  | QuickAttack
  | ElectroBall
  | Tackle
  | VineWhip
  | RazorLeaf
  | SolarBeam
  | WaterGun
  | WaterPulse
  | HydroPump
  | Scratch
  | Ember
  | Flamethrower
  | Inferno
  | BugBite
  | Twineedle
  | PinMissile
  | LeechLife
  | RockThrow
  | Earthquake
  | Explosion
  | Encapsulate
  | Abstract
  | RaiseFail
  | TypeCheck

let pikachu_moves = [ Slam; ThunderShock; QuickAttack; ElectroBall ]

let bulb_moves = [ Tackle; VineWhip; RazorLeaf; SolarBeam ]

let squirtle_moves = [ Tackle; WaterGun; WaterPulse; HydroPump ]

let charmander_moves = [ Scratch; Ember; Flamethrower; Inferno ]

let caterpie_moves = [ Tackle; BugBite ]

let sand_moves = [ Tackle ]

let geodude_moves = [ Tackle; RockThrow; Earthquake; Explosion ]

let o_moves = [ Encapsulate; Abstract; RaiseFail ]

let eevee_moves = [ TailWhip; Scratch; QuickAttack ]

type stats = {
  health : int;
  maxhealth : int;
  speed : int;
  level : int;
  xp : int;
}

type pokemon = {
  name : string;
  species : species;
  element : element;
  moves : move list;
  stats : stats;
}

let make_starter_pokemon = function
  | "pikachu" ->
      {
        name = "Pikachu";
        species = Pikachu;
        element = Electric;
        moves = pikachu_moves;
        stats =
          { health = 18; maxhealth = 30; speed = 90; level = 5; xp = 0 };
      }
  | "squirtle" ->
      {
        name = "Squirtle";
        species = Squirtle;
        element = Water;
        moves = squirtle_moves;
        stats =
          { health = 10; maxhealth = 30; speed = 80; level = 5; xp = 0 };
      }
  | "bulbasaur" ->
      {
        name = "Bulbasaur";
        species = Bulbasaur;
        element = Grass;
        moves = bulb_moves;
        stats =
          { health = 30; maxhealth = 30; speed = 50; level = 5; xp = 0 };
      }
  | "charmander" ->
      {
        name = "Charmander";
        species = Charmander;
        element = Fire;
        moves = charmander_moves;
        stats =
          { health = 30; maxhealth = 30; speed = 60; level = 5; xp = 0 };
      }
  | _ -> failwith "Not a starter Pokemon"

let string_of_species = function
  | Pikachu -> "Pikachu"
  | Bulbasaur -> "Bulbasaur"
  | Squirtle -> "Squirtle"
  | Charmander -> "Charmander"
  | Caterpie -> "Caterpie"
  | Sandshrew -> "Sandshrew"
  | Geodude -> "Geodude"
  | O -> "O"
  | Eevee -> "Eevee"

let string_of_element = function
  | Fire -> "Fire"
  | Water -> "Water"
  | Grass -> "Grass"
  | Electric -> "Electric"
  | Normal -> "Normal"
  | Bug -> "Bug"
  | Rock -> "Rock"
  | Ground -> "Ground"
  | Camel -> "Camel"

let element_of_string = function
  | "Fire" -> Fire
  | "Water" -> Water
  | "Grass" -> Grass
  | "Electric" -> Electric
  | "Normal" -> Normal
  | "Bug" -> Bug
  | "Rock" -> Rock
  | "Ground" -> Ground
  | "Camel" -> Camel
  | _ -> Normal

let element_of_species = function
  | Pikachu -> Electric
  | Bulbasaur -> Grass
  | Squirtle -> Water
  | Charmander -> Fire
  | Caterpie -> Bug
  | Sandshrew -> Ground
  | Geodude -> Rock
  | O -> Camel
  | Eevee -> Normal

let string_of_pokemon pokemon = pokemon.name

let string_of_move = function
  | TailWhip -> "Tail Whip"
  | Slam -> "Slam"
  | ThunderShock -> "Thunder Shock"
  | QuickAttack -> "Quick Attack"
  | ElectroBall -> "Electro Ball"
  | Tackle -> "Tackle"
  | VineWhip -> "Vine Whip"
  | RazorLeaf -> "Razor Leaf"
  | SolarBeam -> "Solar Beam"
  | WaterGun -> "Water Gun"
  | WaterPulse -> "Water Pulse"
  | HydroPump -> "Hydro Pump"
  | Scratch -> "Scratch"
  | Ember -> "Ember"
  | Flamethrower -> "Flamethrower"
  | Inferno -> "Inferno"
  | BugBite -> "Bug Bite"
  | Twineedle -> "Twineedle"
  | PinMissile -> "Pin Missile"
  | LeechLife -> "Leech Life"
  | RockThrow -> "Rock Throw"
  | Earthquake -> "Earthquake"
  | Explosion -> "Explosion"
  | Encapsulate -> "Encapsulate"
  | Abstract -> "Abstract"
  | RaiseFail -> "Raise Fail"
  | TypeCheck -> "Type Check"

let power_of_move move =
  "data/moves.json" |> Yojson.Basic.from_file
  |> member (string_of_move move)
  |> member "power" |> to_int

let element_of_move move =
  "data/moves.json" |> Yojson.Basic.from_file
  |> member (string_of_move move)
  |> member "element" |> to_string |> element_of_string
