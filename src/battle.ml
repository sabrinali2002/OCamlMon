(* Battle text -> Text in main rectangle *)
(* Friendly status -> Player's pokemon status bar *)
(* Enemy status -> Enemy pokemon status bar *)
(* Menu -> Menu navigation in bottom right - Fight, Bag, OCamlMon, Run *)
open Yojson.Basic.Util
open Graphics
open Drawing
open Parser
open Pokemon
open State

(* CONSTANTS *)

type main_text_positions = {
  fight : int * int;
  bag : int * int;
  ocamlmon : int * int;
  run : int * int;
  friendly : int * int;
  enemy : int * int;
}

type effect =
  | Super
  | Standard
  | Less
  | Resistant

let main_text =
  {
    fight = (260, 70);
    bag = (385, 70);
    ocamlmon = (260, 20);
    run = (385, 20);
    friendly = (250, 250);
    enemy = (250, 250);
  }

let main_underline_pos =
  {
    fight = (260, 68);
    bag = (385, 68);
    ocamlmon = (260, 18);
    run = (385, 18);
    friendly = (250, 250);
    enemy = (250, 250);
  }

type move_text_positions = {
  first : int * int;
  second : int * int;
  third : int * int;
  fourth : int * int;
}

let move_text =
  {
    first = (80, 65);
    second = (250, 65);
    third = (80, 25);
    fourth = (250, 25);
  }

let move_circle =
  {
    first = (70, 71);
    second = (240, 71);
    third = (70, 31);
    fourth = (240, 31);
  }

type bag_text_positions = {
  potions : int * int;
  pokeballs : int * int;
}

let bag_text = { potions = (150, 45); pokeballs = (300, 45) }

let bag_circle = { potions = (140, 51); pokeballs = (290, 51) }

type team_text_positions = {
  one : int * int;
  two : int * int;
  three : int * int;
  four : int * int;
  five : int * int;
  six : int * int;
}

let team_text =
  {
    one = (80, 65);
    two = (200, 65);
    three = (320, 65);
    four = (80, 25);
    five = (200, 25);
    six = (320, 25);
  }

let team_circle =
  {
    one = (70, 71);
    two = (190, 71);
    three = (310, 71);
    four = (70, 31);
    five = (190, 31);
    six = (310, 31);
  }

(* type battle_menu = | Main of int | Fight of int | Bag of int | OCamlMon
   of int | Flee

   type battle_data = { friend : pokemon; enemy : pokemon; enemy_team :
   pokemon list; menu : battle_menu; } *)

(* END CONSTANTS *)

let calculate_types source target =
  match source with
  | Fire -> begin
      match target with
      | Grass
      | Rock
      | Bug ->
          Super
      | Fire
      | Water
      | Camel ->
          Less
      | _ -> Standard
    end
  | Water -> begin
      match target with
      | Fire
      | Ground
      | Rock ->
          Super
      | Water
      | Grass
      | Camel ->
          Less
      | _ -> Standard
    end
  | Grass -> begin
      match target with
      | Water
      | Ground
      | Rock ->
          Super
      | Fire
      | Grass
      | Bug
      | Camel ->
          Less
      | _ -> Standard
    end
  | Electric -> begin
      match target with
      | Water -> Super
      | Electric
      | Grass
      | Camel ->
          Less
      | Ground -> Resistant
      | _ -> Standard
    end
  | Normal -> begin
      match target with
      | Rock
      | Camel ->
          Less
      | _ -> Standard
    end
  | Ground -> begin
      match target with
      | Fire
      | Electric
      | Rock ->
          Super
      | Grass
      | Bug
      | Camel ->
          Less
      | _ -> Standard
    end
  | Bug -> begin
      match target with
      | Grass -> Super
      | Fire
      | Camel ->
          Less
      | _ -> Standard
    end
  | Rock -> begin
      match target with
      | Fire
      | Bug ->
          Super
      | Camel -> Less
      | _ -> Standard
    end
  | Camel -> Super

let reset_battle_text () = fill_draw_rect (0, 0) 250 100 white black

let draw_battle_text top bottom =
  reset_battle_text ();
  match bottom with
  | "" -> draw_text (25, 45) top
  | bot ->
      draw_text (25, 55) top;
      draw_text (25, 35) bot

let draw_menu_main selector from_main =
  auto_synchronize false;
  if not from_main then fill_draw_rect (0, 0) 500 100 white black;
  fill_draw_rect (250, 0) 125 50 white black;
  fill_draw_rect (375, 0) 125 50 white black;
  fill_draw_rect (250, 50) 125 50 white black;
  fill_draw_rect (375, 50) 125 50 white black;
  draw_text main_text.fight "Fight";
  draw_text main_text.bag "Bag";
  draw_text main_text.ocamlmon "OCamlMon";
  draw_text main_text.run "Run";
  (match selector with
  | 1 -> draw_rect main_underline_pos.fight 26 0
  | 2 -> draw_rect main_underline_pos.bag 15 0
  | 3 -> draw_rect main_underline_pos.ocamlmon 50 0
  | 4 -> draw_rect main_underline_pos.run 15 0
  | _ -> ());
  auto_synchronize true

let rec draw_moves acc = function
  | [] -> ()
  | h :: t -> (
      match acc with
      | 1 ->
          draw_text move_text.first (string_of_move h);
          draw_moves (acc + 1) t
      | 2 ->
          draw_text move_text.second (string_of_move h);
          draw_moves (acc + 1) t
      | 3 ->
          draw_text move_text.third (string_of_move h);
          draw_moves (acc + 1) t
      | 4 ->
          draw_text move_text.fourth (string_of_move h);
          draw_moves (acc + 1) t
      | _ -> ())
(* let moves = battle.friend.moves in match moves with | [ m1; m2; m3; m4 ]
   -> draw_text move_text.first (string_of_move m1); draw_text
   move_text.second (string_of_move m2); draw_text move_text.third
   (string_of_move m3); draw_text move_text.fourth (string_of_move m4) | _
   -> failwith "Error: Not enough moves" *)

let draw_menu_fight trainer battle selector =
  auto_synchronize false;
  fill_draw_rect (0, 0) 400 100 white black;
  fill_draw_rect (400, 0) 100 100 white black;
  draw_moves 1 battle.friend.moves;
  (match selector with
  | 1 -> draw_circle move_circle.first 5
  | 2 -> draw_circle move_circle.second 5
  | 3 -> draw_circle move_circle.third 5
  | 4 -> draw_circle move_circle.fourth 5
  | _ -> ());
  auto_synchronize true

let draw_menu_bag battle selector =
  auto_synchronize false;
  fill_draw_rect (0, 0) 500 100 white black;
  draw_text bag_text.potions (string_of_int (fst battle.items) ^ " Potions");
  draw_text bag_text.pokeballs
    (string_of_int (snd battle.items) ^ " Pokeballs");
  (match selector with
  | 1 -> draw_circle bag_circle.potions 5
  | 2 -> draw_circle bag_circle.pokeballs 5
  | _ -> ());
  auto_synchronize true

let rec draw_team acc = function
  | [] -> ()
  | h :: t ->
      let name = h.name in
      (match acc with
      | 1 -> draw_text team_text.one name
      | 2 -> draw_text team_text.two name
      | 3 -> draw_text team_text.three name
      | 4 -> draw_text team_text.four name
      | 5 -> draw_text team_text.five name
      | 6 -> draw_text team_text.six name
      | _ -> ());
      draw_team (acc + 1) t

let draw_menu_team battle selector =
  auto_synchronize false;
  fill_draw_rect (0, 0) 500 100 white black;
  draw_team 1 battle.friend_team;
  (match selector with
  | 1 -> draw_circle team_circle.one 5
  | 2 -> draw_circle team_circle.two 5
  | 3 -> draw_circle team_circle.three 5
  | 4 -> draw_circle team_circle.four 5
  | 5 -> draw_circle team_circle.five 5
  | 6 -> draw_circle team_circle.six 5
  | _ -> ());
  auto_synchronize true

let rec next_valid_pokemon acc = function
  | [] -> failwith "Error: No valid Pokemon"
  | h :: t ->
      if h.stats.health > 0 then (h, h :: (t @ acc))
      else next_valid_pokemon (h :: acc) t

let draw_friend pokemon =
  draw_image (image_from_json "pokemon.json" "Pikachu") (50, 125)
(* draw_image (image_from_json "pokemon.json" (string_of_species
   pokemon.species)) (50, 125) *)

let draw_enemy pokemon =
  draw_image (image_from_json "pokemon.json" "Pikachu") (300, 325)
(* draw_image (image_from_json "pokemon.json" (string_of_species
   pokemon.species)) (300, 325) NEED TO REPLACE AFTER WE GET ALL SPRITES
   IN*)

let ( /. ) x y = float_of_int x /. float_of_int y

let draw_poke_status (x, y) pokemon =
  fill_draw_rect (x, y) 250 75 white black;
  draw_text (x + 10, y + 50) (string_of_pokemon pokemon);
  draw_text (x + 220, y + 50) ("Lv" ^ string_of_int pokemon.stats.level);
  draw_text
    (x + 175, y + 10)
    (string_of_int pokemon.stats.health
    ^ " / "
    ^ string_of_int pokemon.stats.maxhealth);
  set_color green;
  fill_rect
    (x + 75, y + 25)
    (int_of_float
       (150. *. (pokemon.stats.health /. pokemon.stats.maxhealth)))
    10;
  draw_rect (x + 75, y + 25) 150 10

let init_battle trainer enemy_team bg_color npc =
  let friendlies = next_valid_pokemon [] (Trainer.current_team trainer) in
  let data =
    match enemy_team with
    | h :: t ->
        {
          friend = fst friendlies;
          friend_team = snd friendlies;
          enemy = h;
          enemy_team = t;
          menu = Main 1;
          items = trainer.bag;
          is_gym = npc;
        }
    | _ -> failwith "init_battle battle.ml: No team"
  in
  auto_synchronize false;
  clear_graph ();
  set_color bg_color;
  fill_rect (0, 0) 500 500;
  set_color black;
  (* DRAWING BOTTOM MENU *)
  draw_menu_main 1 true;
  (* DRAWING POKEMON RECTANGLES*)
  fill_draw_rect (50, 125) 150 150 white black;
  draw_friend data.friend;
  fill_draw_rect (300, 325) 150 150 white black;
  draw_enemy data.enemy;
  (* DRAWING POKEMON STATUS BARS *)
  (* fill_draw_rect (250, 150) 250 75 white black; *)
  draw_poke_status (250, 150) data.friend;
  (* fill_draw_rect (0, 375) 250 75 white black; *)
  draw_poke_status (0, 375) data.enemy;
  let top, bot =
    if npc then ("What will you do?", "")
    else ("You run into a wild Pokemon!", "What will you do?")
  in
  draw_battle_text top bot;
  auto_synchronize true;
  data

let go_back_menu battle menu =
  match menu with
  | Fight _
  | Bag _
  | OCamlMon _ ->
      draw_menu_main 1 false;
      { battle with menu = Main 1 }
  | Main _
  | Flee ->
      battle

let use_potion st trainer battle menu =
  let hp = battle.friend.stats.health in
  let maxhp = battle.friend.stats.maxhealth in
  if hp < maxhp && fst battle.items > 0 then (
    let new_stats =
      { battle.friend.stats with health = min (hp + 15) maxhp }
    in
    let healed = { battle.friend with stats = new_stats } in
    draw_menu_main 2 false;
    draw_battle_text
      ("You healed your " ^ string_of_pokemon healed)
      ("to " ^ string_of_int healed.stats.health ^ " hp!");
    draw_poke_status (250, 150) healed;
    {
      battle with
      menu = Main 2;
      friend = healed;
      friend_team = healed :: List.tl battle.friend_team;
      items = (fst battle.items - 1, snd battle.items);
    })
  else (
    draw_menu_main 2 false;
    draw_battle_text "Your Pokemon is already at full" "health!";
    { battle with menu = Main 2 })

let use_pokeball st trainer battle menu =
  if (not battle.is_gym) && List.length battle.friend_team < 6 then
    let hp = battle.enemy.stats.health in
    let maxhp = battle.enemy.stats.maxhealth in
    let upper = 100 * ((maxhp - hp) / maxhp) in
    if Random.int 100 < 101 then
      {
        battle with
        menu = Flee;
        friend_team = battle.enemy :: battle.friend_team;
        items = (fst battle.items, snd battle.items - 1);
      }
    else (
      draw_menu_main 2 false;
      draw_battle_text "The Pokemon broke free from" "the Pokeball!";
      {
        battle with
        menu = Main 1;
        items = (fst battle.items, snd battle.items - 1);
      })
  else go_back_menu battle menu

let update_move_text move source effect =
  fill_draw_rect (0, 0) 400 100 white black;
  let effectiveness =
    match effect with
    | Super -> "It was super effective!"
    | Standard -> ""
    | Less -> "It was not very effective..."
    | Resistant -> "It had no effect..."
  in
  draw_text (30, 60)
    (source.name ^ " used " ^ string_of_move move ^ "! " ^ effectiveness);
  Unix.sleep 2;
  flush_kp ()

(* element multipluer * base power * level / 50 *)
let damage_pokemon move source target friend_target =
  let power = float_of_int (power_of_move move) in
  (* print (string_of_move move); print (string_of_element (element_of_move
     move)); print (string_of_element target.element); *)
  let effect = calculate_types (element_of_move move) target.element in
  let multiplier =
    match effect with
    | Super -> 2.
    | Standard -> 1.
    | Less -> 0.5
    | Resistant -> 0.
  in
  let damage =
    int_of_float (multiplier *. power) * source.stats.level / 50
  in
  let new_stats =
    { target.stats with health = max (target.stats.health - damage) 0 }
  in
  let pokemon = { target with stats = new_stats } in
  draw_poke_status (if friend_target then (250, 150) else (0, 375)) pokemon;
  update_move_text move source effect;
  pokemon

let update_teams battle friend enemy =
  draw_menu_main 1 false;
  {
    battle with
    menu = Main 1;
    friend;
    enemy;
    friend_team = friend :: List.tl battle.friend_team;
  }

let process_moves st battle pos =
  let friend_move = List.nth battle.friend.moves (pos - 1) in
  let enemy_move =
    List.nth battle.enemy.moves
      (Random.int (List.length battle.enemy.moves))
  in
  if battle.enemy.stats.speed > battle.friend.stats.speed then
    let friend =
      damage_pokemon enemy_move battle.enemy battle.friend true
    in
    if friend.stats.health = 0 then { battle with menu = Flee }
    else
      let enemy = damage_pokemon friend_move friend battle.enemy false in
      if enemy.stats.health = 0 then { battle with menu = Flee }
      else update_teams battle friend enemy
  else
    let enemy =
      damage_pokemon friend_move battle.friend battle.enemy false
    in
    if enemy.stats.health = 0 then { battle with menu = Flee }
    else
      let friend = damage_pokemon enemy_move enemy battle.friend true in
      if friend.stats.health = 0 then { battle with menu = Flee }
      else update_teams battle friend enemy

(** Processes pattern matching for [interact_menu] when changing pokemon.
    Trial to see how factoring out pattern matching goes.*)
let process_team_change st trainer battle menu pos =
  if pos > 1 then (
    let current = battle.friend in
    let target = List.nth battle.friend_team (pos - 1) in
    draw_friend target;
    draw_poke_status (250, 150) target;
    draw_menu_main 3 false;
    draw_battle_text
      ("You switched out " ^ string_of_pokemon current)
      ("with " ^ string_of_pokemon target ^ "!");
    let new_team =
      target :: List.filteri (fun i p -> i != pos - 1) battle.friend_team
    in
    { battle with menu = Main 3; friend = target; friend_team = new_team })
  else (
    draw_menu_main 3 false;
    { battle with menu = Main 3 })

(* let move_main_pointer *)
let interact_menu st trainer battle menu =
  match menu with
  | Main pos -> begin
      match pos with
      | 1 ->
          draw_menu_fight trainer battle 1;
          { battle with menu = Fight 1 }
      | 2 ->
          draw_menu_bag battle 1;
          { battle with menu = Bag 1 }
      | 3 ->
          draw_menu_team battle 1;
          { battle with menu = OCamlMon 1 }
      | 4 -> { battle with menu = Flee }
      | _ ->
          print "1 [interact_menu]; impossible position";
          battle
    end
  | Fight pos -> process_moves st battle pos
  | Bag pos -> begin
      match pos with
      | 1 -> use_potion st trainer battle menu
      | 2 -> use_pokeball st trainer battle menu
      | _ ->
          print "2 [interact_menu]; impossible position";
          battle
    end
  | OCamlMon pos -> process_team_change st trainer battle menu pos
  | _ -> failwith "Impossible?"

let update_main_menu battle pos (cond1, cond2) delta =
  if pos = cond1 || pos = cond2 then (
    draw_menu_main (pos + delta) true;
    { battle with menu = Main (pos + delta) })
  else battle

let update_fight_menu trainer battle pos (cond1, cond2) delta =
  if
    pos = cond1
    || (pos = cond2 && pos + delta <= List.length battle.friend.moves)
  then (
    draw_menu_fight trainer battle (pos + delta);
    { battle with menu = Fight (pos + delta) })
  else battle

let update_bag_menu trainer battle pos =
  let pos = if pos = 1 then 2 else 1 in
  draw_menu_bag battle pos;
  { battle with menu = Bag pos }

let update_team_menu trainer battle pos (cond1, cond2, cond3, cond4) delta =
  if
    (pos = cond1 || pos = cond2 || pos = cond3 || pos = cond4)
    && pos + delta <= List.length (Trainer.current_team trainer)
  then (
    draw_menu_team battle (pos + delta);
    { battle with menu = OCamlMon (pos + delta) })
  else battle

let update_battle_menu st trainer battle key =
  let b_menu = battle.menu in
  match key with
  | 'w' -> begin
      match b_menu with
      | Main pos -> update_main_menu battle pos (3, 4) ~-2
      | Fight pos -> update_fight_menu trainer battle pos (3, 4) ~-2
      | Bag pos -> battle
      | OCamlMon pos -> update_team_menu trainer battle pos (4, 5, 6, 6) ~-3
      | _ -> failwith "Impossible?"
    end
  | 'a' -> begin
      match b_menu with
      | Main pos -> update_main_menu battle pos (2, 4) ~-1
      | Fight pos -> update_fight_menu trainer battle pos (2, 4) ~-1
      | Bag pos -> update_bag_menu trainer battle pos
      | OCamlMon pos -> update_team_menu trainer battle pos (2, 3, 5, 6) ~-1
      | _ -> failwith "Impossible?"
    end
  | 's' -> begin
      match b_menu with
      | Main pos -> update_main_menu battle pos (1, 2) 2
      | Fight pos -> update_fight_menu trainer battle pos (1, 2) 2
      | Bag pos -> battle
      | OCamlMon pos -> update_team_menu trainer battle pos (1, 2, 3, 3) 3
      | _ -> failwith "Impossible?"
    end
  | 'd' -> begin
      match b_menu with
      | Main pos -> update_main_menu battle pos (1, 3) 1
      | Fight pos -> update_fight_menu trainer battle pos (1, 3) 1
      | Bag pos -> update_bag_menu trainer battle pos
      | OCamlMon pos -> update_team_menu trainer battle pos (1, 2, 4, 5) 1
      | _ -> failwith "Impossible?"
    end
  | 'e' -> interact_menu st trainer battle b_menu
  | 'q' -> go_back_menu battle b_menu
  | _ -> battle

(* let run_battle trainer = function | "w" -> failwith "Unimp" | "a" ->
   failwith "Unimp" | "s" -> failwith "Unimp" | "d" -> failwith "Unimp" | _
   -> trainer *)

(* 1 2 3 4 *)
