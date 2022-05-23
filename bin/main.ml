open Yojson.Basic.Util
open Graphics
open Game
open State
open Room
open Tile
open Parser
open Trainer
open Pokemon
open Drawing
open Battle
open Menu
open Npc_data

let mainWorldlist = []

(* let tsprite = "data/player.json" |> Yojson.Basic.from_file |> member
   "sprite"

   let spritelist = tsprite |> to_list

   let player = spritelist |> parse_list parse_color |> Array.of_list *)

(** [is_exit x y r] is whether or not the given coordinates [(x, y)]
    correspond to a valid exit in the given room [r]. *)
let rec is_exit (x, y) = function
  | [] -> false
  | h :: t ->
      if x == fst h.coordinates && y == snd h.coordinates then true
      else is_exit (x, y) t

(** [take_exit r (x, y)] is the room cooresponding with the exit as (x, y) *)
let rec take_exit room (x, y) = function
  | [] -> room
  | h :: t ->
      if (x, y) = h.coordinates then h.name else take_exit room (x, y) t

(** [enter_room r x y] is the string for the room that the player should be
    in after moving, depending on their current room [r] and their current
    position [(x, y)] *)
let enter_room room (x, y) =
  let room' = room_of_string room in
  let pos_exits = room'.exits in
  take_exit room (x, y) pos_exits

(** [new_room_coords x y] is the position of the player of the next room
    after taking an exit. For instance, if the player exits to the right of
    a room, their start position of the next room will be on the left. *)
let rec exit_coord (x, y) exit_list =
  match exit_list with
  | [] -> failwith "error with exits"
  | h :: t ->
      let the_exit = h.coordinates in
      if the_exit = (x, y) then h.player_coord else exit_coord (x, y) t
let extract = function
| None -> failwith "extract error"
| Some v -> v

let pokecenter = ref None

let beachgym = ref None

let cavegym = ref None

let towngym = ref None

let draw_pokecenter coord = if !pokecenter = None then
  pokecenter :=
    Some
      (make_image
      ("data/rooms.json" |> Yojson.Basic.from_file
      |> member "pokecenter_building"
      |> to_list |> parse_list parse_color |> Array.of_list));
  draw_image (extract !pokecenter) coord

let draw_beachgym coord = if !beachgym = None then
  beachgym := Some
    (make_image
      ("data/rooms.json" |> Yojson.Basic.from_file
      |> member "beachgym_building"
      |> to_list |> parse_list parse_color |> Array.of_list));
  draw_image (extract !beachgym) coord

let draw_cavegym coord = if !cavegym = None then
  cavegym := Some
    (make_image
      ("data/rooms.json" |> Yojson.Basic.from_file
     |> member "cavegym_building" |> to_list |> parse_list parse_color
     |> Array.of_list));
  draw_image (extract !cavegym) coord

let draw_towngym coord = if !towngym = None then
  towngym := Some
    (make_image
      ("data/rooms.json" |> Yojson.Basic.from_file
     |> member "towngym_building" |> to_list |> parse_list parse_color
     |> Array.of_list));
  draw_image (extract !towngym) coord

let ipokecenter = ref None

let ibeachgym = ref None

let icavegym = ref None

let itowngym = ref None

let draw_ipokecenter () =
  if !ipokecenter = None then
    ipokecenter :=
      Some
        (make_image
           ("data/rooms.json" |> Yojson.Basic.from_file
          |> member "ipokecenter" |> to_list |> parse_list parse_color
          |> Array.of_list));
  draw_image (extract !ipokecenter) (0, 0)

let draw_ibeachgym () =
  if !ibeachgym = None then
    ibeachgym :=
      Some
        (make_image
           ("data/rooms.json" |> Yojson.Basic.from_file
          |> member "ibeachgym" |> to_list |> parse_list parse_color
          |> Array.of_list));
  draw_image (extract !ibeachgym) (0, 0)

let draw_icavegym () =
  if !icavegym = None then
    icavegym :=
      Some
        (make_image
           ("data/rooms.json" |> Yojson.Basic.from_file |> member "icavegym"
          |> to_list |> parse_list parse_color |> Array.of_list));
  draw_image (extract !icavegym) (0, 0)

let draw_itowngym () =
  if !itowngym = None then
    itowngym :=
      Some
        (make_image
           ("data/rooms.json" |> Yojson.Basic.from_file |> member "itowngym"
          |> to_list |> parse_list parse_color |> Array.of_list));
  draw_image (extract !itowngym) (0, 0)

let helper_draw_room st= let room_array = room_layout (current_room st) in
for row = 0 to 20 do
  for col = 0 to 20 do
    let fill = get_color room_array.(row).(col) in
    draw_square (col * 25, 500 - (25 * row)) fill black
  done
done;
for row = 0 to 10 do
  for col = 0 to 10 do
    if string_of_tile room_array.(row).(col) = "Pokecenter" then
      draw_pokecenter (col * 25, 500 - (25 * row));
    if string_of_tile room_array.(row).(col) = "Beachgym" then
      draw_beachgym (col * 25, 500 - (25 * row));
    if string_of_tile room_array.(row).(col) = "Cavegym" then
      draw_cavegym (col * 25, 500 - (25 * row));
    if string_of_tile room_array.(row).(col) = "Towngym" then
      draw_towngym (col * 25, 500 - (25 * row))
  done
done

(** [draw_room arr] draws the tiles of the room array [arr] to the current
    graphics screen *)
let draw_room st =
  helper_draw_room st;
  if current_room st = "pokecenter" then draw_ipokecenter ()
  else if current_room st = "beachgym" then draw_ibeachgym ()
  else if current_room st = "cavegym" then draw_icavegym ()
  else if current_room st = "towngym" then draw_itowngym ()

let encounter st =
  let room = current_room st in
  let tile = get_tile (current_coord st) room in
  if generate_encounter () then
    match generate_pokemon tile with
    | Some poke ->
        let action =
          init_battle (current_trainer st) [ poke ] (get_color tile) false
        in
        update_action st (Battle action)
    | None -> st
  else st

(** [test_print_poke n] prints a random Pokemon encounter depending on state
    [n] if it occurs. Using for testing purposes before implementing battle
    engine *)
(* let encounter st = let room = current_room st in let tile = get_tile
   (current_coord st) room in let gen_poke_test = if generate_encounter ()
   then match generate_pokemon tile with | Some poke -> print_endline
   poke.name | None -> () in match tile with | Path _ -> () | _ ->
   gen_poke_test *)

(** [move st (x, y) (u, v) sp f] moves the character from its initial
    position of [(x, v)] to [(u, v)], according to the current state [st].
    Note that these coordinates correspond to the bottom-left corner of the
    grid square that the character is on. We use auto_synchronize to batch
    the operations of filling in the old tile with [f] and drawing the
    sprite [sp] on the new tile. *)
let move st (x0, y0) (x1, y1) sp fill =
  let room = current_room st in
  let next_tile =
    try string_of_tile (get_tile (x1, y1) room) with
    | Invalid_argument _ -> "Useless"
  in
  if
    is_exit (x1, y1) (room_of_string room).exits
    || (x1 >= 0 && y1 >= 0 && x1 < 500 && y1 < 500)
       && next_tile <> "Unwal" && next_tile <> "Pokecenter"
       && next_tile <> "Beachgym" && next_tile <> "Cavegym"
       && next_tile <> "Towngym" && next_tile <> "NPC"
  then (
    let trainer = current_trainer st in
    let room' = enter_room room (x1, y1) in
    let x1, y1 =
      if room' = room then (x1, y1)
      else exit_coord (x1, y1) (room_of_string room).exits
    in
    let st' = update_state room' (x1, y1) Walk trainer in
    let is_new_room = room' <> room in
    (* Need to do save this bool because I cant figure out to only have
       conditional and make the drawing order correct. If someone figures
       out a better way feel free to change -CJ *)
    moveto (x1, y1);
    auto_synchronize false;
    if is_new_room then (draw_room st'; flush_kp ())
    else if
      room = "pokecenter" || room = "beachgym" || room = "cavegym"
      || room = "towngym"
    then draw_room st'
    else if not is_new_room then draw_square (x0, y0) fill black;
    draw_image sp (x1, y1);
    auto_synchronize true;
    (* match encounter st' with | Some p -> print_endline p.name; st' | None
       -> st' *)
    if not is_new_room then encounter st' else st')
  else st

let switch_to_room st sp =
  draw_room st;
  draw_image sp (current_coord st)
  
let battle st sp key =
  let b = current_battle st in
  let data = update_battle_menu st (current_trainer st) b key in
  match data.menu with
  | Flee ->
      switch_to_room st sp;
      let st' =  update_trainer st {(current_trainer st) with team = data.friend_team; bag = data.items} in
      update_action st' Walk
  | menu -> update_action st (Battle data)
    
let rec talk_or_no st (x, y) room trainer sp =
  try
    let curr_tile = get_tile (x, y + 25) room in
    if string_of_tile curr_tile = "NPC" then let st' = (talk st room trainer sp) in st'
    else st
  with
  | Failure f -> if f = "No" then st else failwith f
and talk st room trainer_name sp =
let trainer_name = String.uppercase_ascii trainer_name in
(** will need to return updated state with Battle and Battle data*)
match room with
| "pokecenter" -> let st' = nurse_joy_interaction st in (draw_room st'; draw_image sp (current_coord st)); st'
| "beachgym" -> kimmy_battle st room trainer_name
| "cavegym" -> cj_battle st room trainer_name
| "towngym" -> sabrina_battle st room trainer_name
| _ -> failwith "No NPCs here"

(*[menu st] opens up the menu options including bag and team *)

(** [play st sp] is the main game loop for running OCamlMon, where [st]
    represents the current state that the player is in and [sp] is the
    player's sprite. *)
let rec play st sp =
  try
    let status = wait_next_event [ Key_pressed ] in
    if status.keypressed then
      (* if status.key = 'q' then raise Exit else *)
      match current_action st with
      | Walk -> begin
          let x, y = current_coord st in
          let tile_color = get_color (get_tile (x, y) (current_room st)) in
          match status.key with
          | 'w' ->
              let next = move st (x, y) (x, y + 25) sp tile_color in
              play next sp
          | 'a' ->
              let next = move st (x, y) (x - 25, y) sp tile_color in
              play next sp
          | 's' ->
              let next = move st (x, y) (x, y - 25) sp tile_color in
              play next sp
          | 'd' ->
              let next = move st (x, y) (x + 25, y) sp tile_color in
              play next sp
          | 'e' ->
              play
                (talk_or_no st (x, y) (current_room st) (current_trainer st).name sp) sp
          | 'm' ->
              print_endline "Opening Menu";
              play st sp
          | _ -> play st sp
        end
      | Battle p -> play (battle st sp status.key) sp
      | Menu _ -> failwith "Unimplemented"
  with
  | Exit -> clear_graph ()
  | Graphic_failure f -> clear_graph ()
(* | _ -> clear_graph () *)
(* | _ -> clear_graph () *)
(* idk why clear_graph works the best *)

(** [init_game n] begins running the main game loop with the [n]
    customization details that the user provided. *)
let init_game name starter =
  open_graph " 500x500";
  set_window_title "OCamlMon";
  let trainer = init_trainer name starter in
  let st = init_state trainer in
  draw_room st;
  let sp =
    make_image
      ("data/player.json" |> Yojson.Basic.from_file |> member "sprite"
     |> to_list |> parse_list parse_color |> Array.of_list)
  in
  let x, y = current_coord st in
  moveto (x, y);
  draw_image sp (x, y);
  play st sp

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  let name = name () in
  let starter = starter name false in
  confirm name starter;
  init_game name starter

and name () =
  ANSITerminal.print_string [ Bold ]
    " \n\n\n\n\n\
    \     ██████╗  ██████╗ █████╗ ███╗   ███╗██╗     ███╗   ███╗ ██████╗ \
     ███╗   ██╗\n\
    \    ██╔═══██╗██╔════╝██╔══██╗████╗ ████║██║     ████╗ \
     ████║██╔═══██╗████╗  ██║\n\
    \    ██║   ██║██║     ███████║██╔████╔██║██║     ██╔████╔██║██║   \
     ██║██╔██╗ ██║\n\
    \    ██║   ██║██║     ██╔══██║██║╚██╔╝██║██║     ██║╚██╔╝██║██║   \
     ██║██║╚██╗██║\n\
    \    ╚██████╔╝╚██████╗██║  ██║██║ ╚═╝ ██║███████╗██║ ╚═╝ \
     ██║╚██████╔╝██║ ╚████║\n\
    \     ╚═════╝  ╚═════╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚═╝     ╚═╝ ╚═════╝ \
     ╚═╝  ╚═══╝\n\
    \    ";
  print_bold "\nWelcome to the world of ";
  print_emph ANSITerminal.Red "OCamlMon! \n \n";
  print_string "What's your ";
  print_emph ANSITerminal.Yellow "name? \n";
  print_string "> ";
  match read_line () with
  | name -> name
  | exception End_of_file -> name ()

and starter name repeated =
  if not repeated then (
    print_newline ();
    print_emph ANSITerminal.Green name;
    print_endline "! Nice to meet you!";
    print_endline
      "Now, you have a choice. Which starter Pokemon would you like?\n")
  else (
    print_newline ();
    print_string "Sorry, ";
    print_emph ANSITerminal.Green name;
    print_string ". I didn't understand that. \n";
    print_endline "Do you mind saying which Pokemon you would like again?\n");
  print_emph ANSITerminal.Blue "Squirtle     ";
  print_emph ANSITerminal.Green "Bulbasaur    ";
  print_emph ANSITerminal.Red "Charmander    ";

  print_emph ANSITerminal.Yellow "Pikachu     \n";
  print_string "> ";
  match read_line () with
  | s -> begin
      try make_starter_pokemon (String.lowercase_ascii s) with
      | Failure f -> starter name true
    end
  | exception End_of_file -> starter name true

and confirm name starter =
  print_newline ();
  print_emph ANSITerminal.Blue (string_of_species starter.species);
  print_endline " – that's a great choice. \n";
  print_string "Well, ";
  print_emph ANSITerminal.Green name;
  print_endline ", I hope you're ready to go an adventure!\n";
  print_endline "Enter any character to enter the world of OCamlMon!";
  print_string "> ";
  match read_line () with
  | _ -> ()

(* Execute the game engine. *)
let () = main ()