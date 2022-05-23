open Pokemon
open Graphics
open Drawing
open State
open Battle
open Yojson.Basic.Util
open Parser

type gym_leader = string * pokemon list

let battle_transition () =
  set_color black;
  for row = 0 to 99 do
    fill_rect (0, row * 5) 500 5;
    Unix.sleepf 0.005
  done;
  set_color white;
  for col = 0 to 99 do
    fill_rect (col * 5, 100) 5 300;
    Unix.sleepf 0.01
  done

let kimmy_squirtle =
  {
    name = "Squirtle";
    species = Squirtle;
    element = Water;
    moves = squirtle_moves;
    stats = { maxhealth = 60; health = 60; speed = 90; level = 8; xp = 0 };
  }

let kimmy_sandshrew =
  {
    name = "Sandshrew";
    species = Sandshrew;
    element = Ground;
    moves = squirtle_moves;
    stats = { maxhealth = 80; health = 80; speed = 60; level = 10; xp = 0 };
  }

let kimmy = ("KIMMY", [ kimmy_squirtle; kimmy_sandshrew ])

let sabrina_eevee =
  {
    name = "Eevee";
    species = Eevee;
    element = Normal;
    moves = eevee_moves;
    stats = { maxhealth = 40; health = 40; speed = 70; level = 6; xp = 0 };
  }

let sabrina_bulbasaur =
  {
    name = "Bulbasaur";
    species = Bulbasaur;
    element = Grass;
    moves = bulb_moves;
    stats = { maxhealth = 50; health = 50; speed = 60; level = 7; xp = 0 };
  }

let sabrina = ("SABRINA", [ sabrina_eevee; sabrina_bulbasaur ])

let cj_geodude =
  {
    name = "Geodude";
    species = Geodude;
    element = Rock;
    moves = geodude_moves;
    stats = { maxhealth = 50; health = 50; speed = 70; level = 7; xp = 0 };
  }

let cj = ("CJ", [ cj_geodude ])

let rec wait_keypress cont =
  if cont = true then ()
  else
    let status = wait_next_event [ Key_pressed ] in
    match status.key with
    | 'e' -> wait_keypress true
    | _ -> wait_keypress false

let clear_dialogue () =
  fill_draw_rect (66, 16) 368 84 blue black;
  fill_draw_rect (70, 20) 360 75 white black

let kimmy_battle st room trainer_name =
  clear_dialogue ();
  set_text_size 40;
  draw_text (80, 70) "KIMMY: tiktok tiktok";
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) (trainer_name ^ ": . . .");
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70)
    "KIMMY: A hourglass that doesn't have any sand just causes";
  draw_text (80, 50) " everyone to waste their time.";
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) "KIMMY: So don't waste my time.";
  wait_keypress false;
  battle_transition ();
  clear_dialogue ();
  let k_img =
    make_image
      ("data/npc.json" |> Yojson.Basic.from_file |> member "kimmy"
     |> to_list |> parse_list parse_color |> Array.of_list)
  in
  draw_image k_img (150, 150);
  draw_text (80, 70) "Gym leader KIMMY challenged you to a battle!!";
  Unix.sleep 2;
  flush_kp ();
  let battle_data =
    init_battle (current_trainer st) (snd kimmy) 0xf1c40f true
  in
  update_state room (current_coord st) (Battle battle_data)
    (current_trainer st)

let sabrina_battle st room trainer_name =
  clear_dialogue ();
  set_text_size 40;
  draw_text (80, 70) "SABRINA: hehe";
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) (trainer_name ^ ": ?!");
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70)
    "SABRINA: Some times you just gotta pick and choose those";
  draw_text (80, 50) "you keep around you...";
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) "SABRINA: That's what makes life an exciting game of";
  draw_text (80, 50) "russian roulette hehehehe";
  wait_keypress false;
  battle_transition ();
  clear_dialogue ();
  let k_img =
    make_image
      ("data/npc.json" |> Yojson.Basic.from_file |> member "sabrina"
     |> to_list |> parse_list parse_color |> Array.of_list)
  in
  draw_image k_img (150, 150);
  draw_text (80, 70) "Gym leader SABRINA challenged you to a battle!!";
  Unix.sleep 2;
  flush_kp ();
  let battle_data =
    init_battle (current_trainer st) (snd sabrina) 0x6f83a3 true
  in
  update_state room (current_coord st) (Battle battle_data)
    (current_trainer st)

let cj_battle st room trainer_name =
  clear_dialogue ();
  set_text_size 40;
  draw_text (80, 70) "CJ: Zzzzz";
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) (trainer_name ^ ": Uh... hello there?");
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) "CJ: *sniff*";
  wait_keypress false;
  draw_text (80, 50) "ZzZZzzzz";
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) (trainer_name ^ ": Helloooooo ??!!");
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) "CJ: ZzzzzZzzzZzzzzzzz";
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) (trainer_name ^ ": HELLLLLOOOOOO!!!!!!!");
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) "CJ: AAAAAAA";
  wait_keypress false;
  draw_text (80, 50) "AAAAAAA";
  wait_keypress false;
  draw_text (80, 30) "AAAAAAACHHOOOOOOO";
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) "CJ: Gosh all I wanted was some peace and quiet.";
  draw_text (80, 50) "Ok fine, let's do this.";
  wait_keypress false;
  battle_transition ();
  clear_dialogue ();
  let k_img =
    make_image
      ("data/npc.json" |> Yojson.Basic.from_file |> member "cj" |> to_list
     |> parse_list parse_color |> Array.of_list)
  in
  draw_image k_img (150, 150);
  draw_text (80, 70) "Fighter CJ challenged you to a battle!!";
  Unix.sleep 2;
  flush_kp ();
  let battle_data =
    init_battle (current_trainer st) (snd cj) 0x808080 true
  in
  update_state room (current_coord st) (Battle battle_data)
    (current_trainer st)

(** [draw_wait ()] draws the ". . . " while pokemon are healing *)
let draw_wait () =
  clear_dialogue ();
  draw_text (80, 70) ".";
  Unix.sleep 1;
  draw_text (90, 70) ".";
  Unix.sleep 1;
  draw_text (100, 70) ".";
  Unix.sleep 1

let nurse_joy_interaction st =
  let trainer_name = String.uppercase_ascii (current_trainer st).name in
  clear_dialogue ();
  set_text_size 40;
  draw_text (80, 70)
    ("JOY: Hi " ^ trainer_name ^ "! I really hope you are taking care of");
  draw_text (80, 50) "your OCamlMon!";
  wait_keypress false;
  clear_dialogue ();
  draw_text (80, 70) "Give me one second as your pokemon are resting up!";
  wait_keypress false;
  draw_wait ();
  flush_kp ();
  clear_dialogue ();
  draw_text (80, 70) "OK! All done. Best of luck with your adventure!!";
  wait_keypress false;
  update_state (current_room st) (current_coord st) Walk
    (current_trainer st)
