type game =
  { idx : int
  ; max_r : int
  ; max_g : int
  ; max_b : int
  }

type round =
  { r : int
  ; g : int
  ; b : int
  }

let get_game_idx l_line =
  match String.split_on_char ' ' l_line with
  | [ _; idx ] -> int_of_string idx
  | _ -> failwith "Invalid game line"
;;

let handle_token acc token =
  let trimmed = String.trim token in
  match String.split_on_char ' ' trimmed with
  | [ r; "red" ] -> { acc with r = int_of_string r }
  | [ g; "green" ] -> { acc with g = int_of_string g }
  | [ b; "blue" ] -> { acc with b = int_of_string b }
  | _ -> acc
;;

let get_round r_str =
  let initial_round = { r = 0; g = 0; b = 0 } in
  String.split_on_char ',' r_str |> List.fold_left handle_token initial_round
;;

let get_rounds r_line =
  let rounds = String.split_on_char ';' r_line in
  let trimmed = List.map String.trim rounds in
  List.map get_round trimmed
;;

let print_round round =
  Printf.printf "R: %d, G: %d, B: %d\n" round.r round.g round.b
;;

let get_game idx rounds =
  let max_r = List.fold_left (fun acc round -> max acc round.r) 0 rounds in
  let max_g = List.fold_left (fun acc round -> max acc round.g) 0 rounds in
  let max_b = List.fold_left (fun acc round -> max acc round.b) 0 rounds in
  { idx; max_r; max_g; max_b }
;;

let handle_line line =
  match String.split_on_char ':' line with
  | [ l_line; r_line ] ->
    let idx = get_game_idx l_line in
    let rounds = get_rounds r_line in
    get_game idx rounds
  | _ -> failwith "Invalid line"
;;

let is_possible max_r max_g max_b game =
  game.max_r <= max_r && game.max_g <= max_g && game.max_b <= max_b
;;

let a lines =
  let games = List.map handle_line lines in
  let pos = is_possible 12 13 14 in
  let possible_games = List.filter pos games in
  List.fold_left (fun acc game -> acc + game.idx) 0 possible_games
;;

let game_power game = game.max_r * game.max_g * game.max_b

let b lines =
  let games = List.map handle_line lines in
  let game_powers = List.map game_power games in
  List.fold_left (fun acc power -> acc + power) 0 game_powers
;;
