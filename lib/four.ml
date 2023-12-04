open Util

let _numbers_for n_str =
  let opts = List.map int_of_string_opt (String.split_on_char ' ' n_str) in
  List.filter_map (fun x -> x) opts
;;

let numbers_for = memoize _numbers_for

let rec _winners_for (n, winning, yours) =
  match winning, yours with
  | [], _ -> []
  | _, [] -> []
  | ws, y :: ys ->
    (match List.find_opt (( = ) y) ws with
     | Some _ -> y :: _winners_for (n, ws, ys)
     | None -> _winners_for (n, ws, ys))
;;

let winners_for = memoize _winners_for

let card_num_for str =
  let strs = String.split_on_char ' ' str in
  match strs with
  | [ _; _; _; n ] -> int_of_string n
  | [ _; _; n ] -> int_of_string n
  | [ _; n ] -> int_of_string n
  | _ -> failwith "Invalid card"
;;

let _parse_line line =
  let n, content =
    match String.split_on_char ':' line with
    | [ card; content ] -> card_num_for card, content
    | _ -> failwith "Invalid line"
  in
  let winning, yours =
    match String.split_on_char '|' content with
    | [ winning; yours ] -> winning, yours
    | _ -> failwith "Invalid Content"
  in
  n, numbers_for winning, numbers_for yours
;;

let parse_line = memoize _parse_line

let value_of winners =
  List.fold_left
    (fun acc _ -> match acc with 0 -> 1 | _ -> acc * 2)
    0
    winners
;;

let a lines =
  let pairs = List.map parse_line lines in
  let winners = List.map winners_for pairs in
  List.map value_of winners |> List.fold_left ( + ) 0
;;

let rec _range_for ((orig_lines : string list), winners, n) =
  match winners with
  | [] -> []
  | _ :: rest ->
    let prize = List.nth orig_lines n in
    prize :: _range_for (orig_lines, rest, n + 1)
;;

let range_for = memoize _range_for

let rec traverse_lines orig_lines lines idx =
  match lines with
  | [] -> idx
  | line :: rest ->
    let n, winning, yours = parse_line line in
    let winners = winners_for (n, winning, yours) in
    let range = range_for (orig_lines, winners, n) in
    let new_lines = range @ rest in
    traverse_lines orig_lines new_lines (idx + 1)
;;

let b lines = traverse_lines (clone lines) lines 0
