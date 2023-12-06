open Util
open String
open List

let _numbers_for n_str =
  map int_of_string_opt (split_on_char ' ' n_str) |> filter_map (fun x -> x)
;;

let numbers_for = memoize _numbers_for

let rec _winners_for (n, winning, yours) =
  match winning, yours with
  | [], _ -> []
  | _, [] -> []
  | ws, y :: ys ->
    (match find_opt (( = ) y) ws with
     | Some _ -> y :: _winners_for (n, ws, ys)
     | None -> _winners_for (n, ws, ys))
;;

let winners_for = memoize _winners_for

let _card_num_for str =
  match split_on_char ' ' str with
  | [ _; _; _; n ] -> int_of_string n
  | [ _; _; n ] -> int_of_string n
  | [ _; n ] -> int_of_string n
  | _ -> failwith "Invalid card"
;;

let card_num_for = memoize _card_num_for

let _parse_l line =
  let n, content =
    match split_on_char ':' line with
    | [ card; content ] -> card_num_for card, content
    | _ -> failwith "Invalid line"
  in
  let winning, yours =
    match split_on_char '|' content with
    | [ winning; yours ] -> winning, yours
    | _ -> failwith "Invalid Content"
  in
  n, numbers_for winning, numbers_for yours
;;

let parse_l = memoize _parse_l

let value_of winners =
  fold_left
    (fun acc _ ->
      match acc with
      | 0 -> 1
      | _ -> acc * 2)
    0
    winners
;;

let a lines =
  map parse_l lines |> map winners_for |> map value_of |> fold_left ( + ) 0
;;

let rec _range_for (orig_lines, winners, n) =
  match winners with
  | [] -> []
  | _ :: rest -> nth orig_lines n :: _range_for (orig_lines, rest, n + 1)
;;

let range_for = memoize _range_for

let rec traverse_lines orig_lines idx lines =
  match lines with
  | [] -> idx
  | line :: rest ->
    let n, winning, yours = parse_l line in
    range_for (orig_lines, winners_for (n, winning, yours), n) @ rest
    |> traverse_lines orig_lines (idx + 1)
;;

let b lines = traverse_lines (clone lines) 0 lines
