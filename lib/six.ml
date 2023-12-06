open String
open List
open Util

let solve (times, distances) =
  let rec product winners =
    match winners with
    | [] -> 1
    | x :: xs -> x * product xs
  in
  let rec wins_for (time, distance) hold acc =
    match hold with
    | 0 -> acc
    | ms ->
      (match (time - ms) * ms with
       | x when x <= distance -> wins_for (time, distance) (ms - 1) acc
       | _ -> wins_for (time, distance) (ms - 1) (acc + 1))
  in
  let rec paired (times, distances) =
    match times, distances with
    | [], [] -> []
    | [ t ], [ d ] -> [ t, d ]
    | t :: ts, d :: ds -> (t, d) :: paired (ts, ds)
    | _, []
    | [], _ ->
      failwith "invalid input"
  in
  paired (times, distances)
  |> map (fun (t, d) -> wins_for (t, d) t 0)
  |> product
;;

let a lines =
  let v_for str =
    match split_on_char ' ' str with
    | []
    | [ _ ] ->
      failwith "invalid input"
    | _ :: rest -> rest |> map int_of_string_opt |> filter_map ident
  in
  (match lines with
   | [ times; distances ] -> v_for times, v_for distances
   | _ -> failwith "invalid input")
  |> solve
;;

let b lines =
  let v_for str =
    match split_on_char ' ' str with
    | []
    | [ _ ] ->
      failwith "invalid input"
    | _ :: rest -> [ rest |> fold_left ( ^ ) "" |> int_of_string ]
  in
  (match lines with
   | [ times; distances ] -> v_for times, v_for distances
   | _ -> failwith "invalid input")
  |> solve
;;
