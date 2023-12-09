open String
open List

let rec differences_for vals =
  match vals with
  | []
  | [ _ ] ->
    []
  | x :: y :: xs -> (y - x) :: differences_for (y :: xs)
;;

let rec is_linear vals =
  match vals with
  | []
  | [ _ ] ->
    true
  | x :: y :: xs -> x = y && is_linear (y :: xs)
;;

let solve f lines =
  let vals_for line = line |> split_on_char ' ' |> map int_of_string in
  lines |> map vals_for |> map f |> fold_left ( + ) 0
;;

let a lines =
  let rec next_for vals =
    match vals with
    | []
    | [ _ ] ->
      failwith "invalid input"
    | vs when is_linear vs -> hd vs
    | vs -> (vs |> rev |> hd) + (vs |> differences_for |> next_for)
  in
  lines |> solve next_for
;;

let b lines =
  let rec prev_for vals =
    match vals with
    | []
    | [ _ ] ->
      failwith "invalid input"
    | vs when is_linear vs -> hd vs
    | vs -> (vs |> hd) - (vs |> differences_for |> prev_for)
  in
  lines |> solve prev_for
;;
