let vals_for line = line |> String.split_on_char ' ' |> List.map int_of_string

let rec differences_for vals =
  match vals with
  | []
  | [ _ ] ->
    []
  | x :: y :: xs -> (y - x) :: differences_for (y :: xs)
;;

let rec all_same vals =
  match vals with
  | []
  | [ _ ] ->
    true
  | x :: y :: xs -> x = y && all_same (y :: xs)
;;

let solve f lines =
  lines |> List.map vals_for |> List.map f |> List.fold_left ( + ) 0
;;

let a lines =
  let rec next_val_for vals =
    match vals with
    | []
    | [ _ ] ->
      failwith "invalid input"
    | vs when all_same vs -> List.hd vs
    | vs -> (vs |> List.rev |> List.hd) + (vs |> differences_for |> next_val_for)
  in
  lines |> solve next_val_for
;;

let b lines =
  let rec prev_val_for vals =
    match vals with
    | []
    | [ _ ] ->
      failwith "invalid input"
    | vs when all_same vs -> List.hd vs
    | vs -> (vs |> List.hd) - (vs |> differences_for |> prev_val_for)
  in
  lines |> solve prev_val_for
;;
