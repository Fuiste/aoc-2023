let is_digit c = c >= '0' && c <= '9'

let to_first_and_last str =
  match String.length str with
  | 0 -> None
  | n ->
      Some
        ( String.make 1 (String.get str 0)
        ^ String.make 1 (String.get str (n - 1)) )

let to_digits str =
  String.concat ""
    (List.map (String.make 1)
       (List.of_seq (Seq.filter (fun c -> is_digit c) (String.to_seq str))) )

let a lines =
  let code_ints =
    List.map
      (fun code ->
        match code with None -> 0 | Some str -> int_of_string str )
      (List.map to_first_and_last (List.map to_digits lines))
  in
  List.fold_left ( + ) 0 code_ints

let replace_spelled_digits str =
  let digit_map =
    [ ("one", "o1e")
    ; ("two", "t2o")
    ; ("three", "t3e")
    ; ("four", "f4r")
    ; ("five", "f5e")
    ; ("six", "s6x")
    ; ("seven", "s7n")
    ; ("eight", "e8t")
    ; ("nine", "n9e") ]
  in
  let replace_word str (word, digit) =
    Str.global_replace (Str.regexp_string word) digit str
  in
  let replace_digit_words str =
    digit_map |> List.fold_left replace_word str
  in
  replace_digit_words str

let b lines =
  let mapped_lines = List.map replace_spelled_digits lines in
  a mapped_lines
