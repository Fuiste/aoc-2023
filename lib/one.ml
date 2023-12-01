let is_digit c = c >= '0' && c <= '9'

let from_char = String.make 1

let to_first_and_last str =
  match String.length str with
  | 0 -> None
  | n ->
      Some (from_char (String.get str 0) ^ from_char (String.get str (n - 1)))

let to_digits str =
  String.concat ""
    (List.map from_char
       (List.of_seq (Seq.filter (fun c -> is_digit c) (String.to_seq str))) )

let code_val code =
  match code with None -> 0 | Some str -> int_of_string str

let a lines =
  let code_ints =
    List.map code_val (List.map to_digits lines |> List.map to_first_and_last)
  in
  List.fold_left ( + ) 0 code_ints

let replace_spelled_digits str =
  (* This is a silly hack to account for string digits that begin or end with
     characters used in another string digit. While there is probably a more
     elegant solution out there, it works lol *)
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
