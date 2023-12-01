open AOC_2023

let is_digit c = c >= '0' && c <= '9'

let to_first_and_last str =
  match String.length str with
  | 0 -> None
  | 1 ->
      Some
        (String.make 1 (String.get str 0) ^ String.make 1 (String.get str 0))
  | n ->
      Some
        ( String.make 1 (String.get str 0)
        ^ String.make 1 (String.get str (n - 1)) )

let to_digits str =
  let chars = String.to_seq str in
  let filtered = Seq.filter (fun c -> is_digit c) chars in
  let filtered_str = List.map (String.make 1) (List.of_seq filtered) in
  String.concat "" filtered_str

let one () =
  let lines = FileIO.read_lines "./inputs/01.txt" in
  let digits = List.map to_digits lines in
  let codes = List.map to_first_and_last digits in
  let code_ints =
    List.map
      (fun code ->
        match code with None -> 0 | Some str -> int_of_string str )
      codes
  in
  List.fold_left ( + ) 0 code_ints

let () = print_endline (string_of_int (one ()))
