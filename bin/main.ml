open AOC_2023

let () =
  let lines = FileIO.read_lines "./inputs/01.txt" in
  print_endline ("1a: " ^ string_of_int (One.a lines)) ;
  print_endline ("1b: " ^ string_of_int (One.b lines))
