open AOC_2023

let () =
  let lines = FileIO.read_lines "./inputs/01.txt" in
  print_endline (string_of_int (One.b lines))
