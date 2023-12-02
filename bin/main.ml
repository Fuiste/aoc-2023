open AOC_2023

let print_ans prefix ans = print_endline (prefix ^ ans)

let solve_one () =
  let lines = FileIO.read_lines "./inputs/01.txt" in
  One.a lines |> string_of_int |> print_ans "1a: ";
  One.b lines |> string_of_int |> print_ans "1b: ";
  print_endline ""
;;

let solve_two () =
  let lines = FileIO.read_lines "./inputs/02.txt" in
  Two.a lines |> string_of_int |> print_ans "2a: ";
  Two.b lines |> string_of_int |> print_ans "2b: "
;;

let () =
  solve_one ();
  solve_two ()
;;
