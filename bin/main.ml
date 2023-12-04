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
  Two.b lines |> string_of_int |> print_ans "2b: ";
  print_endline ""
;;

let solve_three () =
  let lines = FileIO.read_lines "./inputs/03.txt" in
  Three.a lines |> string_of_int |> print_ans "3a: ";
  Three.b lines |> string_of_int |> print_ans "3b: ";
  print_endline ""
;;

let solve_four () =
  let lines = FileIO.read_lines "./inputs/04.txt" in
  Four.a lines |> string_of_int |> print_ans "4a: ";
  Four.b lines |> string_of_int |> print_ans "4b: ";
  print_endline ""
;;

let () =
  solve_one ();
  solve_two ();
  solve_three ();
  solve_four ()
;;
