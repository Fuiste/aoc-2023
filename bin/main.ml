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
  (* This one is real slow, leave it commented out unless you want to wait a
     while *)
  (* Four.b lines |> string_of_int |> print_ans "4b: "; *)
  print_endline ""
;;

let solve_five () =
  let lines = FileIO.read_lines "./inputs/05.txt" in
  Five.a lines |> string_of_int |> print_ans "5a: ";
  Five.b lines |> string_of_int |> print_ans "5b: ";
  print_endline ""
;;

let solve_six () =
  let lines = FileIO.read_lines "./inputs/06.txt" in
  Six.a lines |> string_of_int |> print_ans "6a: ";
  Six.b lines |> string_of_int |> print_ans "6b: ";
  print_endline ""
;;

let solve_seven () =
  let lines = FileIO.read_lines "./inputs/07.txt" in
  Seven.a lines |> string_of_int |> print_ans "7a: ";
  Seven.b lines |> string_of_int |> print_ans "7b: ";
  print_endline ""
;;

let solve_eight () =
  let lines = FileIO.read_lines "./inputs/08.txt" in
  Eight.a lines |> string_of_int |> print_ans "8a: ";
  Eight.b lines |> string_of_int |> print_ans "8b: ";
  print_endline ""
;;

let solve_nine () =
  let lines = FileIO.read_lines "./inputs/09.txt" in
  Nine.a lines |> string_of_int |> print_ans "9a: ";
  Nine.b lines |> string_of_int |> print_ans "9b: ";
  print_endline ""
;;

let () =
  solve_one ();
  solve_two ();
  solve_three ();
  solve_four ();
  solve_five ();
  solve_six ();
  solve_seven ();
  solve_eight ();
  solve_nine ()
;;
