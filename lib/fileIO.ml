let read_lines file =
  let input_channel = open_in file in
  let rec read_lines_aux acc =
    try
      let line = input_line input_channel in
      read_lines_aux (line :: acc)
    with End_of_file -> acc
  in
  read_lines_aux [] |> List.rev
