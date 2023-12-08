let parse_lines lines =
  let graph = Hashtbl.create 100 in
  let lr_for s =
    match
      String.sub s 1 (String.length s - 2)
      |> String.split_on_char ','
      |> List.map String.trim
    with
    | [ l; r ] -> l, r
    | _ -> failwith "invalid direction"
  in
  match lines with
  | inst :: _ :: rest ->
    let instructions = inst |> String.to_seq |> List.of_seq in
    rest
    |> List.map (fun line ->
      match String.split_on_char '=' line |> List.map String.trim with
      | [ id; rest ] -> id, lr_for rest
      | _ -> failwith "invalid line")
    |> List.iter (fun (id, dirs) -> Hashtbl.add graph id dirs);
    instructions, graph
  | _ -> failwith "invalid input"
;;

let rec traverse node step is_end (instructions, graph) =
  match node with
  | s when is_end s -> step
  | _ ->
    (match Hashtbl.find_opt graph node with
     | None -> failwith "invalid node"
     | Some (left, right) ->
       (match List.nth instructions (step mod List.length instructions) with
        | 'L' -> traverse left (step + 1) is_end (instructions, graph)
        | 'R' -> traverse right (step + 1) is_end (instructions, graph)
        | _ -> failwith "invalid instruction"))
;;

let ends_with c s =
  match s |> String.to_seq |> List.of_seq |> List.rev with
  | x :: _ when x = c -> true
  | _ -> false
;;

let is_end = ends_with 'Z'
let a lines = lines |> parse_lines |> traverse "AAA" 0 (fun s -> s = "ZZZ")

let b lines =
  let lcm_of l =
    let rec gcd a b =
      match b with
      | 0 -> a
      | _ -> gcd b (a mod b)
    in
    let lcm a b =
      match a, b with
      | 0, _
      | _, 0 ->
        0
      | _ -> abs (a * b) / gcd a b
    in
    match l with
    | [] -> failwith "Empty list"
    | x :: rest -> List.fold_left lcm x rest
  in
  let i, g = parse_lines lines in
  g
  |> Hashtbl.to_seq_keys
  |> List.of_seq
  |> List.filter (ends_with 'A')
  |> List.map (fun s -> traverse s 0 is_end (i, g))
  |> lcm_of
;;
