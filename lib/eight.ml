let parse_lines lines =
  let graph = Hashtbl.create 100 in
  let left_right_for dir_str =
    match
      String.sub dir_str 1 (String.length dir_str - 2)
      |> String.split_on_char ','
      |> List.map String.trim
    with
    | [ left; right ] -> left, right
    | _ -> failwith "invalid direction"
  in
  let node_for line =
    match String.split_on_char '=' line |> List.map String.trim with
    | [ id; rest ] -> id, left_right_for rest
    | _ -> failwith "invalid line"
  in
  match lines with
  | inst :: _ :: rest ->
    let nodes = List.map node_for rest in
    let instructions = inst |> String.to_seq |> List.of_seq in
    List.iter (fun (id, dirs) -> Hashtbl.add graph id dirs) nodes;
    instructions, graph
  | _ -> failwith "invalid input"
;;

let rec traverse node step (instructions, graph) =
  match node with
  | "ZZZ" -> step
  | _ ->
    (match Hashtbl.find_opt graph node with
     | None -> failwith "invalid node"
     | Some (left, right) ->
       (match List.nth instructions (step mod List.length instructions) with
        | 'L' -> traverse left (step + 1) (instructions, graph)
        | 'R' -> traverse right (step + 1) (instructions, graph)
        | _ -> failwith "invalid instruction"))
;;

let ends_with c s =
  match s |> String.to_seq |> List.of_seq |> List.rev with
  | x :: _ when x = c -> true
  | _ -> false
;;

let is_start = Util.memoize ends_with 'A'
let is_end = Util.memoize ends_with 'Z'

let starts_for (graph : (string, string * string) Hashtbl.t) =
  Hashtbl.to_seq_keys graph |> List.of_seq |> List.filter is_start
;;

let rec traverse_b step (instructions, graph) node =
  match node with
  | n when is_end n -> step
  | _ ->
    (match Hashtbl.find_opt graph node with
     | None -> failwith "invalid node"
     | Some (left, right) ->
       (match List.nth instructions (step mod List.length instructions) with
        | 'L' -> traverse_b (step + 1) (instructions, graph) left
        | 'R' -> traverse_b (step + 1) (instructions, graph) right
        | _ -> failwith "invalid instruction"))
;;

let a lines = lines |> parse_lines |> traverse "AAA" 0

let b lines =
  let lcm_of l =
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    let lcm a b = if a = 0 || b = 0 then 0 else abs (a * b) / gcd a b in
    match l with
    | [] -> failwith "Empty list"
    | hd :: tl -> List.fold_left lcm hd tl
  in
  let instructions, graph = parse_lines lines in
  starts_for graph |> List.map (traverse_b 0 (instructions, graph)) |> lcm_of
;;
