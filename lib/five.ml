open String
open List

type line_contents =
  | Empty
  | Seeds of int list
  | Mapper of (int * int * int)

let between a b x = a <= x && x <= b

let rec accumulate_lines lines seeds map_matrix acc =
  let parse_line line =
    match split_on_char ' ' line with
    | "seeds:" :: rest -> Seeds (map int_of_string rest)
    | [ d; s; l ] -> Mapper (int_of_string s, int_of_string d, int_of_string l)
    | _ -> Empty
  in
  match lines with
  | [] ->
    (match acc with
     | [] -> map_matrix, seeds
     | _ -> map_matrix @ [ acc ], seeds)
  | line :: rest ->
    (match parse_line line with
     | Empty ->
       (match acc with
        | [] -> accumulate_lines rest seeds map_matrix []
        | _ -> accumulate_lines rest seeds (map_matrix @ [ acc ]) [])
     | Seeds s -> accumulate_lines rest s map_matrix acc
     | Mapper m -> accumulate_lines rest seeds map_matrix (acc @ [ m ]))
;;

let rec destinations_for (map_matrix, positions) =
  let rec destination_for maps seed =
    match maps with
    | [] -> seed
    | (s, d, l) :: rest ->
      if between s (s + l - 1) seed
      then d + (seed - s)
      else destination_for rest seed
  in
  match map_matrix with
  | [] -> positions
  | maps :: rest ->
    destinations_for (rest, positions |> map (destination_for maps))
;;

let a lines =
  accumulate_lines lines [] [] [] |> destinations_for |> fold_left min max_int
;;

type paired_line_contents =
  | Empty
  | SeedPairs of (int * int) list
  | Mapper of (int * int * int)

let rec accumulate_lines_b lines seed_pairs map_matrix acc =
  match lines with
  | [] ->
    (match acc with
     | [] -> map_matrix, seed_pairs
     | _ -> map_matrix @ [ acc ], seed_pairs)
  | line :: rest ->
    let parse_line line =
      match split_on_char ' ' line with
      | "seeds:" :: rest ->
        let rec pairs_for strs =
          match strs with
          | [] -> []
          | [ _ ] -> failwith "Odd number of seed inputs"
          | a :: b :: rest ->
            (int_of_string a, int_of_string b) :: pairs_for rest
        in
        SeedPairs (pairs_for rest)
      | [ d; s; l ] -> Mapper (int_of_string s, int_of_string d, int_of_string l)
      | _ -> Empty
    in
    (match parse_line line with
     | Empty ->
       (match acc with
        | [] -> accumulate_lines_b rest seed_pairs map_matrix []
        | _ -> accumulate_lines_b rest seed_pairs (map_matrix @ [ acc ]) [])
     | SeedPairs s -> accumulate_lines_b rest s map_matrix acc
     | Mapper m -> accumulate_lines_b rest seed_pairs map_matrix (acc @ [ m ]))
;;

(* let apply_map (a, r) (s, d, l) = let e = s + l - 1 in let p = a + r - 1 in
   let offset = d - s in match a, p with (* When the full range is within the
   target *) | x, y when x >= s && y <= e -> [], [ x + offset, r ] (* When the
   seed range underruns AND intersects the target *) | x, y when x < s && y >= s
   && y <= e -> let to_s = s - x in [ x, to_s ], [ d, r - to_s ] (* When the
   seed range overruns AND intersects the target *) | x, y when x >= s && x <= e
   && y > e -> let to_e = e - x + 1 in [ x + offset, to_e ], [ e + 1, r - to_e ]
   (* When the seed range underruns AND overruns the target *) | x, y when x < s
   && y > e -> let to_s = s - x in [ x, to_s ], [ e + 1, [ l; to_s ] |>
   fold_left ( - ) r; d, l ] (* When the seed range is disjoint the target *) |
   x, y when y < s || x > e -> [ a, r ], [] | _ -> [], [] ;; *)

let b lines =
  let _ = accumulate_lines_b lines [] [] [] in
  69
;;
