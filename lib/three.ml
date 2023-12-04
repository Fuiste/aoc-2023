type code =
  { v : int
  ; sz : int
  ; x : int
  ; y : int
  }

type symbol =
  { x : int
  ; y : int
  }

module CodeSet = Set.Make (struct
    type t = code

    let compare (a : code) (b : code) =
      let cmp = compare a.x b.x in
      if cmp <> 0 then cmp else compare a.y b.y
    ;;
  end)

let count_digits n = string_of_int n |> String.length

let rec traverse_line line i_x i_y codes symbols code_val =
  let traverse_flush l x y c s d =
    match code_val with
    | 0 -> traverse_line l (x + 1) y c s d
    | _ ->
      traverse_line
        l
        (x + 1)
        y
        ({ v = d; sz = count_digits d; x = x - 1; y } :: c)
        s
        0
  in
  match line with
  | [] ->
    if code_val > 0
    then
      ( { v = code_val; sz = count_digits code_val; x = i_x - 1; y = i_y }
        :: codes
      , symbols )
    else codes, symbols
  | x :: xs ->
    if One.is_digit x
    then
      traverse_line
        xs
        (i_x + 1)
        i_y
        codes
        symbols
        ((10 * code_val) + (-48 + int_of_char x))
    else (
      match x with
      | '.' -> traverse_flush xs i_x i_y codes symbols code_val
      | _ ->
        traverse_flush
          xs
          i_x
          i_y
          codes
          ({ x = i_x; y = i_y } :: symbols)
          code_val)
;;

let rec traverse_lines lines i_y codes_acc symbols_acc =
  match lines with
  | [] -> codes_acc, symbols_acc
  | x :: xs ->
    let codes, symbols =
      traverse_line (String.to_seq x |> List.of_seq) 0 i_y [] [] 0
    in
    traverse_lines xs (i_y + 1) (codes @ codes_acc) (symbols @ symbols_acc)
;;

let is_adjacent { x; y } (c : code) =
  let is_between a b x = x >= a && x <= b in
  let min_x = c.x - c.sz in
  let max_x = c.x + 1 in
  let min_y = c.y - 1 in
  let max_y = c.y + 1 in
  x |> is_between min_x max_x && y |> is_between min_y max_y
;;

let rec fold_symbols codes_acc (codes : code list) symbols =
  match symbols with
  | [] -> codes_acc
  | x :: xs ->
    let adj_codes = List.filter (is_adjacent x) codes in
    let codes_acc =
      List.fold_left (fun acc c -> CodeSet.add c acc) codes_acc adj_codes
    in
    fold_symbols codes_acc codes xs
;;

let rec fold_symbols_ratio ratios_sum (codes : code list) symbols =
  match symbols with
  | [] -> ratios_sum
  | x :: xs ->
    let adj_codes = List.filter (is_adjacent x) codes in
    let ratio =
      match adj_codes with [] -> 0 | [ a; b ] -> a.v * b.v | _ -> 0
    in
    fold_symbols_ratio (ratios_sum + ratio) codes xs
;;

let a lines =
  let codes, symbols = traverse_lines lines 0 [] [] in
  let codes_unique =
    fold_symbols CodeSet.empty codes symbols |> CodeSet.to_list
  in
  codes_unique |> List.fold_left (fun acc { v; _ } -> acc + v) 0
;;

let b lines =
  let codes, symbols = traverse_lines lines 0 [] [] in
  fold_symbols_ratio 0 codes symbols
;;
