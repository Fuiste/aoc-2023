open List

type card =
  | Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
  | Joker

type hand =
  { cards : card list
  ; bid : int
  }

let value_for card =
  match card with
  | Ace -> 14
  | King -> 13
  | Queen -> 12
  | Jack -> 11
  | Ten -> 10
  | Nine -> 9
  | Eight -> 8
  | Seven -> 7
  | Six -> 6
  | Five -> 5
  | Four -> 4
  | Three -> 3
  | Two -> 2
  | Joker -> 1
;;

type hand_state =
  | FiveKind
  | FourKind
  | FullHouse
  | ThreeKind
  | TwoPair
  | OnePair
  | HighCard

let value_for_state state =
  match state with
  | FiveKind -> 7
  | FourKind -> 6
  | FullHouse -> 5
  | ThreeKind -> 4
  | TwoPair -> 3
  | OnePair -> 2
  | HighCard -> 1
;;

let state_for hand =
  let rec group hand groups =
    match hand.cards with
    | [] -> groups
    | ca :: rest ->
      if find_opt (fun (c, _) -> c = ca) groups = None
      then group { hand with cards = rest } ((ca, 1) :: groups)
      else
        fold_left
          (fun acc (c, i) ->
            match c, ca with
            | c, ca when c = ca -> (c, i + 1) :: acc
            | _ -> (c, i) :: acc)
          []
          groups
        |> group { hand with cards = rest }
  in
  let has t l = find_opt (fun n -> n = t) l != None in
  match group hand [] |> map (fun (_, n) -> n) with
  | l when l |> has 5 -> FiveKind
  | l when l |> has 4 -> FourKind
  | l when l |> has 3 && l |> has 2 -> FullHouse
  | l when l |> has 3 -> ThreeKind
  | [ 2; 2; _ ]
  | [ 2; _; 2 ]
  | [ _; 2; 2 ] ->
    TwoPair
  | l when l |> has 2 -> OnePair
  | _ -> HighCard
;;

let hands_for j_val lines =
  let hand_for line =
    let cards_for hand_str =
      String.to_seq hand_str
      |> of_seq
      |> map (fun c ->
        match c with
        | 'A' -> Ace
        | 'K' -> King
        | 'Q' -> Queen
        | 'J' -> j_val
        | 'T' -> Ten
        | '9' -> Nine
        | '8' -> Eight
        | '7' -> Seven
        | '6' -> Six
        | '5' -> Five
        | '4' -> Four
        | '3' -> Three
        | '2' -> Two
        | _ -> failwith ("Invalid card " ^ String.make 1 c))
    in
    match String.split_on_char ' ' line with
    | []
    | [ _ ] ->
      failwith "Invalid line"
    | [ hand_str; bid_str ] ->
      { cards = cards_for hand_str; bid = int_of_string bid_str }
    | _ -> failwith "Invalid line"
  in
  map hand_for lines
;;

let solve hands =
  let compare ha hb =
    let rec comp_cards ca cb =
      match ca, cb with
      | [], [] -> 0
      | ca :: _, cb :: _ when value_for ca > value_for cb -> 1
      | ca :: _, cb :: _ when value_for ca < value_for cb -> -1
      | _ :: rest_a, _ :: rest_b -> comp_cards rest_a rest_b
      | _ -> failwith "Invalid hand"
    in
    let best hand =
      if find_opt (fun c -> c = Joker) hand.cards != None
      then
        [ Ace
        ; King
        ; Queen
        ; Ten
        ; Nine
        ; Eight
        ; Seven
        ; Six
        ; Five
        ; Four
        ; Three
        ; Two
        ]
        |> map (fun c ->
          { hand with
            cards = map (fun ca -> if ca = Joker then c else ca) hand.cards
          })
        |> fold_left
             (fun ha hb ->
               if (match
                     ( ha |> state_for |> value_for_state
                     , hb |> state_for |> value_for_state )
                   with
                   | va, vb when va > vb -> 1
                   | va, vb when va < vb -> -1
                   | _ -> comp_cards ha.cards hb.cards)
                  >= 0
               then ha
               else hb)
             hand
      else hand
    in
    match
      ( ha |> best |> state_for |> value_for_state
      , hb |> best |> state_for |> value_for_state )
    with
    | va, vb when va > vb -> 1
    | va, vb when va < vb -> -1
    | _ -> comp_cards ha.cards hb.cards
  in
  hands
  |> sort compare
  |> mapi (fun i hand -> i, hand)
  |> fold_left (fun acc (i, h) -> acc + ((i + 1) * h.bid)) 0
;;

let a lines = lines |> hands_for Jack |> solve
let b lines = lines |> hands_for Joker |> solve
