let memoize f =
  let cache = Hashtbl.create 100 in
  fun x ->
    match Hashtbl.find_opt cache x with
    | Some y -> y
    | None ->
      let y = f x in
      Hashtbl.add cache x y;
      y
;;

let ident x = x
let clone l = List.map ident l
