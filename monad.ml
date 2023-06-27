let x = 1 + (4 / 2)

let propagate_none
  (op : int -> int -> int option) (x : int option) (y : int option)
=
  match x, y with
  | None, _ | _ , None -> None
  | Some a, Some b -> op a b

