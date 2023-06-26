let rec repeat f n x = 
  if n = 0 then f x
  else f (repeat f (n - 1) x)


let product_left lst =
  List.fold_left ( *. ) 1.0 lst


let product_right lst =
  List.fold_right ( *. ) lst 1.0

let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

let is_odd n = n mod 2 = 1

let sum_cube_odd n = 
  (from 0 n []) |> (List.filter is_odd) |> (List.map (fun x -> x * x * x)) |> (List.fold_left ( + ) 0)


let rec exists_rec p = function
  | [] -> false
  | h :: t -> if p h then true else exists_rec p t 

let exists_fold p lst =
  if lst = []
    then false
else List.fold_left ( fun x y -> x || p y) false lst


let exists_lib p lst =
  if lst = []
    then false
else List.equal (Bool.equal) (List.filter p lst) []

let account_balance_fold_left account_balance lst = 
  List.fold_left ( Int.sub ) account_balance lst

let account_balance_fold_right account_balance lst =
  account_balance - (List.fold_right (Int.add) lst account_balance)


let rec account_balance_rec account_balance lst = 
  match lst with
  | [] -> account_balance
  | h :: t -> account_balance_rec (account_balance - h) t


let uncurried_append (lst1, lst2) = List.append lst1 lst2

let uncurried_compare (c1, c2) = Char.compare c1 c2

let uncurried_max (x1, x2) = Stdlib.compare x1 x2

let map_composition f g = List.map (fun x -> f (g x))

let greater_than_3 = List.filter (fun x -> x > 3)

let add_1_point = List.map (fun x -> x +. 1.)

let concat_with_comma lst sep =
  match lst with
  | [] -> ""
  | [s] -> s
  | h :: t -> List.fold_left (fun str1 str2 -> str1 ^ "," ^ str2) h t

