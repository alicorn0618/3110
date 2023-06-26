let demo_lst = [1; 2; 3; 4; 5]

let demo_lst1 = 1 :: 2 :: 3 :: 4 :: 5 :: []

let demo_lst2 = 1 :: ([2; 3; 4] @ [5])

let lst_product lst = List.fold_left ( * ) 1 lst

let lst_concat lst = List.fold_left ( ^ ) "" lst

let rec weird_requirement_aux lst len =
  match lst with
  | [] -> if len = 2 || len = 4 then true else false
  | h :: t -> if len = 0 && h = "bigred" then weird_requirement_aux t (len + 1) else if len = 1 && h = "bigred" then weird_requirement_aux t (len + 1) else if len > 4 then false else weird_requirement_aux t (len + 1)

let weird_requirement lst = weird_requirement_aux lst 0


let fifth lst = if List.length lst < 5 then 0 else (List.nth lst 4)

let descend_list (lst:int list) = List.sort (fun a b -> if a > b then -1 else if a < b then 1 else 1) lst

let last lst = List.nth lst ((List.length lst) - 1)

let any_zeores lst = List.exists (fun x -> x = 0) lst

let rec take_aux n acc lst =
  if n = 0 then acc
  else
    match lst with
    | [] -> acc
    | h :: t -> take_aux (n - 1) (h :: acc) t

let take n lst = List.rev (take_aux n [] lst)

let rec drop n lst =
  if n = 0 then lst
  else
    match lst with
    | [] -> []
    | _ :: t -> drop (n- 1) t


let rec is_unimodal_aux lst increase = 
  match lst with
  | [] | [_] -> true
  | [_; _] -> false
  | h1 :: (h2 :: _) as t ->
    if increase && h1 < h2 then is_unimodal_aux t increase
    else if increase && h1 > h2 then is_unimodal_aux t (not increase)
    else if (not increase) && h1 < h2 then is_unimodal_aux t increase
    else false

let is_unimodal lst = is_unimodal_aux lst true

let rec powerset lst =
  match lst with
  | [] -> [[]]
  | h :: t -> let rest = powerset t in List.map (fun x -> h :: x) rest @ rest

let rec print_int_list = function
  | [] -> ()
  | h :: t -> print_endline (string_of_int h); print_int_list t

let print_int_list' lst =
  List.iter (fun x -> print_endline (string_of_int x)) lst

type student = {first_name: string; last_name : string; gpa : float}
let dennis = {first_name = "dennis"; last_name = "ritchie"; gpa = 4.}
let student_full_name (stu: student) = stu.first_name ^ " " ^ stu.last_name
let new_student fn ln gpa = {first_name = fn; last_name = ln; gpa = gpa}

type poketype = Normal | Fire | Water
type pokemon = {name: string; hp : int; ptype : poketype}
let charizard = {name = "charizard"; hp = 78; ptype = Fire}
let squirtle = {name = "squirtle"; hp = 44; ptype = Water}

let safe_hd = function
  | [] -> None
  | h :: _ -> Some h

let safe_tl = function
  | [] -> None
  | _ :: t -> Some t

let max_hp pl = 
  let rec max_hp_aux (res : 'pokemon option) pl =
    match pl with
    | [] -> res
    | h :: t -> if Option.is_none res then max_hp_aux (Some h) t else if (Option.get res).hp < h.hp then max_hp_aux (Some h) t else max_hp_aux res t
  in max_hp_aux None pl

(** [insert k v lst] is an association list that binds key [k] to value [v]
    and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

    (** [lookup k lst] is [Some v] if association list [lst] binds key [k] to
        value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let empy_assoc_list = []

let assoc_1 = insert 1 "one" empy_assoc_list

let assoc_2 = insert 2 "two" assoc_1

let assoc_3 = insert 3 "three" assoc_2

let res_2 = lookup 2 assoc_3

let res_4 = lookup 4 assoc_3


type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x: int) : sign = 
  if x > 0 then Pos
  else if x < 0 then Neg
  else Zero

let quadrant : int * int -> quad option = fun (x, y) ->
  match (sign x, sign y) with
  | (Pos, Pos) -> Some I
  | (Neg, Pos) -> Some II
  | (Neg, Neg) -> Some III
  | (Pos, Neg) -> Some IV
  | _ -> None

let quadrant_when : int * int -> quad option = function
  | (x, y) when x > 0 && y > 0 -> Some I
  | (x, y) when x < 0 && y > 0 -> Some II
  | (x, y) when x < 0 && y < 0 -> Some III
  | (x, y) when x > 0 && y < 0 -> Some IV
  | _ -> None

type 'a tree =
  | Leaf
  | Node of 'a node
  and 'a node = {
  value: 'a;
  left: 'a tree;
  right: 'a tree
}

let rec depth = function
  | Leaf -> 0
  | Node node -> match node with
    {value = _; left = l; right = r} -> 1 + Int.max (depth l) (depth r)

let rec same_shape t1 t2 = 
  match (t1, t2) with
  | (Leaf, Leaf) -> true
  | (Node n1, Node n2) -> same_shape n1.left n2.left && same_shape n1.right n2.right
  | _ -> false

let list_max = 
  function 
  | [] -> failwith "list_max"
  | h :: t -> let rec list_max_aux res = function
                | [] -> res
                | h :: t -> if h > res then list_max_aux h t else list_max_aux res t
in list_max_aux h t

let list_max_string = function
  | [] -> "empty"
  | h :: t -> let rec list_max_string_aux res = function
                | [] -> string_of_int res
                | h :: t -> if h > res then list_max_string_aux h t else list_max_string_aux res t
in list_max_string_aux h t


let rec tree_max : ('a * 'b) tree -> 'a = function
  | Leaf -> failwith "tree_max"
  | Node node -> match node with
    {value = (a, _); left = l; right = r} -> let left_max = tree_max l in let right_max = tree_max r in if a > left_max && a > right_max then a else if left_max > a && left_max > right_max then left_max else right_max


let rec tree_min : ('a * 'b) tree -> 'a = function
  | Leaf -> failwith "tree_min"
  | Node node -> match node with
    {value = (a, _); left = l; right = r} -> let left_min = tree_min l in let right_min = tree_min r in if a < left_min && a < right_min then a else if left_min < a && left_min < right_min then left_min else right_min

let tree_empty = function
  | Leaf -> true
  | Node _ -> false
  
let rec is_bst : ('a * 'b) tree -> bool = function
  | Leaf -> true
  | Node node -> match node with
    {value = (a, _); left = l; right = r} -> if tree_empty l && tree_empty r then true else if tree_empty l && tree_min r > a && is_bst r then true else if tree_empty r && tree_max l < a && is_bst l then true else if tree_max l < a && tree_min r > a && is_bst l && is_bst r then true else false



let sign x = if x > 0 then `Pos else if x < 0 then `Neg else `Zero

let quadrant (x, y) = 
  match (sign x, sign y) with
  | (`Pos, `Pos) -> Some `I
  | (`Neg, `Pos) -> Some `II
  | (`Neg, `Neg) -> Some `III
  | (`Pos, `Neg) -> Some `IV
  | _ -> None

