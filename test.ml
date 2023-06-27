let x : int = 3110

module ListStack = struct
  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false
  
  let push x s = x :: s

  exception Empty

  let peek = function
  | [] -> raise Empty
  | h :: _ -> h

  let pop = function
  | [] -> raise Empty
  | _ :: t -> t
end

let stk = ListStack.push 2 (ListStack.push 1 ListStack.empty )

let stk = ListStack.(push 2 (push 1 empty))

let s1 = ListStack.(empty |> push 1 |> push 2)

let s2 = ListStack.(empty |> push 3)

module M = struct
  let x = 0
  type t = int
end


module M = struct
  let rec even = function
  | 0 -> true
  | n -> odd (n - 1)
  and odd = function
  | 0 -> false
  | n -> even (n - 1)
end

module M = struct
  open List

  (** [uppercase_all lst] upper-cases all the elements of [lst]. *)
  let uppercase_all = map String.uppercase_ascii
end


module type LIST_STACK = sig
  exception Empty
  val empty : 'a list
  val is_empty : 'a list -> bool
  val push : 'a -> 'a list -> 'a list
  val peek : 'a list -> 'a
  val pop : 'a list -> 'a list
end


module type LIST_STACK = sig
  (** [Empty] is raised when an operation cannot be applied
      to an empty stack. *)
  exception Empty

  (** [empty] is the empty stack. *)
  val empty : 'a list

  (** [is_empty s] is whether [s] is empty. *)
  val is_empty : 'a list -> bool

  (** [push x s] pushes [x] onto the top of [s]. *)
  val push : 'a -> 'a list -> 'a list

  (** [peek s] is the top element of [s].
      Raises [Empty] if [s] is empty. *)
  val peek : 'a list -> 'a

  (** [pop s] is all but the top element of [s].
      Raises [Empty] if [s] is empty. *)
  val pop : 'a list -> 'a list
end


module ListStack = struct
  let empty = []

  let is_empty = function [] -> true | _ -> false

  let push x s = x :: s

  exception Empty

  let peek = function
    | [] -> raise Empty
    | x :: _ -> x

  let pop = function
    | [] -> raise Empty
    | _ :: s -> s
end

module ListStack : LIST_STACK = struct
  let empty = []

  let is_empty = function [] -> true | _ -> false

  let push x s = x :: s

  exception Empty

  let peek = function
    | [] -> raise Empty
    | x :: _ -> x

  let pop = function
    | [] -> raise Empty
    | _ :: s -> s
end


module type X = sig
  val x : int
end

module type T = sig
  module Inner : X
end

module M : T = struct
  module Inner : X = struct
    let x = 42
  end
end

module Math = struct
  let rec fact_aux n acc = 
    if n = 0 then acc  else fact_aux (n - 1) (n * acc)

  let fact n = fact_aux n 1
end

module Math = struct
  let fact n = 
    let rec fact_aux n acc = 
      if n = 0 then acc  else fact_aux (n - 1) (n * acc)
    in fact_aux n 1
end

module type Math = sig
  val fact : int -> int
end

module Math = struct
  let fact n = 
    let rec fact_aux n acc = 
      if n = 0 then acc  else fact_aux (n - 1) (n * acc)
    in fact_aux n 1
end


module type MATH = sig
  (** [fact n] is [n!]. *)
  val fact : int -> int
end

module Math = struct
  (** [fact_aux n acc] is [n! * acc]. *)
  let rec fact_aux n acc =
    if n = 0 then acc else fact_aux (n - 1) (n * acc)

  let fact n = fact_aux n 1
end

module MathCheck : MATH = Math

let _ = MathCheck.fact

module type C_PUBLICK = sig
  val y : int
end

module CPrivate = struct
  let x = 0
  let y = 0
end

module C : C_PUBLICK = CPrivate

module type LIST_STACK = sig
   (** [Empty] is raised when an operation cannot be applied
      to an empty stack. *)
      exception Empty

      (** [empty] is the empty stack. *)
      val empty : 'a list
    
      (** [is_empty s] is whether [s] is empty. *)
      val is_empty : 'a list -> bool
    
      (** [push x s] pushes [x] onto the top of [s]. *)
      val push : 'a -> 'a list -> 'a list
    
      (** [peek s] is the top element of [s].
          Raises [Empty] if [s] is empty. *)
      val peek : 'a list -> 'a
    
      (** [pop s] is all but the top element of [s].
          Raises [Empty] if [s] is empty. *)
      val pop : 'a list -> 'a list

      val size : 'a list -> int
end

module ListStack : LIST_STACK = struct
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x s = x :: s
  let peek = function [] -> raise Empty | x :: _ -> x
  let pop = function [] -> raise Empty | _ :: s -> s
  let size = List.length
end

module type LIST_STACK = sig
  type 'a stack
  exception Empty
  val empty : 'a stack
  val is_empty : 'a stack -> bool
  val push : 'a -> 'a stack -> 'a stack
  val peek : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
  val size : 'a stack -> int
end



module ListStackCachedSize : LIST_STACK = struct
  type 'a stack = 'a list * int
  exception Empty
  let empty = ([], 0)
  let is_empty = function 
    | ([], 0) -> true
    | _ -> false
  let push x = function
  | (l, s) -> (x :: l, s + 1)
  let peek = function ([], _) -> raise Empty | (x :: _, _) -> x
  let pop = function ([], _) -> raise Empty | (_ :: l, s) -> (l, s - 1)
  let size = snd
end

module ListStack : LIST_STACK = struct
  type 'a stack = 'a list
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x s = x :: s
  let peek = function [] -> raise Empty | x :: _ -> x
  let pop = function [] -> raise Empty | _ :: s -> s
  let size = List.length
end

module type Stack = sig
  type 'a stack
  exception Empty
  val empty : 'a stack
  val is_empty : 'a stack -> bool
  val push : 'a -> 'a stack -> 'a stack
  val peek : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
  val size : 'a stack -> int
end

module type Stack = sig
  type 'a t
  exception Empty
  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
  val size : 'a t -> int
end

module ListStack : Stack = struct
  type 'a t = 'a list
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x s = x :: s
  let peek = function [] -> raise Empty | x :: _ -> x
  let pop = function [] -> raise Empty | _ :: s -> s
  let size = List.length
end

module CustomStack : Stack = struct
  type 'a entry = {top : 'a; rest : 'a t; size : int}
  and 'a t = S of 'a entry option
  exception Empty
  let empty = S None
  let is_empty = function S None -> true | _ -> false
  let size = function S None -> 0 | S (Some {size}) -> size
  let push x s = S (Some {top = x; rest = s; size = size s + 1})
  let peek = function S None -> raise Empty | S (Some {top}) -> top
  let pop = function S None -> raise Empty | S (Some {rest}) -> rest
end

module type Stack = sig
  type 'a t
  exception Empty
  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
  val size : 'a t -> int
  val to_list : 'a t -> 'a list
end

module ListStack : Stack = struct
  type 'a t = 'a list
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push = List.cons
  let peek = function [] -> raise Empty | x :: _ -> x
  let pop = function [] -> raise Empty | _ :: s -> s
  let size = List.length
  let to_list = Fun.id
end

module type Queue = sig
  type 'a t

  exception Empty

  val empty : 'a t

  val enqueue : 'a -> 'a t -> 'a t

  val front : 'a t -> 'a 

  val dequeue : 'a t -> 'a t

  val size : 'a t -> int

  val to_list : 'a t -> 'a list
end


module ListQueue : Queue = struct
  type 'a t = 'a list
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let enqueue x q = q @ [x]
  let front = function [] -> raise Empty | x :: _ -> x
  let dequeue = function [] -> raise Empty | _ :: q -> q
  let size = List.length
  let to_list = Fun.id
end


module BatchedQueue : Queue = struct
  (** [{o; i}] represents the queue [o @ List.rev i]. For example,
      [{o = [1; 2]; i = [5; 4; 3]}] represents the queue [1, 2, 3, 4, 5],
      where [1] is the front element. To avoid ambiguity about emptiness,
      whenever only one of the lists is empty, it must be [i]. For example,
      [{o = [1]; i = []}] is a legal representation, but [{o = []; i = [1]}]
      is not. This implies that if [o] is empty, [i] must also be empty. *)
  type 'a t = {o : 'a list; i : 'a list}

  exception Empty

  let empty = {o = []; i = []}

  let is_empty = function
    | {o = []} -> true
    | _ -> false

  let enqueue x = function
    | {o = []} -> {o = [x]; i = []}
    | {o; i} -> {o; i = x :: i}

  let front = function
    | {o = []} -> raise Empty
    | {o = h :: _} -> h

  let dequeue = function
    | {o = []} -> raise Empty
    | {o = [_]; i} -> {o = List.rev i; i = []}
    | {o = _ :: t; i} -> {o = t; i}

  let size {o; i} = List.(length o + length i)
  
  let to_list {o; i} = o @ List.rev i
end


module type Map = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  
  val lookup : 'k -> ('k, 'v) t -> 'v

  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module AssocListMap : Map = struct
  (** The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi].
      If a key appears more than once in the list, it is bound to the
      the left-most occurrence in the list. *)
  type ('k, 'v) t = ('k * 'v) list
  let empty = []
  let insert k v m = (k, v) :: m
  let lookup k m = List.assoc k m
  let keys m = List.(m |> map fst |> sort_uniq Stdlib.compare)
  let bindings m = m |> keys |> List.map (fun k -> (k, lookup k m))
end

module type Set = sig
  type 'a t

  val empty : 'a t

  val mem : 'a -> 'a t -> bool

  val add : 'a -> 'a t -> 'a t

  val elements : 'a t -> 'a list
end

module UniqListSet : Set = struct
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let add x s = if mem x s then s else x :: s
  let elements = Fun.id
end

module ListSet : Set = struct
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let add = List.cons
  let elements s = List.sort_uniq Stdlib.compare s
end

module type Ring = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t (* additive inverse *)
  val to_string : t -> string
end

module IntRing : Ring = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = Stdlib. ( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib. ( ~- )
  let to_string = string_of_int
end

let _ = IntRing.(one + one)

let _ = IntRing.(one + one |> to_string)

module FloatRing : Ring = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let to_string = string_of_float
end

module IntRing = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int
end

module _ : Ring = IntRing


module type INT_RING = Ring with type t = int

module IntRing : INT_RING = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int
end


module type FLOAT_RING = Ring with type t = float

module FloatRing : FLOAT_RING = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let to_string = string_of_float
end

module FloatRing : Ring with type t = float = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let to_string = string_of_float
end

module type Set = sig
  type 'a t
  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val elements : 'a t -> 'a list
end

module ListSet : Set = struct
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let add = List.cons
  let elements s = List.sort_uniq Stdlib.compare s
end

module ListSetExtended = struct
  include ListSet
  let of_list lst = List.fold_right add lst empty
end

module type SetExtended = sig
  include Set
  val of_list : 'a list -> 'a t
end

module type SetExtended = sig
  (* BEGIN all the includes *)
  type 'a t
  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val elements  : 'a t -> 'a list
  (* END all the includes *)
  val of_list : 'a list -> 'a t
end
 
module ListSetExtended : SetExtended = struct
  include ListSet
  let of_list lst = List.fold_right add lst empty
end

module ListSetImpl = struct
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let add = List.cons
  let elements s = List.sort_uniq Stdlib.compare s
end

module ListSet : Set = ListSetImpl

module type SetExtended = sig
  include Set
  val of_list : 'a list -> 'a t
end

module ListSetExtendedImpl = struct
  include ListSetImpl
  let of_list lst = lst
end

module ListSetExtended : SetExtended = ListSetExtendedImpl


(**[functor]*)

module type X = sig
  val x : int
end

module IncX (M : X) = struct
  let x = M.x + 1
end

module IntMap = Map.Make(Int)


let m1 = IntMap.(add 1 "one" empty)

let _ = IntMap.(find 1 m1)

let _ = IntMap.(mem 42 m1)

let _ = IntMap.(find 42 m1)

let m1_list = IntMap.(bindings m1)

let m2 = IntMap.(add 1 1. empty)
let m2_list = IntMap.(bindings m2)


type name = {first : string; last : string}

module Name = struct
  type t = name

  let compare {first = first1; last = last1} {first = first2; last = last2}
  =
  match String.compare last1 last2 with
  | 0 -> String.compare first1 first2
  | c -> c
end

module NameMap = Map.Make (Name)


let k1 = {last = "Kardashian"; first = "Kourtney"}
let k2 = {last = "Kardashian"; first = "Kimberly"}
let k3 = {last = "Kardashian"; first = "Khloe"}

let nm =
  NameMap.(empty |> add k1 1979 |> add k2 1980 |> add k3 1984)

let lst = NameMap.bindings nm


let is_even n = n mod 2 = 0

let rec is_sorted  = function
  | [] | [ _ ] -> true
  | h1 :: (h2 :: _ as t) -> h1 <= h2 && is_sorted t


let counter = ref 0

let next_val =
    fun () ->
      counter := !counter + 1;
      !counter

let next_val =
  fun () ->
  incr counter;
  !counter


type 'a pointer = 'a ref option

let null : 'a pointer = None

let malloc (x : 'a) : 'a pointer = Some (ref x)

let p = malloc 42

exception SegFault

let deref (ptr : 'a pointer) : 'a =
  match ptr with None -> raise SegFault | Some r -> !r


(** Singly-linke Lists *)

type 'a node = {next : 'a mlist; value : 'a}

and 'a mlist = 'a node option ref


let empty () : 'a mlist = ref None

let insert_first (lst : 'a mlist) (v : 'a) : unit = 
  lst := Some {next = ref !lst; value = v}

let rec to_list (lst : 'a mlist) : 'a list = 
  match !lst with None -> [] | Some {next; value} -> value :: to_list next

let lst0 = empty ()
let lst1 = lst0
let () = insert_first lst0 1
let single_linked_list = to_list lst1


type point = {x : int; y : int; mutable c : string}

let p = {x = 0; y = 0; c = "red"}

let () = p.c <- "white"

type 'a node = {
  mutable next : 'a node option;
  value : 'a
}

type 'a mlist = {
  mutable first : 'a node option
}

(** [insert_first lst n] mutates mlist [lst] by inserting value [v] as the
    first value in the list. *)
let insert_first (lst : 'a mlist) (v : 'a) =
  lst.first <- Some {value = v; next = lst.first}

(** [empty ()] is an empty singly-linked list. *)
let empty () : 'a mlist = {
  first = None
}

(** [to_list lst] is an OCaml list containing the same values as [lst]
    in the same order. Not tail recursive. *)
let to_list (lst : 'a mlist) : 'a list =
  let rec helper = function
    | None -> []
    | Some {next; value} -> value :: helper next
  in
  helper lst.first

(** Mutable Stacks*)

module type MutableStack = sig
  type 'a t

  exception Empty

  val empty : unit -> 'a t

  val push : 'a -> 'a t -> unit

  val peek : 'a t -> 'a 

  val pop : 'a t -> unit
end


module MutableRecordStack : MutableStack = struct
  type 'a node = {value : 'a; mutable next : 'a node option}

  type 'a t = {mutable top : 'a node option}

  exception Empty

  let empty () = {top = None}

  let push x s = s.top <- Some {value = x; next = s.top}

  let peek s =
    match s.top with
    | None -> raise Empty
    | Some {value} -> value
  
  let pop s =
    match s.top with
    | None -> raise Empty
    | Some {next} -> s.top <- next
end

(** Arrays and Loops *)

let v = [|0.; 1.|]

let () = v.(0) <- 5.

module type Map = sig
  type ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  val find : 'k -> ('k, 'v) t -> 'v option

  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

  val empty : ('k, 'v) t

  val of_list : ('k * 'v) list -> ('k, 'v) t

  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module ListMap : Map = struct
  (** AF: [[(k1, v1); (k2, v2); ...; (kn, vn)]] is the map {k1 : v1, k2 : v2,
      ..., kn : vn}. If a key appears more than once in the list, then in the
      map it is bound to the left-most occurrence in the list. For example,
      [[(k, v1); (k, v2)]] represents {k : v1}. The empty list represents
      the empty map.
      RI: none. *)
  type ('k, 'v) t = ('k * 'v) list

  (** Efficiency: O(1). *)
  let insert k v m = (k, v) :: m

  (** Efficiency: O(n). *)
  let find = List.assoc_opt

  (** Efficiency: O(n). *)
  let remove k lst = List.filter (fun (k', _) -> k <> k') lst

  (** Efficiency: O(1). *)
  let empty = []

  (** Efficiency: O(1). *)
  let of_list lst = lst

  (** [keys m] is a list of the keys in [m], without
      any duplicates.
      Efficiency: O(n log n). *)
  let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare

  (** [binding m k] is [(k, v)], where [v] is the value that [k]
       binds in [m].
       Requires: [k] is a key in [m].
       Efficiency: O(n). *)
  let binding m k = (k, List.assoc k m)

  (** Efficiency: O(n log n) + O(n) * O(n), which is O(n^2). *)
  let bindings m = List.map (binding m) (keys m)
end

module type DirectAddressMap = sig
  type 'v t

  val insert : int -> 'v -> 'v t -> unit

  val find : int -> 'v t -> 'v option

  val remove : int -> 'v t -> unit

  val create : int -> 'v t

  val of_list : int -> (int * 'v) list -> 'v t

  val bindings : 'v t -> (int * 'v) list
end

module ArrayMap : DirectAddressMap = struct
  (** AF: [[|Some v0; Some v1; ... |]] represents {0 : v0, 1 : v1, ...}.
      If element [i] of the array is instead [None], then [i] is not
      bound in the map.
      RI: None. *)
  type 'v t = 'v option array

  (** Efficiency: O(1) *)
  let insert k v a = a.(k) <- Some v

  (** Efficiency: O(1) *)
  let find k a = a.(k)

  (** Efficiency: O(1) *)
  let remove k a = a.(k) <- None

  (** Efficiency: O(c) *)
  let create c = Array.make c None

  (** Efficiency: O(c) *)
  let of_list c lst =
    (* O(c) *)
    let a = create c in
    (* O(c) * O(1) = O(c) *)
    List.iter (fun (k, v) -> insert k v a) lst;
    a

  (** Efficiency: O(c) *)
  let bindings a =
    let bs = ref [] in
    (* O(1) *)
    let add_binding k v =
      match v with None -> () | Some v -> bs := (k, v) :: !bs
    in
    (* O(c) *)
    Array.iteri add_binding a;
    !bs
end

module type TableMap = sig
  (** [('k, 'v) t] is the type of mutable table-based maps that bind
      keys of type ['k] to values of type ['v]. *)
  type ('k, 'v) t

  (** [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was
      already bound in [m], that binding is replaced by the binding to
      [v]. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> unit

  (** [find k m] is [Some v] if [m] binds [k] to [v], and [None] if [m]
      does not bind [k]. *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [remove k m] mutates [m] to remove any binding of [k]. If [k] was
      not bound in [m], the map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> unit

  (** [create hash c] creates a new table map with capacity [c] that
      will use [hash] as the function to convert keys to integers.
      Requires: The output of [hash] is always non-negative, and [hash]
      runs in constant time. *)
  val create : ('k -> int) -> int -> ('k, 'v) t

  (** [bindings m] is an association list containing the same bindings
      as [m]. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list

  (** [of_list hash lst] creates a map with the same bindings as [lst],
      using [hash] as the hash function. Requires: [lst] does not
      contain any duplicate keys. *)
  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t
end


let  t = Hashtbl.create 16
let _ = for i = 1 to 16 do
  Hashtbl.add t i (string_of_int i)
done

let _ = Hashtbl.stats t


(** RB tree *)

type color = Red | Black

type 'a rbtree = Leaf | Node of color * 'a * 'a rbtree * 'a rbtree

let rec mem x = function
  | Leaf -> false
  | Node (_, y, l, r) -> 
    if x < y then mem x l
    else if x > y then mem x  r
    else true

let balance = function
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
    Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d -> Node (a, b, c, d)

let insert x s =
  let rec ins = function
    | Leaf -> Node (Red, x, Leaf, Leaf)
    | Node (color, y, a, b) as s ->
      if x < y then balance (color, y, ins a, b)
      else if x > y then balance (color, y, a, ins b)
      else s
  in
  match ins s with
  | Node (_, y, a, b) -> Node (Black, y, a, b)
  | Leaf -> (* guaranteed to be nonempty *)
    failwith "RBT insert failed with ins returning leaf"


let rec ones = 1 :: ones

let rec a = 0 :: b and b = 1 :: a


(** A signature for Lwt-style promises, with better names *)
module type PROMISE = sig
  type 'a state =
    | Pending
    | Resolved of 'a
    | Rejected of exn

  type 'a promise

  type 'a resolver

  (** [make ()] is a new promise and resolver. The promise is pending. *)
  val make : unit -> 'a promise * 'a resolver

  (** [return x] is a new promise that is already resolved with value
      [x]. *)
  val return : 'a -> 'a promise

  (** [state p] is the state of the promise *)
  val state : 'a promise -> 'a state

  (** [resolve r x] resolves the promise [p] associated with [r] with
      value [x], meaning that [state p] will become [Resolved x].
      Requires: [p] is pending. *)
  val resolve : 'a resolver -> 'a -> unit

  (** [reject r x] rejects the promise [p] associated with [r] with
      exception [x], meaning that [state p] will become [Rejected x].
      Requires: [p] is pending. *)
  val reject : 'a resolver -> exn -> unit
end
module Promise : PROMISE = struct
  type 'a state =
    | Pending
    | Resolved of 'a
    | Rejected of exn

  type 'a promise = 'a state ref

  type 'a resolver = 'a promise

  (** [write_once p s] changes the state of [p] to be [s]. If [p] and
      [s] are both pending, that has no effect. Raises: [Invalid_arg] if
      the state of [p] is not pending. *)
  let write_once p s =
    if !p = Pending then p := s else invalid_arg "cannot write twice"

  let make () =
    let p = ref Pending in
    (p, p)

  let return x = ref (Resolved x)

  let state p = !p

  let resolve r x = write_once r (Resolved x)

  let reject r x = write_once r (Rejected x)
end

module type Monad = sig
  type 'a t 
  val return : 'a -> 'a t (* The return operation metaphorically puts a value into a box*)
  val bind : 'a t -> ('a -> 'b t) -> 'b t (* *)
end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end