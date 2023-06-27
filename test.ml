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