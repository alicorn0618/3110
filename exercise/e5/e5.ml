module type ComplexSig = sig
  type t = float * float
  val zero : t
  val add : t -> t -> t
end

module Complex : ComplexSig = struct
  type t = float * float
  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end



module ListQueue = struct
  type t = int list * int list
  let empty = [], []
  exception Empty
  let enqueue x (q : t) = 
    match q with
    | (front, back) -> (x :: front, back)
  let dequeue (q : t) = 
    match q with
    | ([], []) -> raise Empty
    | (front, []) -> List.hd (List.rev front), ([], List.tl (List.rev front))
    | (front, back) -> List.hd back, (front, List.tl back)
 end


 (** Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty


module BatchedQueue = struct
  type t = int list * int list
  let empty = [], []
  exception Empty
  let enqueue x (q : t) = 
    match q with
    | (front, back) -> (x :: front, back)
  let dequeue (q : t) = 
    match q with
    | ([], []) -> raise Empty
    | (front, []) -> List.hd (List.rev front), ([], List.tl (List.rev front))
    | (front, back) -> List.hd back, (front, List.tl back)
end

let fill_batchedqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (BatchedQueue.enqueue n q) in
  loop n BatchedQueue.empty

module BstMap = struct
  type 'a  tree = |Leaf | Node of 'a * 'a tree * 'a tree
  
end