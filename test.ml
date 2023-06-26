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