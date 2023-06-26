let x : int = 3110

module ListStack = struct
  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false
  
  let push x s = x :: s

  exception Empty
  
end