include Stdlib.Hashtbl

let create () =
  create 17

let is_empty t =
  length t = 0
