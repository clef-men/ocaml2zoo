include Stdlib.Hashtbl

let create () =
  create 17

let is_empty t =
  length t = 0

let map_list fn t =
  fold (fun k v acc -> fn k v :: acc) t []

let keys t =
  map_list (fun k _v -> k) t
