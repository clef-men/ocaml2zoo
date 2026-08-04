type t =
  string

let of_int =
  string_of_int

let internal =
  Printf.sprintf "@%s"
let temporary =
  internal "tmp"

module Hashtbl =
  Hashtbl.Make(String)
module Hashset =
  Hashset.Make(String)
