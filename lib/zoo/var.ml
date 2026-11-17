type t =
  string
type var =
  t

let of_int =
  string_of_int

module Hashtbl =
  Hashtbl.Make(String)
module Hashset =
  Hashset.Make(String)

module Internal = struct
  let make =
    Printf.sprintf "@%s"

  let temporary =
    make "tmp"

  module Generator = struct
    type t =
      int Hashtbl.t

    let create =
      Hashtbl.create

    let next t name =
      let n = Option.value ~default:0 (Hashtbl.find_opt t name) in
      Hashtbl.replace t name (n + 1) ;
      let name =
        if n = 0 then
          name
        else
          name ^ Int.to_string_subscript n
      in
      make name
  end
end
