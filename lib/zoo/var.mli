type t =
  string
type var =
  t

val of_int :
  int -> t

module Hashtbl :
  Hashtbl.S with type key = t
module Hashset :
  Hashset.S with type elt = t

module Internal : sig
  val make :
    string -> t

  val temporary :
    t

  module Generator : sig
    type t

    val create :
      unit -> t

    val next :
      t -> string -> var
  end
end
