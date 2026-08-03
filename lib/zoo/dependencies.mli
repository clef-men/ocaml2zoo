type t =
  string Hashset.t

val of_implementation :
  Implementation.t -> t
