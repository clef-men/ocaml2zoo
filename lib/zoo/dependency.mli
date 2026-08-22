type t =
  string

val make :
  ?suff:string -> string -> string -> t

module Builtin : sig
  val assert_ :
    t
  val assume :
    t
  val diverge :
    t
  val for_ :
    t
  val identifier :
    t
  val structeq :
    t
end
