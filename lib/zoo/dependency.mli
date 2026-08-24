type t

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

val self :
  t

val make :
  ?mode:Mode.t -> string -> string -> t

val compare :
  t -> t -> int

val to_rocq :
  require_kind:Rocq.require_kind -> t -> Rocq.item
