type t

val of_ast :
  Implementation.t -> t

val to_rocq :
  require_kind:Rocq.require_kind -> t -> Rocq.t
