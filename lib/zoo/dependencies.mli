type t

val of_ast :
  Ast.t -> t

val to_rocq :
  require_kind:Rocq.require_kind -> t -> Rocq.t
