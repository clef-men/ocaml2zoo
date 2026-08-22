type t =
  Dependency.t Hashset.t

val of_ast :
  Ast.t -> t
