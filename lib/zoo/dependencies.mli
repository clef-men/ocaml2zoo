type t =
  string Hashset.t

val of_ast :
  Ast.t -> t
