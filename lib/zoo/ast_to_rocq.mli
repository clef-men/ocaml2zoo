type mode =
  | Types
  | Code
  | Opaque

val transl :
  mode:mode -> Ast.t -> Rocq.t
