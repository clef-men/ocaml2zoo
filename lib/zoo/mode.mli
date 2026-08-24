type t =
  | Types
  | Code
  | Opaque

val to_string :
  t -> string
