type mode =
  | Types
  | Code
  | Opaque

val transl :
  mode:mode -> Implementation.t -> Rocq.t
