type t =
  | Cmt
  | Cmti

val of_filename :
  string -> t option

val pp :
  t Fmt.t
