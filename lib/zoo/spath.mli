type t =
  | Ident of string
  | Dot of t * string

val of_list :
  string list -> t

val to_list :
  t -> string list

val to_string :
  t -> string

val pp :
  t Fmt.t
