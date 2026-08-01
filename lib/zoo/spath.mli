type t =
  | Ident of string
  | Dot of t * string

val cons :
  string -> t -> t

val of_list :
  string list -> t

val to_list :
  t -> string list

val to_string :
  sep:string -> t -> string

val append :
  t -> t -> t
val append_list :
  t -> string list -> t

val pp :
  sep:string -> t Fmt.t
