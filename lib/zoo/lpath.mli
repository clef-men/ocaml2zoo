type t =
  | Ident of string
  | Dot of t * string

val cons :
  string -> t -> t

val of_list :
  string list -> t

val append :
  t -> t -> t
val append_list :
  t -> string list -> t

val set_last :
  t -> string -> t

val to_list :
  t -> string list

val to_string :
  sep:string -> ?mod_:string -> t -> string

val pp :
  sep:string -> ?mod_:string -> t Fmt.t
