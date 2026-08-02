type t =
  { library: string
  ; module_: string
  ; path: Lpath.t
  }

val make :
  ?lib:string -> ?mod_:string -> Lpath.t -> t

val ident :
  ?lib:string -> ?mod_:string -> string -> t

val pp :
  sep:string -> t Fmt.t
val pp_full :
  sep:string -> t Fmt.t

module Builtin : sig
  val _0 :
    t
  val _1 :
    t

  val contents :
    t
end
