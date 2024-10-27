module Error : sig
  type t

  val pp :
    t Fmt.t
end

exception Error of Location.t * Error.t

val structure :
  lib:string -> mod_:string -> Typedtree.structure -> Syntax.structure
