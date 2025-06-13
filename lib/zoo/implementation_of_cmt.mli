module Error : sig
  type t

  val pp :
    t Fmt.t
end

exception Error of Location.t * Error.t

exception Ignore

val structure :
  lib:string -> mod_:string -> Typedtree.structure -> Implementation.t
