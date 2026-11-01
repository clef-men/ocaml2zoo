module Error =
  Implementation_of_cmt_error

exception Ignore

val transl :
  lib:string -> mod_:string -> Typedtree.structure -> Implementation.t
