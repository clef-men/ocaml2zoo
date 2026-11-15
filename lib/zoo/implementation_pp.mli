open Implementation

val pp_expression :
  mod_:string -> expression Fmt.t

val pp_fun :
  mod_:string -> (binder list * expression) Fmt.t

val pp_rec :
  mod_:string -> (Lpath.t * Var.t * binder list * expression) Fmt.t

val pp_recs :
  mod_:string -> (Lpath.t * Var.t * binder list * expression) list Fmt.t
