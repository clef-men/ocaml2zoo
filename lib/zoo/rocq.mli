type path =
  string

type ident =
  string

type term =
  string

type require_kind =
  | Require_only
  | Require_import
  | Require_export

type qualification =
  | Qualified
  | Nonqualified

type mutability = Implementation.mutability =
  | Immutable
  | Mutable

type constancy = Implementation.constancy =
  | Constant
  | Nonconstant

type zoo_constructor =
  { zoo_constructor_tag: string
  ; zoo_constructor_constancy: constancy
  }

type zoo_type =
  | Zoo_record of mutability * string list
  | Zoo_variant of zoo_constructor list

type item =
  | Newline
  | Comment of string
  | Require of require_kind * path
  | Opaque of Lpath.t
  | Zoo_type of qualification * Lpath.t * zoo_type
  | Zoo_val of Lpath.t * Implementation.expression
  | Zoo_fun of Lpath.t * Implementation.binder list * Implementation.expression
  | Zoo_recs of (Lpath.t * Var.t * Implementation.binder list * Implementation.expression) list
  | Zoo_parameter of Lpath.t

type t =
  item list

val pp :
  mod_:string -> t Fmt.t
