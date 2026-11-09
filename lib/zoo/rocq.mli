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

type locality =
  | Locality_normal
  | Locality_local
  | Locality_global

type custom =
  unit Fmt.t

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
  | Parameter of ident * term
  | Definition of locality * ident * term option * custom
  | Instance of locality * ident option * custom
  | Opaque of locality * ident
  | Zoo_type of qualification * Lpath.t * zoo_type

type t =
  item list

val pp :
  t Fmt.t
