include module type of struct
  include Annotation_common
end

type btyp =
  | Unit
  | Bool
  | Int
  | Val

type typ =
  | Type_basic of btyp
  | Type_iprop
  | Type_arrow of typ * typ

type 'a scheme =
  { scheme_parameters: 'a list
  ; scheme_return: 'a
  }

type typ_scheme =
  typ scheme

type property =
  | Exclusive
  | Persistent

type predicate =
  { predicate_path: Lpath.t
  ; predicate_type: typ
  ; predicate_properties: property list
  }

type parameter =
  | Param_unit
  | Param_ident of string

type expr =
  | Expr_int of int
  | Expr_ident of string
  | Expr_return
  | Expr_binop of binop * expr * expr

type assertion =
  | Assertion_comparison of comparison * expr * expr
  | Assertion_predicate of Lpath.t * expr list

type spec =
  { spec_name: string option
  ; spec_parameters: (parameter * btyp) list
  ; spec_return: btyp
  ; spec_requires_quantifiers: (string * typ) list
  ; spec_requires: assertion list
  ; spec_ensures_quantifiers: (string * typ) list
  ; spec_ensures: assertion list
  }

type item =
  | Item_predicate of predicate
  | Item_spec of Lpath.t * spec list

type t =
  { library: string
  ; module_: string
  ; items: item list
  }

val typ_propositional :
  typ -> bool
val typ_scheme :
  typ -> typ scheme

val scheme_arity :
  'a scheme -> int
val scheme_is_predicate :
  typ_scheme -> bool
