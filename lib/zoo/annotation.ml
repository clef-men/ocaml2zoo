include Annotation_common

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

let typ_propositional = function
  | Type_iprop ->
      true
  | Type_basic _
  | Type_arrow _ ->
      false

let rec typ_scheme acc ty =
  match ty with
  | Type_basic _
  | Type_iprop ->
      { scheme_parameters= List.rev acc
      ; scheme_return= ty
      }
  | Type_arrow (ty1, ty2) ->
      typ_scheme (ty1 :: acc) ty2
let typ_scheme =
  typ_scheme []

let scheme_arity scheme =
  List.length scheme.scheme_parameters

let scheme_is_predicate scheme =
  typ_propositional scheme.scheme_return
