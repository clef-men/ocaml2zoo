type typ_descr =
  | Type_constr of Longident.t Location.loc * typ list
  | Type_arrow of typ * typ
and typ =
  typ_descr Location.loc

type predicate =
  { predicate_name: string Location.loc
  ; predicate_type: typ
  ; predicate_properties: string Location.loc list
  }

type declaration =
  | Decl_predicate of predicate
type declarations =
  declaration list

type parameter_descr =
  | Param_unit
  | Param_ident of string
type parameter =
  parameter_descr Location.loc

type binop_descr = Annotation_common.binop =
  | Binop_plus | Binop_minus | Binop_mult
type binop =
  binop_descr Location.loc

type comparison_descr = Annotation_common.comparison =
  | Comparison_le | Comparison_lt
type comparison =
  comparison_descr Location.loc

type expr_descr =
  | Expr_int of int Location.loc
  | Expr_ident of string Location.loc
  | Expr_return
  | Expr_binop of binop * expr * expr
and expr =
  expr_descr Location.loc

type assertion_descr =
  | Assertion_comparison of comparison * expr * expr
  | Assertion_predicate of Longident.t Location.loc * expr list
type assertion =
  assertion_descr Location.loc

type spec =
  { spec_name: string Location.loc option
  ; spec_parameters: parameter list Location.loc
  ; spec_requires: assertion list
  ; spec_ensures: assertion list
  }
