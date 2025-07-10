type variable =
  string

type binder =
  variable option

type field =
  string

type tag =
  string

type rec_flag = Asttypes.rec_flag =
  | Nonrecursive
  | Recursive

type mutability =
  | Mutable
  | Immutable_nongenerative
  | Immutable_generative_weak
  | Immutable_generative_strong

type typ =
  | Type_product of field list
  | Type_record of field list
  | Type_variant of tag list

type pattern =
  | Pat_var of variable
  | Pat_tuple of binder list
  | Pat_constr of tag * binder list

type unop =
  | Unop_neg
  | Unop_minus

type binop =
  | Binop_plus | Binop_minus | Binop_mult | Binop_quot | Binop_rem
  | Binop_land | Binop_lor | Binop_lsl | Binop_lsr
  | Binop_eq | Binop_ne | Binop_le | Binop_lt | Binop_ge | Binop_gt
  | Binop_and | Binop_or
  | Binop_structeq | Binop_structne

type expression =
  | Global of variable
  | Local of variable
  | Bool of bool
  | Int of int
  | Let of pattern * expression * expression
  | Letrec of rec_flag * variable * binder list * expression * expression
  | Seq of expression * expression
  | Fun of binder list * expression
  | Apply of expression * expression list
  | Unop of unop * expression
  | Binop of binop * expression * expression
  | If of expression * expression * expression option
  | For of binder * expression * expression * expression
  | Alloc of expression * expression
  | Tuple of expression list
  | Ref of expression
  | Record of expression list
  | Constr of mutability * tag * expression list
  | Proj of expression * field
  | Match of expression * branch list * fallback option
  | Ref_get of expression
  | Ref_set of expression * expression
  | Record_get of expression * field
  | Record_set of expression * field * expression
  | Is_immediate of expression
  | Get_tag of expression
  | Get_size of expression
  | Atomic_loc of expression * field
  | Load of expression * expression
  | Store of expression * expression * expression
  | Xchg of expression * expression
  | Cas of expression * expression * expression
  | Faa of expression * expression
  | Fail
  | Skip
  | Proph
  | Resolve of expression * expression * expression
  | Id
and branch =
  { branch_tag: tag;
    branch_fields: binder list;
    branch_as: binder;
    branch_expr: expression;
  }
and fallback =
  { fallback_as: binder;
    fallback_expr: expression;
  }

type value =
  | Val_expr of variable * expression
  | Val_fun of variable * binder list * expression
  | Val_recs of (variable * variable * binder list * expression) list
  | Val_opaque of variable

type definition =
  | Type of variable * typ
  | Val of value

type t =
  { library: string;
    module_: string;
    dependencies: (string, string Hashset.t) Hashtbl.t;
    definitions: definition list;
  }

val expression_is_value :
  expression -> bool

val types :
  t -> (variable * typ) list
val values :
  t -> value list
