type binder =
  Var.t option

type rec_flag = Asttypes.rec_flag =
  | Nonrecursive
  | Recursive

type mutability =
  | Mutable
  | Immutable_nongenerative
  | Immutable_generative_weak
  | Immutable_generative_strong

type typ =
  | Type_product of string list
  | Type_record of string list
  | Type_variant of string list

type pattern =
  | Pat_var of Var.t
  | Pat_tuple of binder list
  | Pat_constr of Spath.t * binder list

type unop =
  | Unop_neg
  | Unop_minus

type binop =
  | Binop_plus | Binop_minus | Binop_mult | Binop_quot | Binop_rem
  | Binop_land | Binop_lor | Binop_lsl | Binop_lsr
  | Binop_eq | Binop_ne | Binop_le | Binop_lt | Binop_ge | Binop_gt
  | Binop_and | Binop_or
  | Binop_structeq | Binop_structne

type primitive =
  | Alloc
  | Assert
  | Assume
  | Cas
  | Diverge
  | Faa
  | Fail
  | Tag
  | Size
  | Id
  | Immediate
  | Load
  | Proph
  | Ref
  | Resolve
  | Skip
  | Store
  | Xchg

type expression =
  | Global of Spath.t
  | Local of Spath.t
  | Var of Var.t
  | Bool of bool
  | Int of int
  | Let of pattern * expression * expression
  | Letrec of rec_flag * Var.t * binder list * expression * expression
  | Seq of expression * expression
  | Fun of binder list * expression
  | If of expression * expression * expression option
  | For of binder * expression * expression * expression
  | Tuple of expression list
  | Record of expression list
  | Constr of mutability * Spath.t * expression list
  | Proj of expression * Spath.t
  | Match of expression * branch list * fallback option
  | Ref_get of expression
  | Ref_set of expression * expression
  | Record_get of expression * Spath.t
  | Record_set of expression * Spath.t * expression
  | Atomic_loc of expression * Spath.t
  | Unop of unop * expression
  | Binop of binop * expression * expression
  | Primitive of primitive
  | Apply of expression * expression list
and branch =
  { branch_tag: Spath.t
  ; branch_fields: binder list
  ; branch_as: binder
  ; branch_expr: expression
  }
and fallback =
  { fallback_as: binder
  ; fallback_expr: expression
  }

type value =
  | Val_expr of Spath.t * expression
  | Val_fun of Spath.t * binder list * expression
  | Val_recs of (Spath.t * Var.t * binder list * expression) list
  | Val_opaque of Spath.t

type definition =
  | Type of Spath.t * typ
  | Val of value

type t =
  { library: string
  ; module_: string
  ; dependencies: string Hashset.t
  ; definitions: definition list
  }

let rec expression_is_value = function
  | Global _
  | Local _
  | Bool _
  | Int _
  | Fun _ ->
      true
  | Tuple exprs
  | Constr ((Immutable_nongenerative | Immutable_generative_weak), _, exprs) ->
      List.for_all expression_is_value exprs
  | Var _
  | Let _
  | Letrec _
  | Seq _
  | If _
  | For _
  | Record _
  | Constr _
  | Proj _
  | Match _
  | Ref_get _
  | Ref_set _
  | Record_get _
  | Record_set _
  | Atomic_loc _
  | Unop _
  | Binop _
  | Primitive _
  | Apply _ ->
      false

let types str =
  str.definitions |> List.filter_map @@ function
    | Type (path, ty) ->
        Some (path, ty)
    | _ ->
        None
let values str =
  str.definitions |> List.filter_map @@ function
    | Val val_ ->
        Some val_
    | _ ->
        None
