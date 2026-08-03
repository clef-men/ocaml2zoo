open Implementation

type t =
  string Hashset.t

module Builtin = struct
  let assert_ =
    "zoo_std.assert"
  let assume =
    "zoo_std.assume"
  let diverge =
    "zoo_std.diverge"
  let identifier =
    "zoo.program_logic.identifier"
  let structeq =
    "zoo.program_logic.structural_equality"
end

let of_gpath t (path : Gpath.t) =
  let dep = Printf.sprintf "%s.%s" path.library path.module_ in
  Hashset.add t dep

let of_unop _t _unop =
  ()

let of_binop t = function
  | Binop_plus | Binop_minus | Binop_mult | Binop_quot | Binop_rem
  | Binop_land | Binop_lor | Binop_lsl | Binop_lsr
  | Binop_eq | Binop_ne | Binop_le | Binop_lt | Binop_ge | Binop_gt
  | Binop_and | Binop_or ->
      ()
  | Binop_structeq | Binop_structne ->
      Hashset.add t Builtin.structeq

let of_primitive t = function
  | Assert ->
      Hashset.add t Builtin.assert_
  | Assume ->
      Hashset.add t Builtin.assume
  | Diverge ->
      Hashset.add t Builtin.diverge
  | Id ->
      Hashset.add t Builtin.identifier
  | Alloc
  | Cas
  | Faa
  | Fail
  | Tag
  | Size
  | Immediate
  | Load
  | Proph
  | Ref
  | Resolve
  | Skip
  | Store
  | Xchg ->
      ()

let rec of_expression t = function
  | Const path ->
      of_gpath t path
  | Var _ ->
      ()
  | Bool _ ->
      ()
  | Int _ ->
      ()
  | Let (_pat, expr1, expr2) ->
      of_expression t expr1 ;
      of_expression t expr2
  | Letrec (_rec_flag, _var, _bdrs, expr1, expr2) ->
      of_expression t expr1 ;
      of_expression t expr2
  | Seq (expr1, expr2) ->
      of_expression t expr1 ;
      of_expression t expr2
  | Fun (_bdrs, expr) ->
      of_expression t expr
  | If (expr1, expr2, expr3) ->
      of_expression t expr1 ;
      of_expression t expr2 ;
      Option.iter (of_expression t) expr3
  | For (_bdr, expr1, expr2, expr3) ->
      of_expression t expr1 ;
      of_expression t expr2 ;
      of_expression t expr3
  | Tuple exprs ->
      List.iter (of_expression t) exprs
  | Record exprs ->
      List.iter (of_expression t) exprs
  | Constr (_mut, path, exprs) ->
      of_gpath t path ;
      List.iter (of_expression t) exprs
  | Proj (expr, path) ->
      of_expression t expr ;
      of_gpath t path
  | Match (expr, brs, fb) ->
      of_expression t expr ;
      List.iter (of_branch t) brs ;
      Option.iter (of_fallback t) fb
  | Ref_get expr ->
      of_expression t expr
  | Ref_set (expr1, expr2) ->
      of_expression t expr1 ;
      of_expression t expr2
  | Record_get (expr, path) ->
      of_expression t expr ;
      of_gpath t path
  | Record_set (expr1, path, expr2) ->
      of_expression t expr1 ;
      of_gpath t path ;
      of_expression t expr2
  | Atomic_loc (expr, path) ->
      of_expression t expr ;
      of_gpath t path
  | Unop (unop, expr) ->
      of_unop t unop ;
      of_expression t expr
  | Binop (binop, expr1, expr2) ->
      of_binop t binop ;
      of_expression t expr1 ;
      of_expression t expr2
  | Primitive prim ->
      of_primitive t prim
  | Apply (expr, exprs) ->
      of_expression t expr ;
      List.iter (of_expression t) exprs
and of_branch t br =
  of_gpath t br.branch_tag ;
  of_expression t br.branch_expr
and of_fallback t fb =
  of_expression t fb.fallback_expr

let of_value t = function
  | Val_expr (_path, expr) ->
      of_expression t expr
  | Val_fun (_path, _bdrs, expr) ->
      of_expression t expr
  | Val_recs recs ->
      recs |> List.iter @@ fun (_path, _var, _bdrs, expr) ->
        of_expression t expr
  | Val_opaque _ ->
      ()

let of_definition t = function
  | Type _ ->
      ()
  | Val val_ ->
      of_value t val_

let of_implementation impl =
  let t = Hashset.create () in
  List.iter (of_definition t) impl.definitions ;
  Hashset.remove t "." ;
  Hashset.remove t (Printf.sprintf "%s.%s" impl.library impl.module_) ;
  t
