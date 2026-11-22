open Annotation

module Parsetree =
  Annotation_parsetree

type btyp_info =
  { btyp_info_name: string
  ; btyp_info_path: Path.t option
  ; btyp_info_type: btyp
  }
let btyps =
  [|{ btyp_info_name= "unit"
    ; btyp_info_path= Some Predef.path_unit
    ; btyp_info_type= Unit
    }
  ; { btyp_info_name= "bool"
    ; btyp_info_path= Some Predef.path_bool
    ; btyp_info_type= Bool
    }
  ; { btyp_info_name= "int"
    ; btyp_info_path= Some Predef.path_int
    ; btyp_info_type= Int
    }
  ; { btyp_info_name= "val"
    ; btyp_info_path= None
    ; btyp_info_type= Val
    }
  |]
let btyp_of_string str =
  btyps |> Array.find_map @@ fun info ->
    if str = info.btyp_info_name then
      Some info.btyp_info_type
    else
      None
let btyp_of_path path =
  btyps |> Array.find_map @@ fun info ->
    match info.btyp_info_path with
    | None ->
        None
    | Some path' ->
        if Path.same path path' then
          Some info.btyp_info_type
        else
          None
let btyp_of_path path =
  path
  |> btyp_of_path
  |> Option.value ~default:Val
let rec btyp_of_type_expr ty =
  match Types.get_desc ty with
  | Tpoly (ty, _) ->
      btyp_of_type_expr ty
  | Tconstr (path, [], _memo) ->
      btyp_of_path path
  | _ ->
      Val

let rec btyps_of_type_expr acc ty =
  match Types.get_desc ty with
  | Tpoly (ty, _) ->
      btyps_of_type_expr acc ty
  | Tarrow (_lbl, ty1, ty2, _commutable) ->
      let ty1 = btyp_of_type_expr ty1 in
      btyps_of_type_expr (ty1 :: acc) ty2
  | _ ->
      let ty = btyp_of_type_expr ty in
      { scheme_parameters= List.rev acc
      ; scheme_return= ty
      }
let btyps_of_type_expr =
  btyps_of_type_expr []

let iprop =
  "iprop"
let rec transl_typ (ty : Parsetree.typ) =
  match ty.txt with
  | Type_constr (lid, tys) ->
      begin match lid.txt, tys with
      | Lident id, [] ->
          if id = iprop then
            Type_iprop
          else
            begin match btyp_of_string id with
            | Some bty ->
                Type_basic bty
            | None ->
                Error.raise ~loc:lid.loc (Annotation (Unbound (Type lid.txt)))
            end
      | _ ->
          Error.raise ~loc:lid.loc (Annotation (Unbound (Type lid.txt)))
      end
  | Type_arrow (ty1, ty2) ->
      let ty1 = transl_typ ty1 in
      let ty2 = transl_typ ty2 in
      Type_arrow (ty1, ty2)

let properties =
  [|"exclusive",
    Exclusive
  ; "persistent",
    Persistent
  |]
let transl_property name =
  properties |> Array.find_map @@ fun (name', property) ->
    if name = name' then
      Some property
    else
      None
let transl_property (property : string Location.loc) =
  match transl_property property.txt with
  | Some property ->
      property
  | None ->
      Error.raise ~loc:property.loc (Annotation (Predicate_property property.txt))

let transl_predicate ~ctx (pred : Parsetree.predicate) =
  let ty = transl_typ pred.predicate_type in
  let scheme = typ_scheme ty in
  if not @@ scheme_is_predicate scheme then
    Error.raise ~loc:pred.predicate_type.loc (Annotation Predicate_return) ;
  let path = Context.add_predicate ctx pred.predicate_name scheme in
  let properties = pred.predicate_properties |> List.map transl_property in
  { predicate_path= path
  ; predicate_type= ty
  ; predicate_properties= properties
  }

let transl_declaration ~ctx (decl : Parsetree.declaration) =
  match decl with
  | Decl_predicate pred ->
      let pred = transl_predicate ~ctx pred in
      Item_predicate pred
let transl_declarations ~ctx =
  List.map (transl_declaration ~ctx)

let check_ident ~ctx ~ty (id : string Location.loc) =
  match Context.Spec.find_variable ctx id.txt with
  | None ->
      Context.Spec.add_variable ctx id.txt ty ;
      Context.Spec.add_quantifier ctx id.txt ty
  | Some ty' ->
      if ty' <> ty then
        Error.raise ~loc:id.loc (
          Annotation (
            Typing
            { kind= Ident id.txt
            ; expected= ty
            ; inferred= ty'
            }
          )
        )

let rec transl_expr ~ctx ~ty (expr : Parsetree.expr) =
  match expr.txt with
  | Expr_int int ->
      if ty <> Type_basic Int then
        Error.raise ~loc:expr.loc (
          Annotation (
            Typing
            { kind= Expr
            ; expected= ty
            ; inferred= Type_basic Int
            }
          )
        ) ;
      Expr_int int.txt
  | Expr_ident id ->
      check_ident ~ctx ~ty id ;
      Expr_ident id.txt
  | Expr_return ->
      let ty' = Type_basic (Context.Spec.return ctx) in
      if ty' <> ty then
        Error.raise ~loc:expr.loc (
          Annotation (
            Typing
            { kind= Return
            ; expected= ty
            ; inferred= ty'
            }
          )
        ) ;
      Expr_return
  | Expr_binop (op, expr1, expr2) ->
      let expr1 = transl_expr ~ctx ~ty:(Type_basic Int) expr1 in
      let expr2 = transl_expr ~ctx ~ty:(Type_basic Int) expr2 in
      Expr_binop (op.txt, expr1, expr2)

let transl_assertion ~ctx (assertion : Parsetree.assertion) =
  match assertion.txt with
  | Assertion_comparison (cmp, expr1, expr2) ->
      let expr1 = transl_expr ~ctx ~ty:(Type_basic Int) expr1 in
      let expr2 = transl_expr ~ctx ~ty:(Type_basic Int) expr2 in
      Assertion_comparison (cmp.txt, expr1, expr2)
  | Assertion_predicate (pred, exprs) ->
      let path, scheme = Context.find_predicate ctx pred in
      let arity = scheme_arity scheme in
      let num_arg = List.length exprs in
      if arity <> num_arg then
        Error.raise ~loc:assertion.loc (
          Annotation (
            Predicate_arity
            { predicate= path
            ; arity
            ; num_argument= num_arg
            }
          )
        ) ;
      let exprs = List.map2 (fun ty expr -> transl_expr ~ctx ~ty expr) scheme.scheme_parameters exprs in
      Assertion_predicate (path, exprs)

let transl_parameter ~idents ~ty (param : Parsetree.parameter) =
  match param.txt with
  | Param_unit ->
      if ty <> Unit then
        Error.raise ~loc:param.loc (Annotation Spec_unit_parameter) ;
      Param_unit
  | Param_ident id ->
      if Hashset.mem idents id then
        Error.raise ~loc:param.loc (Annotation (Spec_duplicate_parameter id)) ;
      Hashset.add idents id ;
      Param_ident id
let transl_parameters ~tys (params : Parsetree.parameter list Location.loc) =
  let num_param = List.length params.txt in
  if List.length tys < num_param then
    Error.raise ~loc:params.loc (Annotation Spec_too_many_parameters) ;
  let params =
    let idents = Hashset.create () in
    List.map2
      (fun ty param -> transl_parameter ~idents ~ty param, ty)
      (List.take num_param tys)
      params.txt
  in
  params

let transl_spec ~ctx ~ty (spec : Parsetree.spec) =
  let params, return =
    let scheme = btyps_of_type_expr ty in
    let params = transl_parameters ~tys:scheme.scheme_parameters spec.spec_parameters in
    params, scheme.scheme_return
  in
  Context.spec ctx ~return @@ fun () ->
    params |> List.iter (fun (param, bty) ->
      match param with
      | Param_unit ->
          ()
      | Param_ident param ->
          Context.Spec.add_variable ctx param (Type_basic bty)
    ) ;
    let requires_quantifiers, requires =
      Context.Spec.with_quantifiers ctx @@ fun () ->
        spec.spec_requires |> List.map (transl_assertion ~ctx)
    in
    let ensures_quantifiers, ensures =
      Context.Spec.with_quantifiers ctx @@ fun () ->
        spec.spec_ensures |> List.map (transl_assertion ~ctx)
    in
    { spec_name= spec.spec_name |> Option.map Location.txt
    ; spec_parameters= params
    ; spec_return= return
    ; spec_requires_quantifiers= requires_quantifiers
    ; spec_requires= requires
    ; spec_ensures_quantifiers= ensures_quantifiers
    ; spec_ensures= ensures
    }
