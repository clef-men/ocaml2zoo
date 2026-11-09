open Implementation

module Error =
  Implementation_of_cmt_error
module Context =
  Implementation_of_cmt_context
module Builtin =
  Implementation_of_cmt_builtin

exception Ignore

module Envaux = struct
  include Envaux

  let env_of_only_summary env =
    try
      Envaux.env_of_only_summary env
    with Error err ->
      Error.raise ~loc:Location.none (Envaux err)
end

let record_is_mutable =
  List.exists @@ fun lbl -> lbl.Types.ld_mutable = Mutable
let record_mutability lbls =
  if record_is_mutable lbls then
    Mutable
  else
    Immutable
let record_type_is_mutable ty =
  let[@warning "-8"] Types.Type_record (lbls, _) = ty.Types.type_kind in
  record_is_mutable lbls

let transl_open_declaration ~loc (open_ : Typedtree.open_declaration) =
  match open_.open_expr.mod_desc with
  | Tmod_ident _ ->
      ()
  | _ ->
      Error.raise ~loc (Unsupported Open)

let rec pattern_is_neutral (pat : Typedtree.pattern) =
  match pat.pat_desc with
  | Tpat_any ->
      true
  | Tpat_tuple pats ->
      List.for_all (fun (_, pat) -> pattern_is_neutral pat) pats
  | Tpat_record (pats, Closed) ->
      List.for_all (fun (_, _, pat) -> pattern_is_neutral pat) pats
  | Tpat_construct (_, constr, pats, _) ->
      constr.cstr_consts + constr.cstr_nonconsts = 1 &&
      List.for_all pattern_is_neutral pats
  | _ ->
      false
let rec pattern_to_binder ~ctx ~err (pat : Typedtree.pattern) =
  match pat.pat_desc with
  | Tpat_any ->
      None
  | Tpat_var (id, _, _) ->
      Context.add_var ctx id ;
      Some (id |> Ident.name)
  | Tpat_alias (pat, id, _, _, _) ->
      if pattern_is_neutral pat then (
        Context.add_var ctx id ;
        Some (id |> Ident.name)
      ) else (
        Error.raise ~loc:pat.pat_loc (Unsupported err)
      )
  | Tpat_tuple pats ->
      if List.for_all (fun (_, pat) -> pattern_is_neutral pat) pats then
        None
      else
        Error.raise ~loc:pat.pat_loc (Unsupported err)
  | Tpat_record ((_, { lbl_repres= Record_unboxed _; _ }, pat) :: _, _) ->
      pattern_to_binder ~ctx ~err pat
  | Tpat_construct (_, { cstr_tag= Cstr_unboxed; _ }, pats, _) ->
      let[@warning "-8"] [pat] = pats in
      pattern_to_binder ~ctx ~err pat
  | Tpat_construct (_, constr, pats, _) ->
      if constr.cstr_consts + constr.cstr_nonconsts = 1
      && List.for_all pattern_is_neutral pats
      then
        None
      else
        Error.raise ~loc:pat.pat_loc (Unsupported err)
  | _ ->
      Error.raise ~loc:pat.pat_loc (Unsupported err)

let rec transl_pattern ~ctx (pat : Typedtree.pattern) =
  match pat.pat_desc with
  | Tpat_any ->
      None
  | Tpat_var (id, _, _) ->
      Context.add_var ctx id ;
      Some (Pat_var (id |> Ident.name))
  | Tpat_tuple pats ->
      let bdrs = List.map (fun (_, pat) -> pattern_to_binder ~ctx ~err:Pattern_nested pat) pats in
      Some (Pat_tuple bdrs)
  | Tpat_record ((_, { lbl_repres= Record_unboxed _; _ }, pat) :: _, _) ->
      transl_pattern ~ctx pat
  | Tpat_record (((_, lbl, _) :: _) as pats, Closed) ->
      let[@warning "-8"] Types.Tconstr (rcd, _, _) = Types.get_desc lbl.lbl_res in
      if record_type_is_mutable @@ Context.find_type ctx rcd then
        Error.raise ~loc:pat.pat_loc (Unsupported Pattern_record) ;
      let bdrs = List.map (fun (_, _, pat) -> pattern_to_binder ~ctx ~err:Pattern_nested pat) pats in
      Some (Pat_tuple bdrs)
  | Tpat_record _ ->
      Error.raise ~loc:pat.pat_loc (Unsupported Pattern_record)
  | Tpat_construct (_, { cstr_tag= Cstr_unboxed; _ }, pats, _) ->
      let[@warning "-8"] [pat] = pats in
      transl_pattern ~ctx pat
  | Tpat_construct (lid, constr, pats, _) ->
      let bdrs = List.map (pattern_to_binder ~ctx ~err:Pattern_nested) pats in
      if Longident.Map.mem lid.txt Builtin.constant_constructors then
        Error.raise ~loc:lid.loc (Unsupported Pattern_constr) ;
      let _variant, tag = Context.resolve_constructor ctx ~loc:lid.loc constr in
      Some (Pat_constr (tag, bdrs))
  | Tpat_alias _ ->
      Error.raise ~loc:pat.pat_loc (Unsupported Pattern_alias)
  | Tpat_constant _ ->
      Error.raise ~loc:pat.pat_loc (Unsupported Pattern_constant)
  | Tpat_variant _ ->
      Error.raise ~loc:pat.pat_loc (Unsupported Pattern_variant)
  | Tpat_array _ ->
      Error.raise ~loc:pat.pat_loc (Unsupported Pattern_array)
  | Tpat_or _ ->
      Error.raise ~loc:pat.pat_loc (Unsupported Pattern_or)
  | Tpat_lazy _ ->
      Error.raise ~loc:pat.pat_loc (Unsupported Pattern_lazy)

let check_argument_label ~loc (lbl : Asttypes.arg_label) =
  match lbl with
  | Nolabel
  | Labelled _ ->
      ()
  | Optional _ ->
      Error.raise ~loc (Unsupported Argument_optional)
let transl_expression_field ~ctx ~loc expr (lbl : Data_types.label_description)  =
  let rcd, fld = Context.resolve_label ctx ~loc lbl in
  if record_type_is_mutable @@ Context.find_type ctx rcd then
    Record_get (expr, fld)
  else
    Proj (expr, fld)
let rec transl_expression ~ctx (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_ident (path, _, _) ->
      transl_expression_ident ~ctx ~loc:expr.exp_loc path
  | Texp_constant (Const_int int) ->
      Int int
  | Texp_constant _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Literal_non_integer)
  | Texp_let (rec_flag, [bdg], expr2) ->
      let expr1 = transl_expression ~ctx bdg.vb_expr in
      Context.with_scope ctx @@ fun () ->
        begin match transl_pattern ~ctx bdg.vb_pat with
        | None ->
            let expr2 = transl_expression ~ctx expr2 in
            Seq (expr1, expr2)
        | Some pat ->
            match expr1 with
            | Fun (bdrs, expr1) ->
                let[@warning "-8"] Pat_var var = pat in
                let expr2 = transl_expression ~ctx expr2 in
                Letrec (rec_flag, var, bdrs, expr1, expr2)
            | _ ->
                if rec_flag = Recursive then
                  Error.raise ~loc:bdg.vb_loc (Unsupported Expr_let_rec_non_function) ;
                let expr2 = transl_expression ~ctx expr2 in
                Let (pat, expr1, expr2)
        end
  | Texp_let (_, _, _) ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_let_mutual)
  | Texp_function (params, body) ->
      Context.with_scope ctx @@ fun () ->
        let bdrs =
          params |> List.map @@ fun (param : Typedtree.function_param) ->
            check_argument_label ~loc:param.fp_loc param.fp_arg_label ;
            let[@warning "-8"] Typedtree.Tparam_pat pat = param.fp_kind in
            pattern_to_binder ~ctx ~err:Pattern_non_trivial pat
        in
        begin match body with
        | Tfunction_body expr ->
            let expr = transl_expression ~ctx expr in
            Fun (bdrs, expr)
        | Tfunction_cases { cases= brs; param= id; _ } ->
            Context.add_var ctx id ;
            let brs, fb = transl_branches ~ctx brs in
            let var = id |> Ident.name in
            Fun (bdrs @ [Some var], Match (Var var, brs, fb))
        end
  | Texp_apply (expr', exprs) ->
      let args () =
        exprs |> List.map @@ fun (lbl, expr' : _ * Typedtree.apply_arg) ->
          check_argument_label ~loc:expr.exp_loc lbl ;
          match expr' with
          | Omitted () ->
              Error.raise ~loc:expr.exp_loc (Unsupported Argument_omitted)
          | Arg expr' ->
              expr'.exp_type, transl_expression ~ctx expr'
      in
      let default args =
        let expr' = transl_expression ~ctx expr' in
        let exprs = args |> List.map snd in
        Apply (expr', exprs)
      in
      begin match expr'.exp_desc with
      | Texp_ident (path', _, _) ->
          begin match Path.Map.find_opt path' Builtin.applications with
          | None ->
              default (args ())
          | Some (Raising expr) ->
              expr
          | Some (Nonraising mk_expr) ->
              let args = args () in
              match mk_expr args with
              | Some expr ->
                  expr
              | None ->
                  default args
          end
      | _ ->
          default (args ())
      end
  | Texp_ifthenelse (expr1, expr2, expr3) ->
      let expr1 = transl_expression ~ctx expr1 in
      begin match expr1, expr2.exp_desc, expr3 with
      | Unop (Unop_neg, expr1), Texp_apply ({ exp_desc= Texp_ident (path, _, _); _ }, _), None
        when Path.Set.mem path Builtin.raisings ->
          Apply (Primitive Assume, [expr1])
      | _ ->
          let expr2 = transl_expression ~ctx expr2 in
          let expr3 = Option.map (transl_expression ~ctx) expr3 in
          If (expr1, expr2, expr3)
      end
  | Texp_sequence (expr1, expr2) ->
      let expr1 = transl_expression ~ctx expr1 in
      let expr1 =
        match expr1 with
        | Seq (expr1, Tuple []) ->
            expr1
        | _ ->
            expr1
      in
      let expr2 = transl_expression ~ctx expr2 in
      Seq (expr1, expr2)
  | Texp_while (expr1, expr2) ->
      let expr1 = transl_expression ~ctx expr1 in
      let expr2 = transl_expression ~ctx expr2 in
      While (expr1, expr2)
  | Texp_for (id, pat, expr1, expr2, Upto, expr3) ->
      let bdr =
        match pat.ppat_desc with
        | Ppat_any ->
            None
        | Ppat_var { txt= var; _ } ->
            Some var
        | _ ->
            assert false
      in
      let expr1 = transl_expression ~ctx expr1 in
      let expr2 = transl_expression ~ctx expr2 in
      let expr2 =
        match expr2 with
        | Binop (Binop_minus, expr2, Int 1) ->
            expr2
        | _ ->
            Binop (Binop_plus, expr2, Int 1)
      in
      Context.with_scope ctx @@ fun () ->
        Context.add_var ctx id ;
        let expr3 = transl_expression ~ctx expr3 in
        For (bdr, expr1, expr2, expr3)
  | Texp_for (_, _, _, _, Downto, _) ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_for_downward)
  | Texp_tuple exprs ->
      let exprs = List.map (fun (_, expr) -> transl_expression ~ctx expr) exprs in
      Tuple exprs
  | Texp_record rcd ->
      transl_expression_record ~ctx ~loc:expr.exp_loc rcd.fields rcd.extended_expression (fun exprs ->
        match rcd.representation with
        | Record_unboxed _ ->
            let[@warning "-8"] [expr] = exprs in
            expr
        | _ ->
            let[@warning "-8"] Types.Tconstr (rcd, _, _) = Types.get_desc expr.exp_type in
            if record_type_is_mutable @@ Context.find_type ctx rcd then
              Record exprs
            else
              Tuple exprs
      )
  | Texp_construct (_, { cstr_tag= Cstr_unboxed; _ }, exprs) ->
      let[@warning "-8"] [expr] = exprs in
      transl_expression ~ctx expr
  | Texp_construct (lid, constr, exprs) ->
      begin match Longident.Map.find_opt lid.txt Builtin.constant_constructors with
      | Some expr ->
          expr
      | None ->
          let _variant, tag = Context.resolve_constructor ctx ~loc:lid.loc constr in
          let mk_immutable exprs =
            let flag =
              match constr.cstr_generative with
              | Nongenerative ->
                  Constr_immutable_nongenerative
              | Generative ->
                  if Attribute.has_generative_strong constr.cstr_attributes then
                    Constr_immutable_generative_strong
                  else
                    Constr_immutable_generative_weak
            in
            Constr (flag, tag, exprs)
          in
          match constr.cstr_inlined with
          | None ->
              let exprs = List.map (transl_expression ~ctx) exprs in
              mk_immutable exprs
          | Some ty ->
              let[@warning "-8"] [expr] = exprs in
              match expr.exp_desc with
              | Texp_ident (path, _, _) ->
                  transl_expression_ident ~ctx ~loc:expr.exp_loc path
              | Texp_record rcd ->
                  transl_expression_record ~ctx ~loc:expr.exp_loc rcd.fields rcd.extended_expression (fun exprs ->
                    if record_type_is_mutable ty then
                      Constr (Constr_mutable, tag, exprs)
                    else
                      mk_immutable exprs
                  )
              | _ ->
                  assert false
      end
  | Texp_match (expr, brs, _, _) ->
      let expr = transl_expression ~ctx expr in
      let brs, fb = transl_branches ~ctx brs in
      Match (expr, brs, fb)
  | Texp_atomic_loc (expr, lid, lbl) ->
      let expr = transl_expression ~ctx expr in
      let _rcd, fld = Context.resolve_label ctx ~loc:lid.loc lbl in
      Atomic_loc (expr, fld)
  | Texp_field (expr, lid, lbl) ->
      let expr = transl_expression ~ctx expr in
      transl_expression_field ~ctx ~loc:lid.loc expr lbl
  | Texp_setfield (expr1, lid, lbl, expr2) ->
      let expr1 = transl_expression ~ctx expr1 in
      let _rcd, fld = Context.resolve_label ctx ~loc:lid.loc lbl in
      let expr2 = transl_expression ~ctx expr2 in
      Record_set (expr1, fld, expr2)
  | Texp_assert ({ exp_desc= Texp_construct (_, { cstr_name= "false"; _ }, _); _ }, _) ->
      Apply (Primitive Fail, [])
  | Texp_assert (expr, _) ->
      let expr = transl_expression ~ctx expr in
      Apply (Primitive Assert, [expr])
  | Texp_struct_item ({ str_desc= Tstr_open open_; _ }, expr) ->
      transl_open_declaration ~loc:expr.exp_loc open_ ;
      transl_expression ~ctx expr
  | Texp_array _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_array)
  | Texp_try _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_try)
  | Texp_variant _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_variant)
  | Texp_send _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_send)
  | Texp_new _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_new)
  | Texp_instvar _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_inst_var)
  | Texp_setinstvar _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_set_inst_var)
  | Texp_override _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_overwrite)
  | Texp_lazy _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_lazy)
  | Texp_object _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_object)
  | Texp_pack _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_pack)
  | Texp_letop _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_let_op)
  | Texp_unreachable ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_unreachable)
  | Texp_extension_constructor _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_extension)
  | Texp_struct_item _ ->
      Error.raise ~loc:expr.exp_loc (Unsupported Expr_struct_item)
and transl_expression_ident ~ctx ~loc path =
  Context.resolve_path_value ctx ~loc path
and transl_expression_record ~ctx ~loc flds ext_expr mk_expr =
  let ext_expr =
    match ext_expr with
    | None ->
        Either.Left Var.temporary
    | Some ext_expr ->
        match transl_expression ~ctx ext_expr with
        | Var var ->
            Left var
        | ext_expr ->
            Right ext_expr
  in
  let exprs =
    Array.fold_right (fun (lbl, def) acc ->
      let expr =
        match def with
        | Typedtree.Kept _ ->
            transl_expression_field ~ctx ~loc (Var (Either.get_left ~right:Var.temporary ext_expr)) lbl
        | Overridden (_, expr) ->
            transl_expression ~ctx expr
      in
      expr :: acc
    ) flds []
  in
  let expr = mk_expr exprs in
  match ext_expr with
  | Left _ ->
      expr
  | Right ext_expr ->
      Let (Pat_var Var.temporary, ext_expr, expr)

and transl_branches : type a. ctx:Context.t -> a Typedtree.case list -> branch list * fallback option = fun ~ctx brs ->
  let rec aux1 acc = function
    | [] ->
        acc, None
    | br :: brs ->
        br.Typedtree.c_guard |> Option.iter (fun expr ->
          Error.raise ~loc:expr.Typedtree.exp_loc (Unsupported Pattern_guard)
        ) ;
        let end_scope = Context.begin_scope ctx in
        let pat = br.c_lhs in
        let pat =
          match (pat.pat_desc : a Typedtree.pattern_desc) with
          | Tpat_value pat ->
              (pat :> Typedtree.(value general_pattern))
          | Tpat_exception _ ->
              Error.raise ~loc:pat.pat_loc (Unsupported Handler_exception)
          | Tpat_or _ ->
              Error.raise ~loc:pat.pat_loc (Unsupported Pattern_or)
          | Tpat_any ->
              pat
          | Tpat_var _ ->
              pat
          | Tpat_alias _ ->
              pat
          | Tpat_constant _ ->
              pat
          | Tpat_tuple _ ->
              pat
          | Tpat_construct _ ->
              pat
          | Tpat_variant _ ->
              pat
          | Tpat_record _ ->
              pat
          | Tpat_array _ ->
              pat
          | Tpat_lazy _ ->
              pat
        in
        let pat, bdr =
          match pat.pat_desc with
          | Tpat_alias (pat, id, _, _, _) ->
              Context.add_var ctx id ;
              pat, Some (id |> Ident.name)
          | _ ->
              pat, None
        in
        let rec aux2 (pat : Typedtree.pattern) bdr =
          match pat.pat_desc with
          | Tpat_any ->
              let expr = transl_expression ~ctx br.c_rhs in
              end_scope () ;
              acc, Some { fallback_as= bdr; fallback_expr= expr }
          | Tpat_var (id, _, _) ->
              Context.add_var ctx id ;
              let expr = transl_expression ~ctx br.c_rhs in
              end_scope () ;
              let var = id |> Ident.name in
              begin match bdr with
              | None ->
                  acc, Some { fallback_as= Some var; fallback_expr= expr }
              | Some var' ->
                  acc, Some { fallback_as= bdr; fallback_expr= Let (Pat_var var, Var var', expr) }
              end
          | Tpat_record ((_, { lbl_repres= Record_unboxed _; _ }, pat) :: _, _) ->
              aux2 pat bdr
          | Tpat_construct (_, { cstr_tag= Cstr_unboxed; _ }, pats, _) ->
              let[@warning "-8"] [pat] = pats in
              aux2 pat bdr
          | Tpat_construct (lid, constr, pats, _) ->
              if Longident.Map.mem lid.txt Builtin.constant_constructors then
                Error.raise ~loc:lid.loc (Unsupported Pattern_constr) ;
              let _variant, tag = Context.resolve_constructor ctx ~loc:lid.loc constr in
              let bdrs, bdr, expr =
                match constr.cstr_inlined with
                | None ->
                    let bdrs = List.map (pattern_to_binder ~ctx ~err:Pattern_invalid) pats in
                    let bdrs =
                      match bdrs with
                      | [None] ->
                          List.make constr.cstr_arity None
                      | _ ->
                          bdrs
                    in
                    let expr = transl_expression ~ctx br.c_rhs in
                    bdrs, bdr, expr
                | Some ty ->
                    let[@warning "-8"] [pat] = pats in
                    match pat.pat_desc with
                    | Tpat_any ->
                        let expr = transl_expression ~ctx br.c_rhs in
                        let[@warning "-8"] Types.Type_record (lbls, _) = ty.type_kind in
                        let bdrs = List.make (List.length lbls) None in
                        bdrs, bdr, expr
                    | Tpat_var (id, _, _) ->
                        Context.add_var ctx id ;
                        let expr = transl_expression ~ctx br.c_rhs in
                        let bdr, expr =
                          let var = id |> Ident.name in
                          match bdr with
                          | None ->
                              Some var, expr
                          | Some var' ->
                              bdr, Let (Pat_var var, Var var', expr)
                        in
                        let[@warning "-8"] Types.Type_record (lbls, _) = ty.type_kind in
                        let bdrs = List.make (List.length lbls) None in
                        bdrs, bdr, expr
                    | Tpat_record (pats, Closed) ->
                        if record_type_is_mutable ty then
                          Error.raise ~loc:pat.pat_loc (Unsupported Pattern_invalid) ;
                        let bdrs = List.map (fun (_, _, pat) -> pattern_to_binder ~ctx ~err:Pattern_invalid pat) pats in
                        let expr = transl_expression ~ctx br.c_rhs in
                        bdrs, bdr, expr
                    | _ ->
                        Error.raise ~loc:pat.pat_loc (Unsupported Pattern_invalid)
              in
              end_scope () ;
              let br =
                { branch_tag= tag
                ; branch_fields= bdrs
                ; branch_as= bdr
                ; branch_expr= expr
                }
              in
              aux1 (br :: acc) brs
          | _ ->
              Error.raise ~loc:pat.pat_loc (Unsupported Pattern_invalid)
        in
        aux2 pat bdr
  in
  let brs, fb = aux1 [] brs in
  List.rev brs, fb

let transl_value_binding ~ctx rec_flag bdgs (bdg : Typedtree.value_binding) path id rec_flag' expr =
  let rec_flag, rec_flag', expr =
    Context.with_scope ctx @@ fun () ->
      begin match rec_flag, rec_flag' with
      | Recursive, _ ->
          List.iter (fun (_, _, id, _) -> Context.add_var ctx id) bdgs
      | Nonrecursive, Recursive ->
          Context.add_var ctx id
      | Nonrecursive, Nonrecursive ->
          ()
      end ;
      let expr = transl_expression ~ctx expr in
      rec_flag, rec_flag', expr
  in
  let rec_ = rec_flag = Recursive || rec_flag' = Recursive in
  match expr with
  | Fun (bdrs, expr) ->
      if rec_ then
        Val_recs [path, id |> Ident.name, bdrs, expr]
      else
        Val_fun (path, bdrs, expr)
  | _ ->
      if rec_ then
        Error.raise ~loc:bdg.vb_loc (Unsupported Decl_recursive) ;
      if expression_is_value expr then
        Val_expr (path, expr)
      else
        Error.raise ~loc:bdg.vb_loc (Unsupported Decl_invalid)
let transl_value_binding ~ctx rec_flag bdgs bdg path id loc =
  match Attribute.has_overwrite bdg.Typedtree.vb_attributes with
  | None ->
      transl_value_binding ~ctx rec_flag bdgs bdg path id Nonrecursive bdg.vb_expr
  | Some (Overwrite rec_flag' as kind, attr) ->
      let expr =
        match attr.attr_payload with
        | PStr [{ pstr_desc= Pstr_eval (expr, _); _ }] ->
            expr
        | _ ->
            Error.raise ~loc:attr.attr_loc (Overwrite (kind, Invalid))
      in
      let env = Context.env ctx in
      Ctype.with_level ~level:(Env.max_scope env) @@ fun () ->
        let env =
          let add ~env ~loc id =
            let uid =
              Context.module_ ctx
              |> Ident.create_persistent
              |> Types.Uid.of_compilation_unit_id
            in
            env |> Env.add_value id
              { val_type= Ctype.newvar ()
              ; val_attributes= []
              ; val_kind= Val_reg
              ; val_loc= loc
              ; val_uid= uid
              }
          in
          match rec_flag, rec_flag' with
          | Recursive, _ ->
              bdgs |> List.fold_left (fun env (_, _, id, loc) -> add ~env ~loc id) env
          | Nonrecursive, Recursive ->
              add ~env ~loc id
          | Nonrecursive, Nonrecursive ->
              env
        in
        let expr =
          try
            Typecore.type_expression env expr
          with
          | Typecore.Error.In_context _
          | Env.Error.In_context _ ->
              Error.raise ~loc:attr.attr_loc (Overwrite (kind, Typing))
        in
        transl_value_binding ~ctx rec_flag bdgs bdg path id rec_flag' expr
  | Some (Raw, attr) ->
      let raw =
        match attr.attr_payload with
        | PStr [{ pstr_desc= Pstr_eval ({ pexp_desc= Pexp_constant { pconst_desc= Pconst_string (raw, _, _); _ }; _ }, _); _ }] ->
            raw
        | _ ->
            Error.raise ~loc:attr.attr_loc (Overwrite (Raw, Invalid))
      in
      match String.split_on_char '.' raw with
      | [lib; mod_; name] ->
          Val_expr (path, Const (Gpath.ident ~lib ~mod_ name))
      | _ ->
          Error.raise ~loc:attr.attr_loc (Overwrite (Raw, Invalid))

let transl_value_bindings ~ctx rec_flag bdgs =
  let bdgs =
    bdgs |> List.map @@ fun (bdg : Typedtree.value_binding) ->
      match bdg.vb_pat.pat_desc with
      | Tpat_var (id, { loc; _ }, _) ->
          let path = Context.add_local ctx Ident_value id in
          bdg, path, id, loc
      | _ ->
          Error.raise ~loc:bdg.vb_pat.pat_loc (Unsupported Decl_pattern)
  in
  let[@warning "-8"] (bdg, _, _, _) :: _ = bdgs in
  if Attribute.has_ignore bdg.vb_attributes then
    []
  else if Attribute.has_opaque bdg.vb_attributes then
    bdgs |> List.map @@ fun (_, path, _, _) ->
      Val_opaque path
  else
    let vals =
      bdgs |> List.map @@ fun (bdg, path, id, loc) ->
        transl_value_binding ~ctx rec_flag bdgs bdg path id loc
    in
    match rec_flag with
    | Nonrecursive ->
        vals
    | Recursive ->
        let recs = List.concat_map (function Val_recs recs -> recs | _ -> assert false) vals in
        [Val_recs recs]

let transl_type_declaration_record ~ctx lbls =
  let mut = record_mutability lbls in
  let lbls =
    lbls |> List.map @@ fun lbl ->
      Context.add_label ctx lbl ;
      lbl.Types.ld_id |> Ident.name
  in
  Type_record (mut, lbls)
let transl_type_declaration ~ctx (ty : Typedtree.type_declaration) =
  let path = Context.add_local ctx Ident_type ty.typ_id in
  match ty.typ_type.type_kind with
  | Type_abstract _ ->
      []
  | Type_record (_, Record_unboxed _) ->
      []
  | Type_record (lbls, _) ->
      let ty = transl_type_declaration_record ~ctx lbls in
      [Type (path, Type_normal, ty)]
  | Type_variant (_, Variant_unboxed) ->
      []
  | Type_variant (constrs, _) ->
      let constrs, decls =
        List.fold_right (fun (constr : Types.constructor_declaration) (constrs, decls) ->
          Context.add_constructor ctx constr ;
          let tag = constr.cd_id |> Ident.name in
          let const, decls =
            match constr.cd_args with
            | Cstr_tuple tys ->
                let const = if List.length tys = 0 then Constant else Nonconstant in
                const, decls
            | Cstr_record lbls ->
                let ty = transl_type_declaration_record ~ctx lbls in
                let decls = Type (path, Type_inline tag, ty) :: decls in
                Nonconstant, decls
          in
          let constr =
            { constructor_tag= tag
            ; constructor_constancy= const
            }
          in
          constr :: constrs, decls
        ) constrs ([], [])
      in
      let decl = Type (path, Type_normal, Type_variant constrs) in
      decl :: decls
  | Type_open ->
      Error.raise ~loc:ty.typ_loc (Unsupported Type_extensible)
  | Type_external _ ->
      Error.raise ~loc:ty.typ_loc (Unsupported Type_external)

let rec transl_module_expr ~ctx ~mod_ (mexpr : Typedtree.module_expr) =
  match mexpr.mod_desc with
  | Tmod_structure str ->
      transl_structure ~ctx ~mod_ str
  | Tmod_constraint (mexpr, _ty, _constraint, _coerc) ->
      transl_module_expr ~ctx ~mod_ mexpr
  | Tmod_ident _ ->
      Error.raise ~loc:mexpr.mod_loc (Unsupported Decl_module_alias)
  | Tmod_functor _
  | Tmod_apply _
  | Tmod_apply_unit _ ->
      Error.raise ~loc:mexpr.mod_loc (Unsupported Functor)
  | Tmod_unpack _ ->
      Error.raise ~loc:mexpr.mod_loc (Unsupported Module_packed)

and transl_module_binding ~ctx (mbdg : Typedtree.module_binding) =
  match mbdg.mb_id with
  | None ->
      Error.raise ~loc:mbdg.mb_loc (Unsupported Decl_module_unnamed)
  | Some mod_ ->
      let _path = Context.add_local ctx Ident_module mod_ in
      transl_module_expr ~ctx ~mod_ mbdg.mb_expr

and transl_structure_item ~ctx (str_item : Typedtree.structure_item) =
  Context.set_env ctx str_item.str_env ;
  match str_item.str_desc with
  | Tstr_value (rec_flag, bdgs) ->
      let vals = transl_value_bindings ~ctx rec_flag bdgs in
      List.map (fun val_ -> Val val_) vals
  | Tstr_type (_, tys) ->
      List.concat_map (transl_type_declaration ~ctx) tys
  | Tstr_open open_ ->
      transl_open_declaration ~loc:str_item.str_loc open_ ;
      []
  | Tstr_attribute attr ->
      if Attribute.has_ignore [attr] then
        raise Ignore ;
      if Attribute.has_transparent [attr] then
        Context.set_transparent ctx ;
      []
  | Tstr_module mbdg ->
      transl_module_binding ~ctx mbdg
  | Tstr_eval _ ->
      Error.raise ~loc:str_item.str_loc (Unsupported Decl_eval)
  | Tstr_primitive _ ->
      Error.raise ~loc:str_item.str_loc (Unsupported Decl_external)
  | Tstr_typext _ ->
      Error.raise ~loc:str_item.str_loc (Unsupported Type_extensible)
  | Tstr_exception _ ->
      Error.raise ~loc:str_item.str_loc (Unsupported Decl_exception)
  | Tstr_recmodule _ ->
      Error.raise ~loc:str_item.str_loc (Unsupported Decl_module_rec)
  | Tstr_modtype _ ->
      Error.raise ~loc:str_item.str_loc (Unsupported Decl_module_type)
  | Tstr_class _ ->
      Error.raise ~loc:str_item.str_loc (Unsupported Decl_class)
  | Tstr_class_type _ ->
      Error.raise ~loc:str_item.str_loc (Unsupported Decl_class_type)
  | Tstr_include _ ->
      Error.raise ~loc:str_item.str_loc (Unsupported Decl_include)

and transl_structure' ~ctx (str : Typedtree.structure) =
  List.concat_map (transl_structure_item ~ctx) str.str_items
and transl_structure ~ctx ?mod_ str =
  match mod_ with
  | None ->
      transl_structure' ~ctx str
  | Some mod_ ->
      let final_env = str.str_final_env in
      Context.with_module ctx ~mod_ ~final_env @@ fun () ->
        transl_structure' ~ctx str

let transl ~lib ~mod_ (str : Typedtree.structure) =
  let final_env = Envaux.env_of_only_summary str.str_final_env in
  let ctx = Context.create ~lib ~mod_ ~final_env in
  let decls = transl_structure ~ctx str in
  { library= lib
  ; module_= mod_
  ; declarations= decls
  ; transparent= Context.transparent ctx
  }
