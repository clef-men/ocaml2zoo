open Implementation

module Builtin = struct
  let raising =
    [|[|"Stdlib";"raise"|] ;
      [|"Stdlib";"invalid_arg"|] ;
      [|"Stdlib";"failwith"|] ;
    |]
  let raising =
    Array.fold_left (fun acc path ->
      Path.Set.add (Path.of_array path) acc
    ) Path.Set.empty raising

  let values =
    let helper1 expr =
      Fun ([Some "1"], expr)
    in
    let helper2 expr =
      Fun ([Some "1"; Some "2"], expr)
    in
    let helper3 expr =
      Fun ([Some "1"; Some "2"; Some "3"], expr)
    in
    [|[|"Stdlib";"ignore"|],
      Fun ([None],
        Tuple []
      )
    ; [|"Stdlib";"not"|],
      helper1 (
        Unop (Unop_neg, Var "1")
      )
    ; [|"Stdlib";"~-"|],
      helper1 (
        Unop (Unop_minus, Var "1")
      )
    ; [|"Stdlib";"+"|],
      helper2 (
        Binop (Binop_plus, Var "1", Var "2")
      )
    ; [|"Stdlib";"-"|],
      helper2 (
        Binop (Binop_minus, Var "1", Var "2")
      )
    ; [|"Stdlib";"*"|],
      helper2 (
        Binop (Binop_mult, Var "1", Var "2")
      )
    ; [|"Stdlib";"/"|],
      helper2 (
        Binop (Binop_quot, Var "1", Var "2")
      )
    ; [|"Stdlib";"mod"|],
      helper2 (
        Binop (Binop_rem, Var "1", Var "2")
      )
    ; [|"Stdlib";"land"|],
      helper2 (
        Binop (Binop_land, Var "1", Var "2")
      )
    ; [|"Stdlib";"lor"|],
      helper2 (
        Binop (Binop_lor, Var "1", Var "2")
      )
    ; [|"Stdlib";"lsl"|],
      helper2 (
        Binop (Binop_lsl, Var "1", Var "2")
      )
    ; [|"Stdlib";"lsr"|],
      helper2 (
        Binop (Binop_lsr, Var "1", Var "2")
      )
    ; [|"Stdlib";"=="|],
      helper2 (
        Binop (Binop_eq, Var "1", Var "2")
      )
    ; [|"Stdlib";"!="|],
      helper2 (
        Binop (Binop_ne, Var "1", Var "2")
      )
    ; [|"Stdlib";"<="|],
      helper2 (
        Binop (Binop_le, Var "1", Var "2")
      )
    ; [|"Stdlib";"<"|],
      helper2 (
        Binop (Binop_lt, Var "1", Var "2")
      )
    ; [|"Stdlib";">="|],
      helper2 (
        Binop (Binop_ge, Var "1", Var "2")
      )
    ; [|"Stdlib";">"|],
      helper2 (
        Binop (Binop_gt, Var "1", Var "2")
      )
    ; [|"Stdlib";"&&"|],
      helper2 (
        Binop (Binop_and, Var "1", Var "2")
      )
    ; [|"Stdlib";"||"|],
      helper2 (
        Binop (Binop_or, Var "1", Var "2")
      )
    ; [|"Stdlib";"="|],
      helper2 (
        Binop (Binop_structeq, Var "1", Var "2")
      )
    ; [|"Stdlib";"<>"|],
      helper2 (
        Binop (Binop_structne, Var "1", Var "2")
      )
    ; [|"Stdlib";"ref"|],
      helper1 (
        Apply (Primitive Ref, [Var "1"])
      )
    ; [|"Stdlib";"!"|],
      helper1 (
        Ref_get (Var "1")
      )
    ; [|"Stdlib";":="|],
      helper2 (
        Ref_set (Var "1", Var "2")
      )
    ; [|"Stdlib";"Obj";"repr"|],
      helper1 (
        Var "1"
      )
    ; [|"Stdlib";"Obj";"obj"|],
      helper1 (
        Var "1"
      )
    ; [|"Stdlib";"Obj";"magic"|],
      helper1 (
        Var "1"
      )
    ; [|"Stdlib";"Obj";"is_int"|],
      helper1 (
        Apply (Primitive Immediate, [Var "1"])
      )
    ; [|"Stdlib";"Obj";"tag"|],
      helper1 (
        Apply (Primitive Tag, [Var "1"])
      )
    ; [|"Stdlib";"Obj";"size"|],
      helper1 (
        Apply (Primitive Size, [Var "1"])
      )
    ; [|"Stdlib";"Obj";"field"|],
      helper2 (
        Apply (Primitive Load, [Var "1"; Var "2"])
      )
    ; [|"Stdlib";"Obj";"set_field"|],
      helper3 (
        Apply (Primitive Store, [Var "1"; Var "2"; Var "3"])
      )
    ; [|"Stdlib";"Obj";"new_block"|],
      helper2 (
        Apply (Primitive Alloc, [Var "1"; Var "2"])
      )
    ; [|"Stdlib";"Atomic";"Loc";"get"|],
      helper1 (
        Apply (Primitive Load, [Proj (Var "1", Gpath.Builtin._0); Proj (Var "1", Gpath.Builtin._1)])
      )
    ; [|"Stdlib";"Atomic";"Loc";"set"|],
      helper2 (
        Apply (Primitive Store, [Proj (Var "1", Gpath.Builtin._0); Proj (Var "1", Gpath.Builtin._1); Var "2"])
      )
    ; [|"Stdlib";"Atomic";"Loc";"exchange"|],
      helper2 (
        Apply (Primitive Xchg, [Var "1"; Var "2"])
      )
    ; [|"Stdlib";"Atomic";"Loc";"compare_and_set"|],
      helper3 (
        Apply (Primitive Cas, [Var "1"; Var "2"; Var "3"])
      )
    ; [|"Stdlib";"Atomic";"Loc";"fetch_and_add"|],
      helper2 (
        Apply (Primitive Faa, [Var "1"; Var "2"])
      )
    ; [|"Stdlib";"Atomic";"Loc";"decr"|],
      helper1 (
        Seq
        ( Apply (Primitive Faa, [Var "1"; Int (-1)])
        , Tuple []
        )
      )
    ; [|"Stdlib";"Atomic";"Loc";"incr"|],
      helper1 (
        Seq
        ( Apply (Primitive Faa, [Var "1"; Int 1])
        , Tuple []
        )
      )
    ; [|"Stdlib";"Atomic";"make"|],
      helper1 (
        Apply (Primitive Ref, [Var "1"])
      )
    ; [|"Stdlib";"Atomic";"get"|],
      helper1 (
        Ref_get (Var "1")
      )
    ; [|"Stdlib";"Atomic";"set"|],
      helper2 (
        Ref_set (Var "1", Var "2")
      )
    ; [|"Stdlib";"Atomic";"exchange"|],
      helper2 (
        Apply (Primitive Xchg, [Atomic_loc (Var "1", Gpath.Builtin.contents); Var "2"])
      )
    ; [|"Stdlib";"Atomic";"compare_and_set"|],
      helper3 (
        Apply (Primitive Cas, [Atomic_loc (Var "1", Gpath.Builtin.contents); Var "2"; Var "3"])
      )
    ; [|"Stdlib";"Atomic";"fetch_and_add"|],
      helper2 (
        Apply (Primitive Faa, [Atomic_loc (Var "1", Gpath.Builtin.contents); Var "2"])
      )
    ; [|"Stdlib";"Atomic";"decr"|],
      helper1 (
        Seq
        ( Apply (Primitive Faa, [Atomic_loc (Var "1", Gpath.Builtin.contents); Int (-1)])
        , Tuple []
        )
      )
    ; [|"Stdlib";"Atomic";"incr"|],
      helper1 (
        Seq
        ( Apply (Primitive Faa, [Atomic_loc (Var "1", Gpath.Builtin.contents); Int 1])
        , Tuple []
        )
      )
    ; [|"Zoo";"resolve_with"|],
      helper3 (
        Apply (Primitive Resolve, [Var "1"; Var "2"; Var "3"])
      )
    ; [|"Zoo";"resolve_silent"|],
      helper2 (
        Apply (Primitive Resolve, [Apply (Primitive Skip, []); Var "1"; Var "2"])
      )
    ; [|"Zoo";"resolve"|],
      helper2 (
        Seq
        ( Apply (Primitive Resolve, [Apply (Primitive Skip, []); Var "1"; Var "2"])
        , Var "2"
        )
      )
    |]
  let values =
    Array.fold_left (fun acc (path, expr) ->
      Path.Map.add (Path.of_array path) expr acc
    ) Path.Map.empty values
  let values =
    Path.Set.fold (fun path acc ->
      let expr = Fun ([None], Apply (Primitive Diverge, [Tuple []])) in
      Path.Map.add path expr acc
    ) raising values

  type applications =
    | Opaque of expression
    | Transparent of (expression list -> expression option)
  let applications =
    let helper1 mk_expr = function
      | [expr] ->
          Some (mk_expr expr)
      | _ ->
          None
    in
    let helper2 mk_expr = function
      | [expr1; expr2] ->
          Some (mk_expr expr1 expr2)
      | _ ->
          None
    in
    let helper3 mk_expr = function
      | [expr1; expr2; expr3] ->
          Some (mk_expr expr1 expr2 expr3)
      | _ ->
          None
    in
    [|[|"Stdlib";"ignore"|],
      helper1 (fun expr ->
        Seq (expr, Tuple [])
      )
    ; [|"Stdlib";"not"|],
      helper1 (fun expr ->
        Unop (Unop_neg, expr)
      )
    ; [|"Stdlib";"~-"|],
      helper1 (fun expr ->
        Unop (Unop_minus, expr)
      )
    ; [|"Stdlib";"+"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_plus, expr1, expr2)
      )
    ; [|"Stdlib";"-"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_minus, expr1, expr2)
      )
    ; [|"Stdlib";"*"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_mult, expr1, expr2)
      )
    ; [|"Stdlib";"/"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_quot, expr1, expr2)
      )
    ; [|"Stdlib";"mod"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_rem, expr1, expr2)
      )
    ; [|"Stdlib";"land"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_land, expr1, expr2)
      )
    ; [|"Stdlib";"lor"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_lor, expr1, expr2)
      )
    ; [|"Stdlib";"lsl"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_lsl, expr1, expr2)
      )
    ; [|"Stdlib";"lsr"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_lsr, expr1, expr2)
      )
    ; [|"Stdlib";"=="|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_eq, expr1, expr2)
      )
    ; [|"Stdlib";"!="|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_ne, expr1, expr2)
      )
    ; [|"Stdlib";"<="|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_le, expr1, expr2)
      )
    ; [|"Stdlib";"<"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_lt, expr1, expr2)
      )
    ; [|"Stdlib";">="|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_ge, expr1, expr2)
      )
    ; [|"Stdlib";">"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_gt, expr1, expr2)
      )
    ; [|"Stdlib";"&&"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_and, expr1, expr2)
      )
    ; [|"Stdlib";"||"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_or, expr1, expr2)
      )
    ; [|"Stdlib";"="|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_structeq, expr1, expr2)
      )
    ; [|"Stdlib";"<>"|],
      helper2 (fun expr1 expr2 ->
        Binop (Binop_structne, expr1, expr2)
      )
    ; [|"Stdlib";"ref"|],
      helper1 (fun expr ->
        Apply (Primitive Ref, [expr])
      )
    ; [|"Stdlib";"!"|],
      helper1 (fun expr ->
        Ref_get expr
      )
    ; [|"Stdlib";":="|],
      helper2 (fun expr1 expr2 ->
        Ref_set (expr1, expr2)
      )
    ; [|"Stdlib";"Obj";"repr"|],
      helper1 (fun expr ->
        expr
      )
    ; [|"Stdlib";"Obj";"obj"|],
      helper1 (fun expr ->
        expr
      )
    ; [|"Stdlib";"Obj";"magic"|],
      helper1 (fun expr ->
        expr
      )
    ; [|"Stdlib";"Obj";"is_int"|],
      helper1 (fun expr ->
        Apply (Primitive Immediate, [expr])
      )
    ; [|"Stdlib";"Obj";"tag"|],
      helper1 (fun expr ->
        Apply (Primitive Tag, [expr])
      )
    ; [|"Stdlib";"Obj";"size"|],
      helper1 (fun expr ->
        Apply (Primitive Size, [expr])
      )
    ; [|"Stdlib";"Obj";"field"|],
      helper2 (fun expr1 expr2 ->
        Apply (Primitive Load, [expr1; expr2])
      )
    ; [|"Stdlib";"Obj";"set_field"|],
      helper3 (fun expr1 expr2 expr3 ->
        Apply (Primitive Store, [expr1; expr2; expr3])
      )
    ; [|"Stdlib";"Obj";"new_block"|],
      helper2 (fun expr1 expr2 ->
        Apply (Primitive Alloc, [expr1; expr2])
      )
    ; [|"Stdlib";"Atomic";"Loc";"get"|],
      helper1 (fun expr ->
        Apply (Primitive Load, [Proj (expr, Gpath.Builtin._0); Proj (expr, Gpath.Builtin._1)])
      )
    ; [|"Stdlib";"Atomic";"Loc";"set"|],
      helper2 (fun expr1 expr2 ->
        Apply (Primitive Store, [Proj (expr1, Gpath.Builtin._0); Proj (expr1, Gpath.Builtin._1); expr2])
      )
    ; [|"Stdlib";"Atomic";"Loc";"exchange"|],
      helper2 (fun expr1 expr2 ->
        Apply (Primitive Xchg, [expr1; expr2])
      )
    ; [|"Stdlib";"Atomic";"Loc";"compare_and_set"|],
      helper3 (fun expr1 expr2 expr3 ->
        Apply (Primitive Cas, [expr1; expr2; expr3])
      )
    ; [|"Stdlib";"Atomic";"Loc";"fetch_and_add"|],
      helper2 (fun expr1 expr2 ->
        Apply (Primitive Faa, [expr1; expr2])
      )
    ; [|"Stdlib";"Atomic";"Loc";"decr"|],
      helper1 (fun expr ->
        Seq
        ( Apply (Primitive Faa, [expr; Int (-1)])
        , Tuple []
        )
      )
    ; [|"Stdlib";"Atomic";"Loc";"incr"|],
      helper1 (fun expr ->
        Seq
        ( Apply (Primitive Faa, [expr; Int 1])
        , Tuple []
        )
      )
    ; [|"Stdlib";"Atomic";"make"|],
      helper1 (fun expr ->
        Apply (Primitive Ref, [expr])
      )
    ; [|"Stdlib";"Atomic";"get"|],
      helper1 (fun expr ->
        Ref_get expr
      )
    ; [|"Stdlib";"Atomic";"set"|],
      helper2 (fun expr1 expr2 ->
        Ref_set (expr1, expr2)
      )
    ; [|"Stdlib";"Atomic";"exchange"|],
      helper2 (fun expr1 expr2 ->
        Apply (Primitive Xchg, [Atomic_loc (expr1, Gpath.Builtin.contents); expr2])
      )
    ; [|"Stdlib";"Atomic";"compare_and_set"|],
      helper3 (fun expr1 expr2 expr3 ->
        Apply (Primitive Cas, [Atomic_loc (expr1, Gpath.Builtin.contents); expr2; expr3])
      )
    ; [|"Stdlib";"Atomic";"fetch_and_add"|],
      helper2 (fun expr1 expr2 ->
        Apply (Primitive Faa, [Atomic_loc (expr1, Gpath.Builtin.contents); expr2])
      )
    ; [|"Stdlib";"Atomic";"decr"|],
      helper1 (fun expr ->
        Seq
        ( Apply (Primitive Faa, [Atomic_loc (expr, Gpath.Builtin.contents); Int (-1)])
        , Tuple []
        )
      )
    ; [|"Stdlib";"Atomic";"incr"|],
      helper1 (fun expr ->
        Seq
        ( Apply (Primitive Faa, [Atomic_loc (expr, Gpath.Builtin.contents); Int 1])
        , Tuple []
        )
      )
    ; [|"Zoo";"proph"|],
      helper1 (fun _expr ->
        Apply (Primitive Proph, [])
      )
    ; [|"Zoo";"resolve_with"|],
      helper3 (fun expr1 expr2 expr3 ->
        Apply (Primitive Resolve, [expr1; expr2; expr3])
      )
    ; [|"Zoo";"resolve_silent"|],
      helper2 (fun expr1 expr2 ->
        Apply (Primitive Resolve, [Apply (Primitive Skip, []); expr1; expr2])
      )
    ; [|"Zoo";"resolve"|],
      helper2 (fun expr1 expr2 ->
        Let
        ( Pat_var Var.temporary
        , expr2
        , Seq
          ( Apply (Primitive Resolve, [Apply (Primitive Skip, []); expr1; Var Var.temporary])
          , Var Var.temporary
          )
        )
      )
    ; [|"Zoo";"id"|],
      helper1 (fun _expr ->
        Apply (Primitive Id, [])
      )
    |]
  let applications =
    Array.fold_left (fun acc (path, mk_expr) ->
      Path.Map.add (Path.of_array path) (Transparent mk_expr) acc
    ) Path.Map.empty applications
  let applications =
    Path.Set.fold (fun path acc ->
      let expr = Apply (Primitive Diverge, [Tuple []]) in
      Path.Map.add path (Opaque expr) acc
    ) raising applications

  let constant_constructors =
    [|[|"()"|],
      Tuple []
    ; [|"true"|],
      Bool true
    ; [|"false"|],
      Bool false
    ;
    |]
  let constant_constructors =
    Array.fold_left (fun acc (lid, expr) ->
      Longident.Map.add (Longident.of_array lid) expr acc
    ) Longident.Map.empty constant_constructors

  let types =
    [|"list"
    ; "option"
    |]
end

module Unsupported = struct
  type t =
    | Literal_non_integer
    | Pattern_alias
    | Pattern_constant
    | Pattern_variant
    | Pattern_record
    | Pattern_array
    | Pattern_or
    | Pattern_lazy
    | Pattern_guard
    | Pattern_constr
    | Pattern_nested
    | Pattern_invalid
    | Pattern_non_trivial
    | Handler_exception
    | Expr_let_rec_non_function
    | Expr_let_mutual
    | Expr_for_downward
    | Expr_array
    | Expr_try
    | Expr_variant
    | Expr_while
    | Expr_send
    | Expr_new
    | Expr_inst_var
    | Expr_set_inst_var
    | Expr_overwrite
    | Expr_let_module
    | Expr_let_exception
    | Expr_lazy
    | Expr_object
    | Expr_pack
    | Expr_let_op
    | Expr_unreachable
    | Expr_extension
    | Argument_optional
    | Argument_omitted
    | Functor
    | Type_extensible
    | Def_recursive
    | Def_invalid
    | Def_pattern
    | Def_eval
    | Def_primitive
    | Def_exception
    | Def_module
    | Def_module_type
    | Def_class
    | Def_class_type
    | Def_include
    | Open

  let to_string = function
    | Literal_non_integer ->
        "non-integer literal"
    | Pattern_alias ->
        {|"as" pattern|}
    | Pattern_constant ->
        "constant pattern"
    | Pattern_variant ->
        "variant pattern"
    | Pattern_record ->
        "invalid record pattern"
    | Pattern_array ->
        "array pattern"
    | Pattern_or ->
        "disjunction pattern"
    | Pattern_lazy ->
        {|"lazy" pattern|}
    | Pattern_guard ->
        "guard expression"
    | Pattern_constr ->
        "invalid constructor pattern"
    | Pattern_nested ->
        "nested pattern"
    | Pattern_invalid ->
        "invalid pattern"
    | Pattern_non_trivial ->
        "non-trivial pattern in function parameter"
    | Handler_exception ->
        "exception handler"
    | Expr_let_rec_non_function ->
        "recursive binding must bind a function"
    | Expr_let_mutual ->
        "mutually recursive let-bindings"
    | Expr_for_downward ->
        {|downward "for" loop|}
    | Expr_array ->
        "array expression"
    | Expr_try ->
        {|"try" expression|}
    | Expr_variant ->
        "variant expression"
    | Expr_while ->
        {|"while" expression|}
    | Expr_send ->
        "method call"
    | Expr_new ->
        {|"new" expression|}
    | Expr_inst_var ->
        "instance variable"
    | Expr_set_inst_var ->
        "instance variable assignment"
    | Expr_overwrite ->
        "overwrite expression"
    | Expr_let_module ->
        "module binding"
    | Expr_let_exception ->
        "exception binding"
    | Expr_lazy ->
        {|"lazy" expression|}
    | Expr_object ->
        "object expression"
    | Expr_pack ->
        "module expression"
    | Expr_let_op ->
        "binding operator"
    | Expr_unreachable ->
        "unreachable branch"
    | Expr_extension ->
        "extension"
    | Argument_optional ->
        "optional function argument"
    | Argument_omitted ->
        "omitted function argument"
    | Functor ->
        "module functor"
    | Type_extensible ->
        "extensible variant"
    | Def_recursive ->
        "recursive toplevel definition must be a function"
    | Def_invalid ->
        "toplevel definition must be a constant or a function"
    | Def_pattern ->
        "toplevel definition pattern must be a variable"
    | Def_eval ->
        "evaluated expression"
    | Def_primitive ->
        "primitive definition"
    | Def_exception ->
        "exception definition"
    | Def_module ->
        "module definition"
    | Def_module_type ->
        "module type definition"
    | Def_class ->
        "class definition"
    | Def_class_type ->
        "class type definition"
    | Def_include ->
        {|"include" declaration|}
    | Open ->
        "opened module must be an identifier"

  let pp ppf t =
    Fmt.string ppf (to_string t)
end

module Error_overwrite = struct
  type t =
    | Invalid
    | Ill_typed

  let pp (kind : Attribute.overwrite_kind) ppf = function
    | Invalid ->
        Fmt.pf ppf "payload must be %s"
          begin match kind with
          | Overwrite _ ->
              "an expression"
          | Raw ->
              "of the form library.module.identifier"
          end
    | Ill_typed ->
        Fmt.pf ppf "cannot infer type"
end

module Error = struct
  type t =
    | Unsupported of Unsupported.t
    | Overwrite of Attribute.overwrite_kind * Error_overwrite.t
    | Envaux of Envaux.error

  let pp ppf = function
    | Unsupported unsupported ->
        Fmt.pf ppf "unsupported feature: %a"
          Unsupported.pp unsupported
    | Overwrite (kind, err) ->
        Fmt.pf ppf {|attribute "%s%s": %a|}
          Attribute.overwrite
          (Attribute.overwrite_kind_to_string kind)
          (Error_overwrite.pp kind) err
    | Envaux err ->
        Fmt.pf ppf "internal Envaux error: %a"
          Envaux.report_error err
end

exception Error of Location.t * Error.t

let error ~loc err =
  raise @@ Error (loc, err)
let unsupported ~loc err =
  error ~loc (Unsupported err)
let error_overwrite ~loc kind err =
  error ~loc (Overwrite (kind, err))

exception Ignore

let record_is_mutable =
  List.exists @@ fun lbl -> lbl.Types.ld_mutable = Mutable
let record_type_is_mutable ty =
  let[@warning "-8"] Types.Type_record (lbls, _) = ty.Types.type_kind in
  record_is_mutable lbls

module Context = struct
  type t =
    { library: string
    ; module_: string
    ; mutable env: Env.t
    ; final_env: Env.t
    ; mutable vars: Ident.Set.t
    }

  let create ~lib ~mod_ ~final_env =
    { library= lib
    ; module_= mod_
    ; env= Env.empty
    ; final_env
    ; vars= Ident.Set.empty
    }

  let env t =
    t.env
  let set_env t env =
    t.env <- Envaux.env_of_only_summary env

  let find_type t path =
    Env.find_type path t.env

  let mem_var t id =
    Ident.Set.mem id t.vars
  let add_var t id =
    t.vars <- Ident.Set.add id t.vars
  let save_vars t =
    let vars = t.vars in
    fun () ->
      t.vars <- vars
  let protect_vars t fn =
    let vars = t.vars in
    let res = fn () in
    t.vars <- vars ;
    res

  let normalize name =
    if String.starts_with_uppercase name then
      name |> String.uncapitalize_ascii
    else
      name
  let resolve_ident t kind id =
    let name = id |> Ident.name |> normalize in
    let name =
      let[@warning "-8"] Some idx = Env.find_index kind id t.final_env in
      if idx = 0 then
        name
      else
        name ^ Int.to_string_subscript idx
    in
    Lpath.Ident name
  let resolve_path t ~loc kind path =
    match Path.to_list path with
    | None ->
        unsupported ~loc Functor
    | Some (id, names) ->
        if mem_var t id then (
          assert (names = []) ;
          Var (Ident.name id)
        ) else if Ident.global id then (
          let id = Ident.name id in
          if Array.mem id Builtin.types then
            Global (Gpath.ident id)
          else
            let lib = id |> normalize in
            let mod_, names =
              match names with
              | [] ->
                  lib, [lib]
              | name :: names' ->
                  if String.starts_with_uppercase name then
                    name |> normalize, names' |> List.map normalize
                  else
                    lib, names |> List.map normalize
            in
            let path = names |> Lpath.of_list |> Gpath.make ~lib ~mod_ in
            Global path
        ) else (
          let path = resolve_ident t kind id in
          let path = names |> List.map normalize |> Lpath.append_list path in
          Local path
        )
  let resolve_path t ~loc kind path =
    match Path.Map.find_opt path Builtin.values with
    | Some expr ->
        expr
    | None ->
        resolve_path t ~loc kind path

  let resolve_type t ~loc ty =
    match Types.get_desc ty with
    | Tconstr (ty, _, _) ->
        ty, resolve_path t ~loc IdentType ty
    | _ ->
        assert false
  let resolve_constructor_or_label t ~loc ty name =
    let ty, typ_path = resolve_type t ~loc ty in
    let path =
      match typ_path with
      | Global typ_path ->
          let path = Lpath.set_last typ_path.path name in
          { typ_path with path }
      | Local typ_path ->
          let path = Lpath.set_last typ_path name in
          Gpath.make ~lib:t.library ~mod_:t.module_ path
      | _ ->
          assert false
    in
    ty, path
  let resolve_constructor t ~loc (constr : Data_types.constructor_description) =
    resolve_constructor_or_label t ~loc constr.cstr_res constr.cstr_name
  let resolve_label t ~loc (lbl : Data_types.label_description) =
    resolve_constructor_or_label t ~loc lbl.lbl_res lbl.lbl_name
end

let transl_open_declaration ~loc (open_ : Typedtree.open_declaration) =
  match open_.open_expr.mod_desc with
  | Tmod_ident _ ->
      ()
  | _ ->
      unsupported ~loc Open

let rec pattern_is_neutral (pat : Typedtree.pattern) =
  match pat.pat_desc with
  | Tpat_any ->
      true
  | Tpat_tuple pats ->
      List.for_all pattern_is_neutral pats
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
      Some (Ident.name id)
  | Tpat_alias (pat, id, _, _) ->
      if pattern_is_neutral pat then (
        Context.add_var ctx id ;
        Some (Ident.name id)
      ) else (
        unsupported ~loc:pat.pat_loc err
      )
  | Tpat_tuple pats ->
      if List.for_all pattern_is_neutral pats then
        None
      else
        unsupported ~loc:pat.pat_loc err
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
        unsupported ~loc:pat.pat_loc err
  | _ ->
      unsupported ~loc:pat.pat_loc err

let rec transl_pattern ~ctx (pat : Typedtree.pattern) =
  match pat.pat_desc with
  | Tpat_any ->
      None
  | Tpat_var (id, _, _) ->
      Context.add_var ctx id ;
      Some (Pat_var (Ident.name id))
  | Tpat_tuple pats ->
      let bdrs = List.map (pattern_to_binder ~ctx ~err:Pattern_nested) pats in
      Some (Pat_tuple bdrs)
  | Tpat_record ((_, { lbl_repres= Record_unboxed _; _ }, pat) :: _, _) ->
      transl_pattern ~ctx pat
  | Tpat_record (((_, lbl, _) :: _) as pats, Closed) ->
      let[@warning "-8"] Types.Tconstr (rcd, _, _) = Types.get_desc lbl.lbl_res in
      if record_type_is_mutable @@ Context.find_type ctx rcd then
        unsupported ~loc:pat.pat_loc Pattern_record ;
      let bdrs = List.map (fun (_, _, pat) -> pattern_to_binder ~ctx ~err:Pattern_nested pat) pats in
      Some (Pat_tuple bdrs)
  | Tpat_record _ ->
      unsupported ~loc:pat.pat_loc Pattern_record
  | Tpat_construct (_, { cstr_tag= Cstr_unboxed; _ }, pats, _) ->
      let[@warning "-8"] [pat] = pats in
      transl_pattern ~ctx pat
  | Tpat_construct (lid, constr, pats, _) ->
      let bdrs = List.map (pattern_to_binder ~ctx ~err:Pattern_nested) pats in
      if Longident.Map.mem lid.txt Builtin.constant_constructors then
        unsupported ~loc:lid.loc Pattern_constr ;
      let _variant, tag = Context.resolve_constructor ctx ~loc:lid.loc constr in
      Some (Pat_constr (tag, bdrs))
  | Tpat_alias _ ->
      unsupported ~loc:pat.pat_loc Pattern_alias
  | Tpat_constant _ ->
      unsupported ~loc:pat.pat_loc Pattern_constant
  | Tpat_variant _ ->
      unsupported ~loc:pat.pat_loc Pattern_variant
  | Tpat_array _ ->
      unsupported ~loc:pat.pat_loc Pattern_array
  | Tpat_or _ ->
      unsupported ~loc:pat.pat_loc Pattern_or
  | Tpat_lazy _ ->
      unsupported ~loc:pat.pat_loc Pattern_lazy

let check_argument_label ~loc (lbl : Asttypes.arg_label) =
  match lbl with
  | Nolabel
  | Labelled _ ->
      ()
  | Optional _ ->
      unsupported ~loc Argument_optional
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
      unsupported ~loc:expr.exp_loc Literal_non_integer
  | Texp_let (rec_flag, [bdg], expr2) ->
      let expr1 = transl_expression ~ctx bdg.vb_expr in
      Context.protect_vars ctx @@ fun () ->
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
                  unsupported ~loc:bdg.vb_loc Expr_let_rec_non_function ;
                let expr2 = transl_expression ~ctx expr2 in
                Let (pat, expr1, expr2)
        end
  | Texp_let (_, _, _) ->
      unsupported ~loc:expr.exp_loc Expr_let_mutual
  | Texp_function (params, body) ->
      Context.protect_vars ctx @@ fun () ->
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
            let var = Ident.name id in
            Fun (bdrs @ [Some var], Match (Var var, brs, fb))
        end
  | Texp_apply (expr', exprs) ->
      let arguments () =
        exprs |> List.map @@ fun (lbl, expr') ->
          check_argument_label ~loc:expr.exp_loc lbl ;
          match expr' with
          | None ->
              unsupported ~loc:expr.exp_loc Argument_omitted
          | Some expr' ->
              transl_expression ~ctx expr'
      in
      let default exprs =
        let expr' = transl_expression ~ctx expr' in
        Apply (expr', exprs)
      in
      begin match expr'.exp_desc with
      | Texp_ident (path', _, _) ->
          begin match Path.Map.find_opt path' Builtin.applications with
          | None ->
              default (arguments ())
          | Some (Opaque expr) ->
              expr
          | Some (Transparent mk_expr) ->
              let exprs = arguments () in
              match mk_expr exprs with
              | Some expr ->
                  expr
              | None ->
                  default exprs
          end
      | _ ->
          default (arguments ())
      end
  | Texp_ifthenelse (expr1, expr2, expr3) ->
      let expr1 = transl_expression ~ctx expr1 in
      begin match expr1, expr2.exp_desc, expr3 with
      | Unop (Unop_neg, expr1), Texp_apply ({ exp_desc= Texp_ident (path, _, _); _ }, _), None
        when Path.Set.mem path Builtin.raising ->
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
      Context.protect_vars ctx @@ fun () ->
        Context.add_var ctx id ;
        let expr3 = transl_expression ~ctx expr3 in
        For (bdr, expr1, expr2, expr3)
  | Texp_for (_, _, _, _, Downto, _) ->
      unsupported ~loc:expr.exp_loc Expr_for_downward
  | Texp_tuple exprs ->
      let exprs = List.map (transl_expression ~ctx) exprs in
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
                  Immutable_nongenerative
              | Generative ->
                  if Attribute.has_generative_strong constr.cstr_attributes then
                    Immutable_generative_strong
                  else
                    Immutable_generative_weak
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
                      Constr (Mutable, tag, exprs)
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
  | Texp_open (open_, expr) ->
      transl_open_declaration ~loc:expr.exp_loc open_ ;
      transl_expression ~ctx expr
  | Texp_array _ ->
      unsupported ~loc:expr.exp_loc Expr_array
  | Texp_try _ ->
      unsupported ~loc:expr.exp_loc Expr_try
  | Texp_variant _ ->
      unsupported ~loc:expr.exp_loc Expr_variant
  | Texp_while _ ->
      unsupported ~loc:expr.exp_loc Expr_while
  | Texp_send _ ->
      unsupported ~loc:expr.exp_loc Expr_send
  | Texp_new _ ->
      unsupported ~loc:expr.exp_loc Expr_new
  | Texp_instvar _ ->
      unsupported ~loc:expr.exp_loc Expr_inst_var
  | Texp_setinstvar _ ->
      unsupported ~loc:expr.exp_loc Expr_set_inst_var
  | Texp_override _ ->
      unsupported ~loc:expr.exp_loc Expr_overwrite
  | Texp_letmodule _ ->
      unsupported ~loc:expr.exp_loc Expr_let_module
  | Texp_letexception _ ->
      unsupported ~loc:expr.exp_loc Expr_let_exception
  | Texp_lazy _ ->
      unsupported ~loc:expr.exp_loc Expr_lazy
  | Texp_object _ ->
      unsupported ~loc:expr.exp_loc Expr_object
  | Texp_pack _ ->
      unsupported ~loc:expr.exp_loc Expr_pack
  | Texp_letop _ ->
      unsupported ~loc:expr.exp_loc Expr_let_op
  | Texp_unreachable ->
      unsupported ~loc:expr.exp_loc Expr_unreachable
  | Texp_extension_constructor _ ->
      unsupported ~loc:expr.exp_loc Expr_extension
and transl_expression_ident ~ctx ~loc path =
  Context.resolve_path ctx ~loc IdentValue path
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
        Option.iter (fun expr -> unsupported ~loc:expr.Typedtree.exp_loc Pattern_guard) br.Typedtree.c_guard ;
        let restore_vars = Context.save_vars ctx in
        let pat = br.c_lhs in
        let pat =
          match (pat.pat_desc : a Typedtree.pattern_desc) with
          | Tpat_value pat ->
              (pat :> Typedtree.(value general_pattern))
          | Tpat_exception _ ->
              unsupported ~loc:pat.pat_loc Handler_exception
          | Tpat_or _ ->
              unsupported ~loc:pat.pat_loc Pattern_or
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
          | Tpat_alias (pat, var, _, _) ->
              Context.add_var ctx var ;
              pat, Some (Ident.name var)
          | _ ->
              pat, None
        in
        let rec aux2 (pat : Typedtree.pattern) bdr =
          match pat.pat_desc with
          | Tpat_any ->
              let expr = transl_expression ~ctx br.c_rhs in
              restore_vars () ;
              acc, Some { fallback_as= bdr; fallback_expr= expr }
          | Tpat_var (id, _, _) ->
              Context.add_var ctx id ;
              let expr = transl_expression ~ctx br.c_rhs in
              restore_vars () ;
              let var = Ident.name id in
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
                unsupported ~loc:lid.loc Pattern_constr ;
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
                          let var = Ident.name id in
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
                          unsupported ~loc:pat.pat_loc Pattern_invalid ;
                        let bdrs = List.map (fun (_, _, pat) -> pattern_to_binder ~ctx ~err:Pattern_invalid pat) pats in
                        let expr = transl_expression ~ctx br.c_rhs in
                        bdrs, bdr, expr
                    | _ ->
                        unsupported ~loc:pat.pat_loc Pattern_invalid
              in
              restore_vars () ;
              let br =
                { branch_tag= tag
                ; branch_fields= bdrs
                ; branch_as= bdr
                ; branch_expr= expr
                }
              in
              aux1 (br :: acc) brs
          | _ ->
              unsupported ~loc:pat.pat_loc Pattern_invalid
        in
        aux2 pat bdr
  in
  let brs, fb = aux1 [] brs in
  List.rev brs, fb

let transl_value_binding ~ctx rec_flag bdgs (bdg : Typedtree.value_binding) path id rec_flag' expr =
  let rec_flag, rec_flag', expr =
    Context.protect_vars ctx @@ fun () ->
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
        Val_recs [path, Ident.name id, bdrs, expr]
      else
        Val_fun (path, bdrs, expr)
  | _ ->
      if rec_ then
        unsupported ~loc:bdg.vb_loc Def_recursive ;
      if expression_is_value expr then
        Val_expr (path, expr)
      else
        unsupported ~loc:bdg.vb_loc Def_invalid
let transl_value_binding ~ctx mod_ rec_flag bdgs bdg path id loc =
  match Attribute.has_overwrite bdg.Typedtree.vb_attributes with
  | None ->
      transl_value_binding ~ctx rec_flag bdgs bdg path id Nonrecursive bdg.vb_expr
  | Some (Overwrite rec_flag' as kind, attr) ->
      begin match attr.attr_payload with
      | PStr [{ pstr_desc= Pstr_eval (expr, _); _ }] ->
          let env = Context.env ctx in
          let add ~env ~loc id =
            env |> Env.add_value id
              { val_type= Ctype.newvar ()
              ; val_attributes= []
              ; val_kind= Val_reg
              ; val_loc= loc
              ; val_uid= Types.Uid.of_compilation_unit_id (Ident.create_persistent mod_)
              }
          in
          let env =
            match rec_flag, rec_flag' with
            | Recursive, _ ->
                List.fold_left (fun env (_, _, id, loc) -> add ~env ~loc id) env bdgs
            | Nonrecursive, Recursive ->
                add ~env ~loc id
            | Nonrecursive, Nonrecursive ->
                env
          in
          let expr =
            try
              Typecore.type_expression env expr
            with Typecore.Error _ ->
              error_overwrite ~loc:attr.attr_loc kind Ill_typed
          in
          transl_value_binding ~ctx rec_flag bdgs bdg path id rec_flag' expr
      | _ ->
          error_overwrite ~loc:attr.attr_loc kind Invalid
      end
  | Some (Raw, attr) ->
      begin match attr.attr_payload with
      | PStr [{ pstr_desc= Pstr_eval ({ pexp_desc= Pexp_constant { pconst_desc= Pconst_string (raw, _, _); _ }; _ }, _); _ }] ->
          begin match String.split_on_char '.' raw with
          | [lib; mod_; name] ->
              Val_expr (path, Global (Gpath.ident ~lib ~mod_ name))
          | _ ->
              error_overwrite ~loc:attr.attr_loc Raw Invalid
          end
      | _ ->
          error_overwrite ~loc:attr.attr_loc Raw Invalid
      end

let transl_value_bindings ~ctx mod_ rec_flag bdgs =
  let bdgs =
    bdgs |> List.map @@ fun (bdg : Typedtree.value_binding) ->
      match bdg.vb_pat.pat_desc with
      | Tpat_var (id, { loc; _ }, _) ->
          let path = Context.resolve_ident ctx IdentValue id in
          bdg, path, id, loc
      | _ ->
          unsupported ~loc:bdg.vb_pat.pat_loc Def_pattern
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
        transl_value_binding ~ctx mod_ rec_flag bdgs bdg path id loc
    in
    match rec_flag with
    | Nonrecursive ->
        vals
    | Recursive ->
        let recs = List.concat_map (function Val_recs recs -> recs | _ -> assert false) vals in
        [Val_recs recs]

let transl_type_declaration_record lbls =
  let is_mut = record_is_mutable lbls in
  let lbls = List.map (fun lbl -> Ident.name lbl.Types.ld_id) lbls in
  if is_mut then
    Type_record lbls
  else
    Type_product lbls
let transl_type_declaration (ty : Typedtree.type_declaration) =
  let name = ty.typ_name.txt in
  match ty.typ_type.type_kind with
  | Type_abstract _ ->
      []
  | Type_record (_, Record_unboxed _) ->
      []
  | Type_record (lbls, _) ->
      let ty = transl_type_declaration_record lbls in
      [Type (Ident name, ty)]
  | Type_variant (_, Variant_unboxed) ->
      []
  | Type_variant (constrs, _) ->
      let tags, defs =
        List.fold_right (fun (constr : Types.constructor_declaration) (tags, defs) ->
          let tag = Ident.name constr.cd_id in
          let defs =
            match constr.cd_args with
            | Cstr_record lbls ->
                let ty = transl_type_declaration_record lbls in
                let name = Printf.sprintf "%s.%s" name tag in
                Type (Ident name, ty) :: defs
            | _ ->
                defs
          in
          tag :: tags, defs
        ) constrs ([], [])
      in
      Type (Ident name, Type_variant tags) :: defs
  | Type_open ->
      unsupported ~loc:ty.typ_loc Type_extensible

let transl_structure_item ~ctx mod_ (str_item : Typedtree.structure_item) =
  match str_item.str_desc with
  | Tstr_value (rec_flag, bdgs) ->
      let vals = transl_value_bindings ~ctx mod_ rec_flag bdgs in
      List.map (fun val_ -> Val val_) vals
  | Tstr_type (_, tys) ->
      List.concat_map transl_type_declaration tys
  | Tstr_open open_ ->
      transl_open_declaration ~loc:str_item.str_loc open_ ;
      []
  | Tstr_attribute attr ->
      if Attribute.has_ignore [attr] then
        raise Ignore ;
      []
  | Tstr_eval _ ->
      unsupported ~loc:str_item.str_loc Def_eval
  | Tstr_primitive _ ->
      unsupported ~loc:str_item.str_loc Def_primitive
  | Tstr_typext _ ->
      unsupported ~loc:str_item.str_loc Type_extensible
  | Tstr_exception _ ->
      unsupported ~loc:str_item.str_loc Def_exception
  | Tstr_module _
  | Tstr_recmodule _ ->
      unsupported ~loc:str_item.str_loc Def_module
  | Tstr_modtype _ ->
      unsupported ~loc:str_item.str_loc Def_module_type
  | Tstr_class _ ->
      unsupported ~loc:str_item.str_loc Def_class
  | Tstr_class_type _ ->
      unsupported ~loc:str_item.str_loc Def_class_type
  | Tstr_include _ ->
      unsupported ~loc:str_item.str_loc Def_include
let transl_structure_item ~ctx mod_ (str_item : Typedtree.structure_item) =
  Context.set_env ctx str_item.str_env ;
  transl_structure_item ~ctx mod_ str_item

let transl_structure ~lib ~mod_ (str : Typedtree.structure) =
  let final_env =
    try
      Envaux.env_of_only_summary str.str_final_env
    with Envaux.Error err ->
      error ~loc:Location.none (Envaux err)
  in
  let ctx = Context.create ~lib ~mod_ ~final_env in
  let definitions = List.concat_map (transl_structure_item ~ctx mod_) str.str_items in
  { library= lib
  ; module_= mod_
  ; definitions
  }
