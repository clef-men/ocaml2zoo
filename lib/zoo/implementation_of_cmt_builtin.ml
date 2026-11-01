open Implementation

let raisings =
  [|[|"Stdlib";"raise"|]
  ; [|"Stdlib";"invalid_arg"|]
  ; [|"Stdlib";"failwith"|]
  |]
let raisings =
  Array.fold_left (fun acc path ->
    Path.Set.add (Path.of_array path) acc
  ) Path.Set.empty raisings

type nonraising =
  | Nonraising_untyped of (expression list -> expression option) * expression option
  | Nonraising_typed of ((Types.type_expr * expression) list -> expression option)
let nonraisings =
  let helper1 mk_expr =
    ( 1
    , Nonraising_untyped
      ( ( function
            | [expr] ->
                Some (mk_expr expr)
            | _ ->
                None
        )
      , None
      )
    )
  in
  let helper2 mk_expr =
    ( 2
    , Nonraising_untyped
      ( ( function
            | [expr1; expr2] ->
                Some (mk_expr expr1 expr2)
            | _ ->
                None
        )
      , None
      )
    )
  in
  let helper3 mk_expr =
    ( 3
    , Nonraising_untyped
      ( ( function
            | [expr1; expr2; expr3] ->
                Some (mk_expr expr1 expr2 expr3)
            | _ ->
                None
        )
      , None
      )
    )
  in
  [|[|"Stdlib";"ignore"|],
    ( 1
    , Nonraising_untyped
      ( ( function
            | [expr] ->
                Some (Seq (expr, Tuple []))
            | _ ->
                None
        )
      , Some (Fun ([None], Tuple []))
      )
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
    ( 2
    , Nonraising_typed (
        function
          | [ty1, expr1; _ty2, expr2] ->
              if Types.is_int ty1 then
                Some (Binop (Binop_le, expr1, expr2))
              else
                None
          | _ ->
              None
      )
    )
  ; [|"Stdlib";"<"|],
    ( 2
    , Nonraising_typed (
        function
          | [ty1, expr1; _ty2, expr2] ->
              if Types.is_int ty1 then
                Some (Binop (Binop_lt, expr1, expr2))
              else
                None
          | _ ->
              None
      )
    )
  ; [|"Stdlib";">="|],
    ( 2
    , Nonraising_typed (
        function
          | [ty1, expr1; _ty2, expr2] ->
              if Types.is_int ty1 then
                Some (Binop (Binop_ge, expr1, expr2))
              else
                None
          | _ ->
              None
      )
    )
  ; [|"Stdlib";">"|],
    ( 2
    , Nonraising_typed (
        function
          | [ty1, expr1; _ty2, expr2] ->
              if Types.is_int ty1 then
                Some (Binop (Binop_gt, expr1, expr2))
              else
                None
          | _ ->
              None
      )
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
let nonraisings =
  nonraisings |> Array.map @@ fun (path, spec) ->
    Path.of_array path, spec

type application =
  | Raising of expression
  | Nonraising of ((Types.type_expr * expression) list -> expression option)
let applications =
  Path.Map.empty
let applications =
  Path.Set.fold (fun path acc ->
    let expr = Apply (Primitive Diverge, [Tuple []]) in
    Path.Map.add path (Raising expr) acc
  ) raisings applications
let applications =
  Array.fold_left (fun acc (path, (_arity, nonraising)) ->
    let mk_expr =
      match nonraising with
      | Nonraising_untyped (mk_expr, _) ->
          fun args ->
            args |> List.map snd |> mk_expr
      | Nonraising_typed mk_expr ->
          mk_expr
    in
    Path.Map.add path (Nonraising mk_expr) acc
  ) applications nonraisings

let values =
  Path.Map.empty
let values =
  Path.Set.fold (fun path acc ->
    let expr = Fun ([None], Apply (Primitive Diverge, [Tuple []])) in
    Path.Map.add path expr acc
  ) raisings values
let values =
  Array.fold_left (fun acc (path, (arity, nonraising)) ->
    match nonraising with
    | Nonraising_untyped (mk_expr, expr) ->
        let expr =
          match expr with
          | Some expr ->
              expr
          | None ->
              let expr = mk_expr @@ List.init arity (fun i -> Var (Var.of_int (i + 1))) in
              let expr = expr |> Option.get in
              let expr = Fun (List.init arity (fun i -> Some (Var.of_int (i + 1))), expr) in
              expr
        in
        Path.Map.add path expr acc
    | Nonraising_typed _ ->
        acc
  ) values nonraisings

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
