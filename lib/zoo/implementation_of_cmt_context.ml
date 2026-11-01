module Error =
  Implementation_of_cmt_error
module Builtin =
  Implementation_of_cmt_builtin

type scope =
  { scope_ids: Ident.Set.t
  ; scope_constructors: string Hashset.t
  ; scope_labels: string Hashset.t
  ; scope_final_env: Env.t
  }

type t =
  { library: string
  ; module_: string
  (* environment at the current item *)
  ; mutable env: Env.t
  (* module scoping *)
  ; mutable path: string list
  ; mutable locals: Lpath.t Ident.Map.t
  ; scopes: scope Stack.t
  (* variables for the current item *)
  ; mutable vars: Ident.Set.t
  (* whether or not to generate a file to make declarations opaque *)
  ; mutable transparent: bool
  }

let create ~lib ~mod_ ~final_env =
  let scope =
    { scope_ids= Ident.Set.empty
    ; scope_constructors= Hashset.create ()
    ; scope_labels= Hashset.create ()
    ; scope_final_env= final_env
    }
  in
  { library= lib
  ; module_= mod_
  ; env= Env.empty
  ; path= []
  ; locals= Ident.Map.empty
  ; scopes= Stack.of_list [scope]
  ; vars= Ident.Set.empty
  ; transparent= false
  }

let module_ t =
  t.module_

let env t =
  t.env
let set_env t env =
  t.env <- Envaux.env_of_only_summary env

let final_env t =
  let scope = Stack.top t.scopes in
  scope.scope_final_env

let find_type t path =
  Env.find_type path t.env

let normalize name =
  if String.starts_with_uppercase name then
    name |> String.uncapitalize_ascii
  else
    name

let find_local t id =
  Ident.Map.find id t.locals
let add_local t kind id =
  let path =
    let name = id |> Ident.name |> normalize in
    let name =
      let[@warning "-8"] Some idx = Env.find_index kind id (final_env t) in
      if idx = 0 then
        name
      else
        name ^ Int.to_string_subscript idx
    in
    Lpath.of_list_rev (name :: t.path)
  in
  t.locals <- Ident.Map.add id path t.locals ;
  let scope = Stack.pop t.scopes in
  let ids = Ident.Set.add id scope.scope_ids in
  Stack.push { scope with scope_ids= ids } t.scopes ;
  path
let with_module t ~mod_ ~final_env fn =
  let mod_ = mod_ |> Ident.name |> normalize in
  let final_env = Envaux.env_of_only_summary final_env in
  t.path <- mod_ :: t.path ;
  let scope =
    { scope_ids= Ident.Set.empty
    ; scope_constructors= Hashset.create ()
    ; scope_labels= Hashset.create ()
    ; scope_final_env= final_env
    }
  in
  Stack.push scope t.scopes ;
  let res = fn () in
  let scope = Stack.pop t.scopes in
  t.locals <- Ident.Set.fold Ident.Map.remove scope.scope_ids t.locals ;
  t.path <- List.tl t.path ;
  res

let add_constructor t (constr : Types.constructor_declaration) =
  let name = constr.cd_id |> Ident.name in
  let scope = Stack.top t.scopes in
  if Hashset.mem scope.scope_constructors name then
    Error.raise_unsupported ~loc:constr.cd_loc (Shadowing Constructor) ;
  Hashset.add scope.scope_constructors name

let add_label t (lbl : Types.label_declaration) =
  let name = lbl.ld_id |> Ident.name in
  let scope = Stack.top t.scopes in
  if Hashset.mem scope.scope_labels name then
    Error.raise_unsupported ~loc:lbl.ld_loc (Shadowing Label) ;
  Hashset.add scope.scope_labels name

let mem_var t id =
  Ident.Set.mem id t.vars
let add_var t id =
  t.vars <- Ident.Set.add id t.vars
let begin_scope t =
  let vars = t.vars in
  fun () ->
    t.vars <- vars
let with_scope t fn =
  let vars = t.vars in
  let res = fn () in
  t.vars <- vars ;
  res

let resolve_path t ~loc path : Implementation.expression =
  match Path.to_list path with
  | None ->
      Error.raise_unsupported ~loc Functor
  | Some (id, names) ->
      if mem_var t id then (
        assert (names = []) ;
        Var (id |> Ident.name)
      ) else if Ident.global id then (
        let id = id |> Ident.name in
        if Array.mem id Builtin.types then
          Const (Gpath.ident id)
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
          Const path
      ) else (
        let path = find_local t id in
        let path = names |> List.map normalize |> Lpath.append_list path in
        let path = Gpath.make ~lib:t.library ~mod_:t.module_ path in
        Const path
      )
let resolve_path_type =
  resolve_path
let resolve_path_value t ~loc path =
  match Path.Map.find_opt path Builtin.values with
  | Some expr ->
      expr
  | None ->
      resolve_path t ~loc path

let resolve_type t ~loc ty =
  match Types.get_desc ty with
  | Tconstr (ty, _, _) ->
      ty, resolve_path_type t ~loc ty
  | _ ->
      assert false
let resolve_constructor_or_label t ~loc ty name =
  let ty, typ_path = resolve_type t ~loc ty in
  let path =
    match typ_path with
    | Const typ_path ->
        let path = Lpath.set_last typ_path.path name in
        { typ_path with path }
    | _ ->
        assert false
  in
  ty, path
let resolve_constructor t ~loc (constr : Data_types.constructor_description) =
  resolve_constructor_or_label t ~loc constr.cstr_res constr.cstr_name
let resolve_label t ~loc (lbl : Data_types.label_description) =
  resolve_constructor_or_label t ~loc lbl.lbl_res lbl.lbl_name

let transparent t =
  t.transparent
let set_transparent t =
  t.transparent <- true
