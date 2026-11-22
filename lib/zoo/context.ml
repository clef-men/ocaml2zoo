module Builtin =
  Implementation_of_typedtree_builtin

let normalize name =
  if String.starts_with_uppercase name then
    name |> String.uncapitalize_ascii
  else
    name

module Scope = struct
  type t =
    { final_env: Env.t
    ; mutable idents: Ident.Set.t
    ; constructors: string Hashset.t
    ; labels: string Hashset.t
    ; modules: string Hashset.t
    ; predicates: string Hashset.t
    }

  let create ~final_env =
    { final_env= final_env
    ; idents= Ident.Set.empty
    ; constructors= Hashset.create ()
    ; labels= Hashset.create ()
    ; modules= Hashset.create ()
    ; predicates= Hashset.create ()
    }

  let final_env t =
    t.final_env

  let idents t =
    t.idents
  let add_ident t id =
    t.idents <- Ident.Set.add id t.idents

  let add_constructor t ~loc name =
    if Hashset.mem t.constructors name then
      Error.raise ~loc (Unsupported (Shadowing Constructor)) ;
    Hashset.add t.constructors name

  let add_label t ~loc name =
    if Hashset.mem t.labels name then
      Error.raise ~loc (Unsupported (Shadowing Label)) ;
    Hashset.add t.labels name

  let modules t =
    t.modules
  let add_module t name =
    assert (not @@ Hashset.mem t.modules name) ;
    Hashset.add t.modules name

  let predicates t =
    t.predicates
  let add_predicate t ~loc name =
    if Hashset.mem t.predicates name then
      Error.raise ~loc (Unsupported (Shadowing Predicate)) ;
    Hashset.add t.predicates name
end

module View = struct
  type t =
    { predicates: (string, Lpath.t * Annotation.typ Annotation.scheme) Hashtbl.t
    ; modules: (string, t) Hashtbl.t
    }

  let create () =
    { predicates= Hashtbl.create ()
    ; modules= Hashtbl.create ()
    }

  let close_scope t ~mod_ scope =
    let preds = Hashtbl.create () in
    scope |> Scope.predicates |> Hashset.iter (fun pred ->
      pred
      |> Hashtbl.find_and_remove t.predicates
      |> Option.get
      |> Hashtbl.add preds pred
    ) ;
    let mods = Hashtbl.create () in
    scope |> Scope.modules |> Hashset.iter (fun mod_ ->
      mod_
      |> Hashtbl.find_and_remove t.modules
      |> Option.get
      |> Hashtbl.add mods mod_
    ) ;
    Hashtbl.add t.modules mod_
      { predicates= preds
      ; modules= mods
      }

  let find_module_nonrec t (lid : Longident.t Location.loc) mod_ =
    match Hashtbl.find_opt t.modules mod_ with
    | None ->
        Error.raise ~loc:lid.loc (Annotation (Unbound (Module lid.txt)))
    | Some t ->
        t
  let rec find_module t (lid : Longident.t Location.loc) =
    match lid.txt with
    | Lident mod_ ->
        find_module_nonrec t lid mod_
    | Ldot (lid, mod_) ->
        let t = find_module t lid in
        find_module_nonrec t lid mod_.txt
    | Lapply _ ->
        Error.raise ~loc:lid.loc (Unsupported Functor)

  let find_predicate t (lid : Longident.t Location.loc) name =
    match Hashtbl.find_opt t.predicates name with
    | None ->
        Error.raise ~loc:lid.loc (Annotation (Unbound (Predicate lid.txt)))
    | Some info ->
        info
  let find_predicate t (lid : Longident.t Location.loc) =
    match lid.txt with
    | Lident name ->
        find_predicate t lid name
    | Ldot (lid', name) ->
        let t = find_module t lid' in
        find_predicate t lid name.txt
    | Lapply _ ->
        Error.raise ~loc:lid.loc (Unsupported Functor)
  let add_predicate t name path scheme =
    Hashtbl.add t.predicates name (path, scheme)
end

type t =
  { library: string
  ; module_: string
  (* environment at the current item *)
  ; mutable env: Env.t
  (* module scoping *)
  ; mutable path: string list
  ; scopes: Scope.t Stack.t
  ; mutable locals: Lpath.t Ident.Map.t
  ; view: View.t
  (* variables for the current item *)
  ; mutable variables: Ident.Set.t
  (* whether or not to generate a file to make declarations opaque *)
  ; mutable transparent: bool
  (* specifications *)
  ; mutable spec_return: Annotation.btyp
  ; spec_variables: (string, Annotation.typ) Hashtbl.t
  ; mutable spec_quantifiers: (string * Annotation.typ) list
  }

let create ~lib ~mod_ ~final_env =
  { library= lib
  ; module_= mod_
  ; env= Env.empty
  ; path= []
  ; scopes= Stack.of_list [Scope.create ~final_env]
  ; locals= Ident.Map.empty
  ; view= View.create ()
  ; variables= Ident.Set.empty
  ; transparent= false
  ; spec_return= Val
  ; spec_variables= Hashtbl.create ()
  ; spec_quantifiers= []
  }

let module_ t =
  t.module_

let env t =
  t.env
let set_env t env =
  t.env <- Envaux.env_of_only_summary env

let mk_lpath t name =
  Lpath.of_list_rev (name :: t.path)
let mk_gpath t path =
  Gpath.make ~lib:t.library ~mod_:t.module_ path

let current_scope t =
  Stack.top t.scopes

let final_env t =
  Scope.final_env (current_scope t)

let find_type t path =
  Env.find_type path t.env

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
    mk_lpath t name
  in
  t.locals <- Ident.Map.add id path t.locals ;
  Scope.add_ident (current_scope t) id ;
  path
let with_module t ~mod_ ~final_env fn =
  let mod_ = mod_ |> Ident.name in
  let final_env = Envaux.env_of_only_summary final_env in
  t.path <- (mod_ |> normalize) :: t.path ;
  let scope = Scope.create ~final_env in
  Stack.push scope t.scopes ;
  let res = fn () in
  Stack.drop t.scopes ;
  t.locals <- Ident.Set.fold Ident.Map.remove (Scope.idents scope) t.locals ;
  View.close_scope t.view ~mod_ scope ;
  t.path <- List.tl t.path ;
  Scope.add_module (current_scope t) mod_ ;
  res

let add_constructor t (constr : Types.constructor_declaration) =
  let name = constr.cd_id |> Ident.name in
  Scope.add_constructor (current_scope t) ~loc:constr.cd_loc name

let add_label t (lbl : Types.label_declaration) =
  let name = lbl.ld_id |> Ident.name in
  Scope.add_label (current_scope t) ~loc:lbl.ld_loc name

let mem_variable t id =
  Ident.Set.mem id t.variables
let add_variable t id =
  t.variables <- Ident.Set.add id t.variables
let begin_scope t =
  let vars = t.variables in
  fun () ->
    t.variables <- vars
let with_scope t fn =
  let vars = t.variables in
  let res = fn () in
  t.variables <- vars ;
  res

let resolve_path t ~loc path : Implementation.expression =
  match Path.to_list path with
  | None ->
      Error.raise ~loc (Unsupported Functor)
  | Some (id, names) ->
      if mem_variable t id then (
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
        let path = mk_gpath t path in
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

let find_predicate t lid =
  View.find_predicate t.view lid
let add_predicate t (name : string Location.loc) scheme =
  let path = mk_lpath t name.txt in
  Scope.add_predicate (current_scope t) ~loc:name.loc name.txt ;
  View.add_predicate t.view name.txt path scheme ;
  path

let spec t ~return fn =
  t.spec_return <- return ;
  let res = fn () in
  Hashtbl.reset t.spec_variables ;
  res

module Spec = struct
  let return t =
    t.spec_return

  let find_variable t id =
    Hashtbl.find_opt t.spec_variables id
  let add_variable t id ty =
    assert (not @@ Hashtbl.mem t.spec_variables id) ;
    Hashtbl.add t.spec_variables id ty

  let with_quantifiers t fn =
    t.spec_quantifiers <- [] ;
    let res = fn () in
    let quantifiers = t.spec_quantifiers |> List.rev in
    t.spec_quantifiers <- [] ;
    quantifiers, res
  let add_quantifier t id ty =
    if not @@ List.mem_assoc id t.spec_quantifiers then
      t.spec_quantifiers <- (id, ty) :: t.spec_quantifiers
end
