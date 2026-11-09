include Ocaml_common.Env

type ident_kind =
  | Ident_value
  | Ident_type
  | Ident_module
  | Ident_modtype
  | Ident_class
  | Ident_cltype

let find_index kind =
  match kind with
  | Ident_value ->
      find_value_index
  | Ident_type ->
      find_type_index
  | Ident_module ->
      find_module_index
  | Ident_modtype ->
      find_modtype_index
  | Ident_class ->
      find_class_index
  | Ident_cltype ->
      find_cltype_index

let max_scope =
  let normalize scope =
    if scope < Btype.generic_level then
      scope
    else
      Btype.lowest_level
  in
  let rec max_scope acc = function
    | Env_empty ->
        acc
    | Env_value (summary, id, _)
    | Env_type (summary, id, _)
    | Env_extension (summary, id, _)
    | Env_module (summary, id, _, _)
    | Env_modtype (summary, id, _)
    | Env_class (summary, id, _)
    | Env_cltype (summary, id, _)
    | Env_not_aliasable (summary, id)
    | Env_persistent (summary, id) ->
        max_scope (max acc (normalize (Ident.scope id))) summary
    | Env_open (summary, path) ->
        max_scope (max acc (normalize (Path.scope path))) summary
    | Env_constraints (summary, _)
    | Env_copy_types summary
    | Env_value_unbound (summary, _, _)
    | Env_module_unbound (summary, _, _) ->
        max_scope acc summary
  in
  max_scope
let max_scope t =
  max_scope Btype.lowest_level (summary t)
