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
