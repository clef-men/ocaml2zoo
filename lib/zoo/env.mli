include module type of struct
  include Ocaml_common.Env
end

type ident_kind =
  | Ident_value
  | Ident_type
  | Ident_module
  | Ident_modtype
  | Ident_class
  | Ident_cltype

val find_index :
  ident_kind -> Ident.t -> t -> int option
