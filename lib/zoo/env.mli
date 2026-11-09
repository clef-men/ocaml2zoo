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

(* [max_scope env] is the maximal scope of the identifiers bound in [env],
   ignoring unscoped identifiers. Typing new code in [env] must happen at a
   level at least [max_scope env], otherwise type constructors of [env] may
   be wrongly reported as escaping their scope. *)
val max_scope :
  t -> int
