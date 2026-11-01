type t

val create :
  lib:string -> mod_:string -> final_env:Env.t -> t

val module_ :
  t -> string

val env :
  t -> Env.t
val set_env :
  t -> Env.t -> unit

val find_type :
  t -> Path.t -> Types.type_declaration

val add_local :
  t -> Env.ident_kind -> Ident.t -> Lpath.t
val with_module :
  t -> mod_:Ident.t -> final_env:Env.t -> (unit -> 'a) -> 'a

val add_constructor :
  t -> Types.constructor_declaration -> unit

val add_label :
  t -> Types.label_declaration -> unit

val add_var :
  t -> Ident.t -> unit
val begin_scope :
  t -> (unit -> unit)
val with_scope :
  t -> (unit -> 'a) -> 'a

val resolve_path_value :
  t -> loc:Location.t -> Path.t -> Implementation.expression

val resolve_constructor :
  t -> loc:Location.t -> Data_types.constructor_description -> Path.t * Gpath.t
val resolve_label :
  t -> loc:Location.t -> Data_types.label_description -> Path.t * Gpath.t

val transparent :
  t -> bool
val set_transparent :
  t -> unit
