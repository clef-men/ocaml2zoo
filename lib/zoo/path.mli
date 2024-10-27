include module type of struct
  include Ocaml_common.Path
end

val head :
  t -> Ident.t option

val flatten :
  t -> (Ident.t * string list) option

val of_array :
  string array -> t

val to_string :
  string -> t -> string option
