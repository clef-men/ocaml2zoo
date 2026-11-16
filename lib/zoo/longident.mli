include module type of struct
  include Ocaml_common.Longident
end

val head :
  t -> string option

val last :
  t -> string option

val of_array :
  string array -> t

val pp :
  t Fmt.t

module Map :
  Map.S with type key = t
