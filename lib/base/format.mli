include module type of struct
  include Stdlib.Format
end

type 'a t =
  formatter -> 'a -> unit

val pp_print_list :
  ?sep:unit t ->
  (formatter -> int -> 'a -> unit) ->
  'a list t
