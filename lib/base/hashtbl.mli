include module type of struct
  include Stdlib.Hashtbl
end

val create :
  unit -> ('a, 'b) t

val is_empty :
  ('a, 'b) t -> bool

val map_list :
  ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c list

val keys :
  ('a, 'b) t -> 'a list
