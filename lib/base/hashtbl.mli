include module type of struct
  include Stdlib.Hashtbl
end

val create :
  unit -> ('a, 'b) t

val is_empty :
  ('a, 'b) t -> bool
