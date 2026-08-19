module type S = sig
  type elt

  type t

  val create :
    unit -> t

  val singleton :
    elt -> t

  val mem :
    t -> elt -> bool

  val add :
    t -> elt -> unit

  val remove :
    t -> elt -> unit

  val to_list :
    t -> elt list
  val to_list_sort :
    (elt -> elt -> int) -> t -> elt list

  val map_list :
    (elt -> 'a) -> t -> 'a list
end
