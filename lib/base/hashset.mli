type 'a t

val create :
  unit -> 'a t

val singleton :
  'a -> 'a t

val add :
  'a t -> 'a -> unit

val pp :
  ?sep:unit Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
