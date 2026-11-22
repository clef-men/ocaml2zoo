exception Error of unit Cmdliner.Term.ret

val raise :
  ?usage:bool ->
  ('a, Format.formatter, unit, unit, unit, 'b) format6 ->
  'a

val invalid_file :
  ?usage:bool ->
  filetype:Filetype.t ->
  ('a, Format.formatter, unit, unit, unit, 'b) format6 ->
  'a
val invalid_directory :
  ?usage:bool -> unit -> unit
