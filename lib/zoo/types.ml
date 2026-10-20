include Ocaml_common.Types

let is_int expr =
  match get_desc expr with
  | Tconstr (path, _, _) ->
      Path.same Predef.path_int path
  | _ ->
      false
