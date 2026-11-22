include Ocaml_common.Envaux

let env_of_only_summary env =
  try
    env_of_only_summary env
  with Error err ->
    Error.raise ~loc:Location.none (Envaux err)
