exception Error of unit Cmdliner.Term.ret

let raise ?(usage = true) fmt =
  let fmt = "@[<v>" ^^ fmt ^^ "@]" in
  fmt |> Fmt.kstr @@ fun msg ->
    raise @@ Error (`Error (usage, msg))

let invalid_file ?(usage = true) ~filetype fmt =
  raise ~usage ("invalid %a file" ^^ fmt)
    Filetype.pp filetype
let invalid_directory ?(usage = true) () =
  raise ~usage "invalid directory"
