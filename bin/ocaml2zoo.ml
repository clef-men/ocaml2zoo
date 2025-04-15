open Cmdliner

let input =
  let docv = "input" in
  let doc = "Input dune project or .cmt file." in
  Arg.(required & pos 0 (some file) None & info [] ~docv ~doc)

let output =
  let docv = "output" in
  let doc = "Output directory." in
  Arg.(required & pos 1 (some dir) None & info [] ~docv ~doc)

let force =
  let doc = "Force generation for up-to-date targets." in
  Arg.(value & flag & info ["f";"force"] ~doc)

let quiet =
  let doc = "Enable quiet mode." in
  Arg.(value & flag & info ["q";"quiet"] ~doc)

let ignore_file =
  ".zooignore"
let ignore =
  let docv = "library" in
  let doc = "Prevent a library from being processed." in
  Arg.(value & opt_all string [] & info ["ignore"] ~docv ~doc)

let only =
  let docv = "library" in
  let doc = "Mark a library. Only marked libraries are processed." in
  Arg.(value & opt_all string [] & info ["only"] ~docv ~doc)

let info =
  let doc = "OCaml to Zoo" in
  Cmd.info "ocaml2zoo" ~doc

let main input output force quiet ignore only =
  let ignore =
    try
      In_channel.with_open_text ignore_file (fun input ->
        In_channel.fold_lines (fun ignore line ->
          String.trim line :: ignore
        ) ignore input
      )
    with Sys_error _ ->
      ignore
  in
  let args : Main.arguments =
    { input;
      output;
      force;
      quiet;
      ignore;
      only;
    }
  in
  try `Ok (Main.main args)
  with Main.Error err -> err
let main =
  let open Term in
  ret (
    const main
    $ input
    $ output
    $ force
    $ quiet
    $ ignore
    $ only
  )
let () =
  Cmd.v info main
  |> Cmd.eval
  |> exit
