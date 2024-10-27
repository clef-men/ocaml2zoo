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

let exclude =
  let docv = "library" in
  let doc = "Ignored libraries." in
  Arg.(value & opt_all string [] & info ["exclude"] ~docv ~doc)

let info =
  let doc = "OCaml to Zoo" in
  Cmd.info "ocaml2zoo" ~doc

let main input output force quiet exclude =
  let args : Main.arguments =
    { input;
      output;
      force;
      quiet;
      exclude;
    }
  in
  try `Ok (Main.main args)
  with Main.Error err -> err
let main =
  Term.(ret (const main $ input $ output $ force $ quiet $ exclude))
let () =
  Cmd.v info main
  |> Cmd.eval
  |> exit
