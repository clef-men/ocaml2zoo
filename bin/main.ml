type arguments =
  { input: string
  ; output: string
  ; force: bool
  ; quiet: bool
  ; ignore: string list
  ; only: string list
  ; describe: bool
  }

type output =
  { output_types: string
  ; output_code: string
  ; output_opaque: string
  }
let output ~output_dir ~mod_name ~mode =
  Printf.sprintf "%s%s.v"
    (Filename.concat output_dir mod_name)
    (Zoo.Mode.to_string mode)
let output ~output_dir ~mod_name =
  { output_types= output ~output_dir ~mod_name ~mode:Types
  ; output_code= output ~output_dir ~mod_name ~mode:Code
  ; output_opaque= output ~output_dir ~mod_name ~mode:Opaque
  }

let read ~filetype input =
  match Cmt_format.read_cmt input with
  | exception Sys_error err ->
      Error.invalid_file ~filetype ": %s"
        err
  | exception Cmt_format.Error _
  | exception Cmi_format.Error _ ->
      Error.invalid_file ~filetype ""
  | cmt ->
      Load_path.(init ~auto_include:no_auto_include ~visible:cmt.cmt_loadpath.visible ~hidden:cmt.cmt_loadpath.hidden) ;
      cmt

let main_cmt ~lib_name ~mod_name ~input ~output =
  let cmt = read ~filetype:Cmt input in
  match cmt.cmt_annots with
  | Implementation str ->
      begin match Zoo.Implementation_of_typedtree.transl ~lib:lib_name ~mod_:mod_name str with
      | exception Zoo.Error.Error (loc, err) ->
          Error.raise ~usage:false "%a:@ %a"
            Location.print_loc loc
            Zoo.Error.pp err
      | exception Zoo.Implementation_of_typedtree.Ignore ->
          ()
      | ast ->
          let rocq = Zoo.Implementation_to_rocq.transl ~mode:Types ast in
          Out_channel.with_open_text output.output_types (fun chan ->
            Fmt.pf (Format.formatter_of_out_channel chan) "%a@."
              (Zoo.Rocq.pp ~mod_:ast.module_) rocq
          ) ;
          let rocq = Zoo.Implementation_to_rocq.transl ~mode:Code ast in
          Out_channel.with_open_text output.output_code (fun chan ->
            Fmt.pf (Format.formatter_of_out_channel chan) "%a@."
              (Zoo.Rocq.pp ~mod_:ast.module_) rocq
          ) ;
          if not ast.transparent then
            let rocq = Zoo.Implementation_to_rocq.transl ~mode:Opaque ast in
            Out_channel.with_open_text output.output_opaque (fun chan ->
              Fmt.pf (Format.formatter_of_out_channel chan) "%a@."
                (Zoo.Rocq.pp ~mod_:ast.module_) rocq
            )
      end
  | _ ->
      Error.invalid_file ~filetype:Cmt ": not an implementation"

let main_cmti ~lib_name ~mod_name ~input =
  let cmt = read ~filetype:Cmti input in
  match cmt.cmt_annots with
  | Interface sig_ ->
      begin match Zoo.Annotation_of_typedtree.transl ~lib:lib_name ~mod_:mod_name sig_ with
      | exception Zoo.Error.Error (loc, err) ->
          Error.raise ~usage:false "%a:@ %a"
            Location.print_loc loc
            Zoo.Error.pp err
      | _annot ->
          ()
      end
  | _ ->
      Error.invalid_file ~filetype:Cmti ": not an interface"

let main_singlefile ~(filetype : Filetype.t) args =
  let input = args.input in
  let lib_name = Filename.(input |> dirname |> basename) |> String.uncapitalize_ascii in
  let mod_name = Filename.(input |> basename |> remove_extension) |> String.uncapitalize_ascii in
  let output = output ~output_dir:args.output ~mod_name in
  match filetype with
  | Cmt ->
      main_cmt ~lib_name ~mod_name ~input ~output
  | Cmti ->
      main_cmti ~lib_name ~mod_name ~input

let check ~args ~input ~output =
  if args.force then
    true
  else
    match Unix.stat input with
    | exception Unix.Unix_error _ ->
        Error.invalid_file ~filetype:Cmt ""
    | input_stats ->
        match Unix.stat output.output_code with
        | output_stats when input_stats.st_mtime < output_stats.st_mtime ->
            false
        | _
        | exception Unix.Unix_error _ ->
            true
let main_directory args (dune : Dune.t) =
  let ignore = List.map String.uncapitalize_ascii args.ignore in
  let only = List.map String.uncapitalize_ascii args.only in
  dune.libraries |> Hashtbl.iter @@ fun _ (lib : Dune.library) ->
    if lib.library_local then
      let lib_name = lib.library_name in
      if not (List.mem lib_name ignore) && (only = [] || List.mem lib_name only) then
        let output_dir = Filename.concat args.output lib_name in
        begin try Sys.mkdir output_dir 0o777 with Sys_error _ -> () end ;
        lib.library_modules |> Hashtbl.iter @@ fun mod_name (mod_ : Dune.module_) ->
          if not @@ Filename.check_suffix mod_.module_impl "-gen" then
            let input = Filename.concat args.input mod_.module_cmt in
            let output = output ~output_dir ~mod_name in
            if check ~args ~input ~output then (
              main_cmt ~lib_name ~mod_name ~input ~output ;
              mod_.module_cmti |> Option.iter (fun cmti ->
                let input = Filename.concat args.input cmti in
                main_cmti ~lib_name ~mod_name ~input
              )
            ) else if not args.quiet then (
              Fmt.pr {|Ignoring module "%s" from library "%s" (already up-to-date).@.|}
                mod_name
                lib_name
            )
let main_directory args =
  begin try
    Sys.chdir args.input
  with Sys_error _ ->
    Error.raise "cannot move to input directory"
  end ;
  match Dune.of_directory () with
  | Error err ->
      Error.raise "%s"
        err
  | Ok dune ->
      if args.describe then (
        Fmt.pr "%a@."
          Dune.pp dune
      ) else (
        begin try
          Sys.chdir dune.build_context
        with Sys_error _ ->
          Error.raise "cannot move to dune build context: %s"
            (Filename.concat args.input dune.build_context)
        end ;
        main_directory args dune
      )

let main args =
  let input =
    try
      Unix.realpath args.input
    with Unix.Unix_error _ ->
      Error.raise "invalid input file"
  in
  let output =
    try
      Unix.realpath args.output
    with Unix.Unix_error _ ->
      Error.raise "invalid output directory"
  in
  let args = { args with input; output } in
  match Filetype.of_filename args.input with
  | Some filetype ->
      main_singlefile ~filetype args
  | None ->
      if Sys.is_directory args.input then
        main_directory args
      else
        Error.invalid_directory ()
