type module_ =
  { module_name: string
  ; module_impl: string
  ; module_cmt: string
  ; module_cmti: string option
  }

type library =
  { library_name: string
  ; library_local: bool
  ; library_modules: (string, module_) Hashtbl.t
  }

type t =
  { build_context: string
  ; libraries: (string, library) Hashtbl.t
  }

module Of_sexp = struct
  exception Error of string

  let invalid () =
    raise @@ Error "dune description is ill-formed"

  let (!!) ref =
    if !ref = "" then
      invalid () ;
    !ref

  type sexp = Csexp.t =
    | Atom of string
    | List of sexp list

  let (let@) sexp fn =
    match sexp with
    | Atom _ ->
        invalid ()
    | List sexps ->
        fn sexps
  let (let<) sexps fn =
    match sexps with
    | [] ->
        invalid ()
    | sexp :: _ ->
        fn sexp
  let (let<@) sexps fn =
    match sexps with
    | List sexps :: _ ->
        fn sexps
    | _ ->
        invalid ()

  let rec bool = function
    | Atom "true" ->
        true
    | Atom "false" ->
        false
    | List [sexp] ->
        bool sexp
    | _ ->
        invalid ()

  let rec string = function
    | Atom str ->
        str
    | List [sexp] ->
        string sexp
    | _ ->
        invalid ()

  let module_ sexp =
    let@ sexps = sexp in
    let name = ref "" in
    let impl = ref "" in
    let cmt = ref "" in
    let cmti = ref None in
    sexps |> List.iter (function
      | List (Atom "name" :: sexps) ->
          let< sexp = sexps in
          name := string sexp
      | List (Atom "impl" :: sexps) ->
          let<@ sexps = sexps in
          let< sexp = sexps in
          impl := string sexp
      | List (Atom "cmt" :: sexps) ->
          let< sexp = sexps in
          cmt := string sexp
      | List (Atom "cmti" :: sexps) ->
          let< sexp = sexps in
          if sexp <> List [] then
            cmti := Some (string sexp)
      | _ ->
          ()
    ) ;
    { module_name= String.uncapitalize_ascii !!name
    ; module_impl= !!impl
    ; module_cmt= !!cmt
    ; module_cmti= !cmti
    }

  let library sexp =
    let@ sexps = sexp in
    let name = ref "" in
    let local = ref None in
    let mods = Hashtbl.create () in
    sexps |> List.iter (function
      | List (Atom "name" :: sexps) ->
          let< sexp = sexps in
          name := String.uncapitalize_ascii (string sexp)
      | List (Atom "local" :: sexps) ->
          let< sexp = sexps in
          local := Some (bool sexp)
      | List (Atom "modules" :: sexps) ->
          let<@ sexps = sexps in
          sexps |> List.iter (fun sexp ->
            let mod_ = module_ sexp in
            Hashtbl.add mods mod_.module_name mod_
          )
      | _ ->
          ()
    ) ;
    { library_name= !!name
    ; library_local= Option.get_lazy invalid !local
    ; library_modules= mods
    }

  let main sexp =
    let@ sexps = sexp in
    let build_context = ref "" in
    let libs = Hashtbl.create () in
    sexps |> List.iter (function
      | List (Atom "build_context" :: sexps) ->
          let< sexp = sexps in
          build_context := string sexp
      | List (Atom "library" :: sexps) ->
          let< sexp = sexps in
          let lib = library sexp in
          Hashtbl.add libs lib.library_name lib
      | _ ->
          ()
    ) ;
    { build_context= !!build_context
    ; libraries= libs
    }
  let main sexp =
    try
      Ok (main sexp)
    with Error err ->
      Error err
end

let of_sexp =
  Of_sexp.main

let describe_command =
  "dune describe --lang 0.1 --format csexp --root ."
let of_directory () =
  let chan = Unix.open_process_in describe_command in
  set_binary_mode_in chan false ;
  Fun.protect ~finally:(fun () -> close_in chan) @@ fun () ->
    Result.bind (Csexp.input chan) of_sexp

let pp_module ppf mod_ =
  Fmt.pf ppf "+ %s@,  @[<v>+ impl: %s@,+ cmt: %s@,+ cmti: %s@]"
    mod_.module_name
    mod_.module_impl
    mod_.module_cmt
    (Option.value ~default:"∅" mod_.module_cmti)
let pp_library ppf lib =
  Fmt.pf ppf "+ %s (%s)@,  @[<v>%a@]"
    lib.library_name
    (if lib.library_local then "local" else "extern")
    (Fmt.hashtbl @@ fun ppf (_, mod_) -> pp_module ppf mod_) lib.library_modules
let pp ppf t =
  Fmt.pf ppf "@[<v>%a@]"
    (Fmt.hashtbl @@ fun ppf (_, lib) -> pp_library ppf lib) t.libraries
