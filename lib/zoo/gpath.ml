type t =
  { library: string
  ; module_: string
  ; path: Lpath.t
  }

let make ?(lib = "") ?(mod_ = "") path =
  { library= lib
  ; module_= mod_
  ; path
  }

let ident ?lib ?mod_ id =
  make ?lib ?mod_ (Ident id)

let to_string ~sep t =
  Lpath.to_string ~sep ~mod_:t.module_ t.path

let to_string_full ~sep t =
  Printf.sprintf "%s%s%s"
    t.library
    sep
    (to_string ~sep t)

let pp ~sep ppf t =
  t
  |> to_string ~sep
  |> Fmt.string ppf

let pp_full ~sep ppf t =
  t
  |> to_string_full ~sep
  |> Fmt.string ppf

module Builtin = struct
  let _0 =
    ident "0"
  let _1 =
    ident "1"

  let contents =
    ident "contents"
end
