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

let pp ~sep ppf t =
  Lpath.pp ~sep ~mod_:t.module_ ppf t.path

let pp_full ~sep ppf t =
  Fmt.pf ppf "%s%s%a"
    t.library
    sep
    (pp ~sep) t
