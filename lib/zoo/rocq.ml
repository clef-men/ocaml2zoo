type path =
  string

type ident =
  string

type term =
  string

type require_kind =
  | Require_only
  | Require_import
  | Require_export

type locality =
  | Locality_normal
  | Locality_local
  | Locality_global

type custom =
  unit Fmt.t

type qualification =
  | Qualified
  | Nonqualified

type mutability = Implementation.mutability =
  | Immutable
  | Mutable

type zoo_type =
  | Zoo_record of mutability * string list
  | Zoo_variant of string list

type item =
  | Newline
  | Comment of string
  | Require of require_kind * path
  | Parameter of ident * term
  | Definition of locality * ident * term option * custom
  | Instance of locality * ident option * custom
  | Opaque of locality * ident
  | Zoo_type of qualification * Lpath.t * zoo_type

type t =
  item list

let newline =
  Newline
let[@inline] comment str =
  Comment str
let[@inline] require kind path =
  Require (kind, path)
let[@inline] parameter id tm =
  Parameter (id, tm)
let[@inline] definition local id tm custom =
  Definition (local, id, tm, custom)
let[@inline] instance local id custom =
  Instance (local, id, custom)
let[@inline] opaque local id =
  Opaque (local, id)
let[@inline] zoo_type qualif path typ =
  Zoo_type (qualif, path, typ)

let pp_require_kind ppf = function
  | Require_only ->
      ()
  | Require_import ->
      Fmt.pf ppf " Import"
  | Require_export ->
      Fmt.pf ppf " Export"

let pp_locality ppf = function
  | Locality_normal ->
      ()
  | Locality_local ->
      Fmt.pf ppf "#[local] "
  | Locality_global ->
      Fmt.pf ppf "#[global] "

let pp_qualification ppf = function
  | Qualified ->
      Fmt.pf ppf "#[qualified] "
  | Nonqualified ->
      ()

let pp_mutability ppf = function
  | Immutable ->
      ()
  | Mutable ->
      Fmt.pf ppf " mutable"

let pp_zoo_type ppf = function
  | Zoo_record (mut, flds) ->
      Fmt.pf ppf "%a@,  @[<v>{ %a@,}@]"
        pp_mutability mut
        Fmt.(list ~sep:(any "@,; ") string) flds
  | Zoo_variant constrs ->
      Fmt.pf ppf "@,  @[<v>| %a@]"
        Fmt.(list ~sep:(any "@,| ") string) constrs

let pp_item ppf = function
  | Newline ->
      ()
  | Comment str ->
      Fmt.pf ppf "(* %s *)"
        str
  | Require (kind, path) ->
      Fmt.pf ppf "Require%a %s."
        pp_require_kind kind
        path
  | Parameter (id, tm) ->
      Fmt.pf ppf "Parameter %s : %s."
        id
        tm
  | Definition (local, id, tm, custom) ->
      Fmt.pf ppf "%aDefinition %s%a :=@,  %a."
        pp_locality local
        id
        Fmt.(option @@ fmt " : %s") tm
        custom ()
  | Instance (local, id, custom) ->
      Fmt.pf ppf "%aInstance %a:@,  %a.@,Proof.@,  done.@,Qed."
        pp_locality local
        Fmt.(option string) id
        custom ()
  | Opaque (local, id) ->
      Fmt.pf ppf "%aOpaque %s."
        pp_locality local
        id
  | Zoo_type (qualif, lpath, typ) ->
      Fmt.pf ppf "%aZoo type %a :=%a."
        pp_qualification qualif
        (Lpath.pp ~sep:"." ?mod_:None) lpath
        pp_zoo_type typ

let pp ppf t =
  Fmt.pf ppf "@[<v>" ;
  Fmt.list pp_item ppf t ;
  Fmt.pf ppf "@]"
