type path =
  string

type ident =
  string

type term =
  string

type scope =
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

type item =
  | Newline
  | Require of require_kind * path
  | Parameter of ident * term
  | Definition of locality * ident * term option * custom
  | Instance of locality * ident option * custom
  | Notation of locality * string * custom * scope
  | Opaque of locality * ident

type t =
  item list

let newline =
  Newline
let[@inline] require kind path =
  Require (kind, path)
let[@inline] parameter id tm =
  Parameter (id, tm)
let[@inline] definition local id tm custom =
  Definition (local, id, tm, custom)
let[@inline] instance local id custom =
  Instance (local, id, custom)
let[@inline] notation local str custom scope =
  Notation (local, str, custom, scope)
let[@inline] opaque local id =
  Opaque (local, id)

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

let pp_item ppf = function
  | Newline ->
      ()
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
  | Notation (local, str, custom, scope) ->
      Fmt.pf ppf {|%aNotation "'%s'" := (@,  %a@,)(in custom %s@,).|}
        pp_locality local
        str
        custom ()
        scope
  | Opaque (local, id) ->
      Fmt.pf ppf "%aOpaque %s."
        pp_locality local
        id

let pp ppf t =
  Fmt.pf ppf "@[<v>" ;
  Fmt.list pp_item ppf t ;
  Fmt.pf ppf "@]"
