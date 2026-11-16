include Ocaml_common.Longident

let rec head = function
  | Lident id ->
      Some id
  | Ldot (t, _) ->
      head t.txt
  | Lapply _ ->
      None

let last = function
  | Lident id ->
      Some id
  | Ldot (_, id) ->
      Some id.txt
  | Lapply _ ->
      None

let rec of_array arr len i t =
  if i = len then
    t
  else
    let t = Location.mknoloc t in
    let id = Location.mknoloc arr.(i) in
    of_array arr len (i + 1) (Ldot (t, id))
let of_array arr =
  of_array arr (Array.length arr) 1 (Lident arr.(0))

let rec pp ppf = function
  | Lident id ->
      Fmt.string ppf id
  | Ldot (t, id) ->
      Fmt.pf ppf "%a.%s"
        pp t.txt
        id.txt
  | Lapply (t1, t2) ->
      Fmt.pf ppf "%a(%a)"
        pp t1.txt
        pp t2.txt

module Map =
  Map.Make (struct
    type nonrec t =
      t
    let compare =
      compare
  end)
