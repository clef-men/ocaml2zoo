include Ocaml_common.Longident

let rec head = function
  | Lident s ->
      Some s
  | Ldot (t, _) ->
      head t.txt
  | Lapply (_, _) ->
      None

let last = function
  | Lident s ->
      Some s
  | Ldot (_, s) ->
      Some s.txt
  | Lapply (_, _) ->
      None

let rec of_array arr len i t =
  if i = len then
    t
  else
    let t = Location.mknoloc t in
    let s = Location.mknoloc arr.(i) in
    of_array arr len (i + 1) (Ldot (t, s))
let of_array arr =
  of_array arr (Array.length arr) 1 (Lident arr.(0))

let rec to_string sep acc = function
  | Lident s ->
      Some (s ^ acc)
  | Ldot (t, s) ->
      to_string sep (sep ^ s.txt ^ acc) t.txt
  | Lapply _ ->
      None
let to_string sep = function
  | Lident s ->
      Some s
  | Ldot (t, s) ->
      to_string sep (sep ^ s.txt) t.txt
  | Lapply _ ->
      None

module Map =
  Map.Make (struct
    type nonrec t =
      t
    let compare =
      compare
  end)
