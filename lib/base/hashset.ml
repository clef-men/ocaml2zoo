type 'a t =
  ('a, unit) Hashtbl.t

let create =
  Hashtbl.create

let add t elt =
  Hashtbl.replace t elt ()

let singleton elt =
  let t = create () in
  add t elt ;
  t

let pp ?sep pp_elt =
  Fmt.hashtbl ?sep (fun ppf (elt, ()) -> pp_elt ppf elt)
