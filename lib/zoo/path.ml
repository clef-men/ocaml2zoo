include Ocaml_common.Path

let rec head = function
  | Pident id ->
      Some id
  | Pdot (t, _)
  | Pextra_ty (t, _) ->
      head t
  | Papply _ ->
      None

let rec of_array arr len i t =
  if i = len then
    t
  else
    of_array arr len (i + 1) (Pdot (t, arr.(i)))
let of_array arr =
  of_array arr (Array.length arr) 1 (Pident (Ident.create_persistent arr.(0)))

let rec to_list acc = function
  | Pident id ->
      Some (id, acc)
  | Pdot (t, name) ->
      to_list (name :: acc) t
  | Pextra_ty (t, _) ->
      to_list acc t
  | Papply _ ->
      None
let to_list =
  to_list []

let pp =
  Format_doc.compat Ocaml_common.Path.print
