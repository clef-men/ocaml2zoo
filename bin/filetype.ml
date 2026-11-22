type t =
  | Cmt
  | Cmti

let of_filename file =
  match Filename.extension file with
  | ".cmt" ->
      Some Cmt
  | ".cmti" ->
      Some Cmti
  | _ ->
      None

let to_string = function
  | Cmt ->
      ".cmt"
  | Cmti ->
      ".cmti"

let pp ppf t =
  t
  |> to_string
  |> Fmt.string ppf
