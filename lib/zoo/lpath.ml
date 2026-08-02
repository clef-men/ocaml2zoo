type t =
  | Ident of string
  | Dot of t * string

let rec cons name = function
  | Ident name' ->
      Dot (Ident name, name')
  | Dot (t, name') ->
      Dot (cons name t, name')

let rec of_list acc = function
  | [] ->
      acc
  | name :: names ->
      of_list (Dot (acc, name)) names
let of_list = function
  | [] ->
      invalid_arg __FUNCTION__
  | name :: names ->
      of_list (Ident name) names

let rec append t1 = function
  | Ident name ->
      Dot (t1, name)
  | Dot (t2, name) ->
      Dot (append t1 t2, name)

let append_list =
  List.fold_left (fun t name -> Dot (t, name))

let set_last t name =
  match t with
  | Ident _ ->
      Ident name
  | Dot (t, _) ->
      Dot (t, name)

let rec to_list acc = function
  | Ident name ->
      name :: acc
  | Dot (t, name) ->
      to_list (name :: acc) t
let to_list =
  to_list []

let to_string ~sep t =
  t
  |> to_list
  |> String.concat sep
let to_string ~sep ?(mod_ = "") t =
  let t = to_string ~sep t in
  if mod_ = "" then
    t
  else
    Printf.sprintf "%s%s%s"
      mod_
      sep
      t

let pp ~sep ?mod_ ppf t =
  t
  |> to_string ~sep ?mod_
  |> Fmt.string ppf
