open Annotation

let btyp_to_string = function
  | Unit ->
      "unit"
  | Bool ->
      "bool"
  | Int ->
      "int"
  | Val ->
      "val"
let pp_btyp ppf bty =
  bty
  |> btyp_to_string
  |> Fmt.string ppf

let rec pp_typ ppf = function
  | Type_basic bty ->
      pp_btyp ppf bty
  | Type_iprop ->
      Fmt.pf ppf "iprop"
  | Type_arrow (ty1, ty2) ->
      Fmt.pf ppf "%a -> %a"
        pp_typ_protect ty1
        pp_typ ty2
and pp_typ_protect ppf ty =
  match ty with
  | Type_basic _
  | Type_iprop ->
      pp_typ ppf ty
  | Type_arrow _ ->
      Fmt.pf ppf "(%a)"
        pp_typ ty
