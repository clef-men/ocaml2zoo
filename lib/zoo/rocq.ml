module Type = struct
  let value =
    "val"
end

module Custom_entry = struct
  let recs =
    Printf.sprintf "zoo%srecs"
      Separator.cdot1
end

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
  | Local
  | Global

type qualification =
  | Qualified
  | Nonqualified

type mutability = Implementation.mutability =
  | Immutable
  | Mutable

type constancy = Implementation.constancy =
  | Constant
  | Nonconstant

type zoo_constructor =
  { zoo_constructor_tag: string
  ; zoo_constructor_constancy: constancy
  }

type zoo_type =
  | Zoo_record of mutability * string list
  | Zoo_variant of zoo_constructor list

type item =
  | Newline
  | Comment of string
  | Require of require_kind * path
  | Opaque of Lpath.t
  | Zoo_type of qualification * Lpath.t * zoo_type
  | Zoo_val of Lpath.t * Implementation.expression
  | Zoo_fun of Lpath.t * Implementation.binder list * Implementation.expression
  | Zoo_recs of (Lpath.t * Var.t * Implementation.binder list * Implementation.expression) list
  | Zoo_parameter of Lpath.t

type t =
  item list

let pp_require_kind ppf = function
  | Require_only ->
      ()
  | Require_import ->
      Fmt.pf ppf " Import"
  | Require_export ->
      Fmt.pf ppf " Export"

let pp_locality ppf = function
  | Local ->
      Fmt.pf ppf "#[local] "
  | Global ->
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

let pp_constancy ppf = function
  | Constant ->
      ()
  | Nonconstant ->
      Fmt.pf ppf " _"

let pp_zoo_constructor ppf constr =
  Fmt.pf ppf "%s%a"
    constr.zoo_constructor_tag
    pp_constancy constr.zoo_constructor_constancy

let pp_zoo_type ppf = function
  | Zoo_record (mut, flds) ->
      Fmt.pf ppf "%a@,  @[<v>{ %a@,}@]"
        pp_mutability mut
        Fmt.(list ~sep:(any "@,; ") string) flds
  | Zoo_variant constrs ->
      Fmt.pf ppf "@,  @[<v>| %a@]"
        Fmt.(list ~sep:(any "@,| ") pp_zoo_constructor) constrs

let pp_item ~mod_ ~gen ppf = function
  | Newline ->
      ()
  | Comment str ->
      Fmt.pf ppf "(* %s *)"
        str
  | Require (kind, path) ->
      Fmt.pf ppf "Require%a %s."
        pp_require_kind kind
        path
  | Opaque path ->
      Fmt.pf ppf "%aOpaque %a."
        pp_locality Global
        (Lpath.pp ~sep:Separator.cdot2 ~mod_) path
  | Zoo_type (qualif, path, typ) ->
      Fmt.pf ppf "%aZoo type %a :=%a."
        pp_qualification qualif
        (Lpath.pp ~sep:"." ?mod_:None) path
        pp_zoo_type typ
  | Zoo_val (path, expr) ->
      Fmt.pf ppf "Definition %a : %s :=@,  @[%a@]."
        (Lpath.pp ~sep:Separator.cdot2 ~mod_) path
        Type.value
        (Implementation_pp.pp_expression ~mod_) expr
  | Zoo_fun (path, params, expr) ->
      Fmt.pf ppf "Definition %a : %s :=@,  @[%a@]."
        (Lpath.pp ~sep:Separator.cdot2 ~mod_) path
        Type.value
        (Implementation_pp.pp_fun ~mod_) (params, expr)
  | Zoo_recs [path, var, params, expr] ->
      Fmt.pf ppf "Definition %a : %s :=@,  @[%a@]."
        (Lpath.pp ~sep:Separator.cdot2 ~mod_) path
        Type.value
        (Implementation_pp.pp_rec ~mod_) (path, var, params, expr)
  | Zoo_recs recs ->
      let id = Generator.next gen in
      Fmt.pf ppf "%aDefinition __%s%s%i :=@,  @[<v>( %a@,)%%%s@]."
        pp_locality Local
        Custom_entry.recs
        Separator.cdot1
        id
        (Implementation_pp.pp_recs ~mod_) recs
        Custom_entry.recs ;
      Fmt.pf ppf "@," ;
      Format.pp_print_list (fun ppf i (path, _, _, _) ->
        Fmt.pf ppf "Definition %a :=@,  ValRecs %i __%s%s%i."
          (Lpath.pp ~sep:Separator.cdot2 ~mod_) path
          i
          Custom_entry.recs
          Separator.cdot1
          id
      ) ppf recs ;
      Fmt.pf ppf "@," ;
      Format.pp_print_list (fun ppf i (path, _, _, _) ->
        Fmt.pf ppf "%aInstance :@,  @[<v>AsValRecs'@,  @[<v>%a@,%i@,__%s%s%i@,[ %a@,]@]@].@,Proof.@,  done.@,Qed."
          pp_locality Global
          (Lpath.pp ~sep:Separator.cdot2 ~mod_) path
          i
          Custom_entry.recs
          Separator.cdot1
          id
          Fmt.(
            list ~sep:(any "@,; ") @@ fun ppf (path, _, _, _) ->
              Lpath.pp ~sep:Separator.cdot2 ~mod_ ppf path
          ) recs
      ) ppf recs
  | Zoo_parameter path ->
      Fmt.pf ppf "Parameter %a : %s."
        (Lpath.pp ~sep:Separator.cdot2 ~mod_) path
        Type.value

let pp ~mod_ ppf t =
  let gen = Generator.create () in
  Fmt.pf ppf "@[<v>" ;
  Fmt.list (pp_item ~mod_ ~gen) ppf t ;
  Fmt.pf ppf "@]"
