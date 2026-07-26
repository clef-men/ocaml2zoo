open Implementation

module Punctuation = struct
  let alt =
    "|"
  let arrow =
    "->"
  let atomic_loc_left =
    ".["
  let atomic_loc_right =
    "]"
  let backtick =
    "‘"
  let brace_left =
    "{"
  let brace_right =
    "}"
  let bracket_left =
    "["
  let bracket_left' =
    "@["
  let bracket_right =
    "]"
  let comma =
    ","
  let equal =
    "="
  let paren_left =
    "("
  let paren_right =
    ")"
  let proj_left =
    ".<"
  let proj_right =
    ">"
  let record_get_left =
    ".{"
  let record_get_right =
    "}"
  let record_set_left =
    "<-{"
  let record_set_right =
    "}"
  let ref_get =
    "!"
  let ref_set =
    "<-"
  let semicolon =
    "⍮"
  let tag =
    "§"
  let wildcard =
    "⎽"

  let unit =
    "()"

  let nil =
    "[]"
  let cons =
    "::"
end

module Unop = struct
  let neg =
    "~"
  let minus =
    "-"
end

module Binop = struct
  let minus =
    "-"
  let mult =
    "*"
  let plus =
    "+"
  let quot =
    "𝗾𝘂𝗼𝘁"
  let rem =
    "𝗿𝗲𝗺"

  let land_ =
    "𝗹𝗮𝗻𝗱"
  let lor_ =
    "𝗹𝗼𝗿"
  let lsl_ =
    "𝗹𝘀𝗹"
  let lsr_ =
    "𝗹𝘀𝗿"

  let eq =
    "=="
  let ge =
    "≥"
  let gt =
    ">"
  let le =
    "≤"
  let lt =
    "<"
  let ne =
    "!="

  let and_ =
    "𝗮𝗻𝗱"
  let or_ =
    "𝗼𝗿"

  let structeq =
    "="
  let structne =
    "≠"
end

module Keyword = struct
  let as_ =
    "𝗮𝘀"
  let do_ =
    "𝗱𝗼"
  let done_ =
    "𝗱𝗼𝗻𝗲"
  let else_ =
    "𝗲𝗹𝘀𝗲"
  let end_ =
    "𝗲𝗻𝗱"
  let for_ =
    "𝗳𝗼𝗿"
  let fun_ =
    "𝗳𝘂𝗻"
  let if_ =
    "𝗶𝗳"
  let in_ =
    "𝗶𝗻"
  let let_ =
    "𝗹𝗲𝘁"
  let letrec =
    "𝗹𝗲𝘁𝗿𝗲𝗰"
  let match_ =
    "𝗺𝗮𝘁𝗰𝗵"
  let rec_ =
    "𝗿𝗲𝗰"
  let recs =
    "𝗿𝗲𝗰𝘀"
  let then_ =
    "𝘁𝗵𝗲𝗻"
  let to_ =
    "𝘁𝗼"
  let with_ =
    "𝘄𝗶𝘁𝗵"

  let alloc =
    "𝗮𝗹𝗹𝗼𝗰"
  let assert_ =
    "𝗮𝘀𝘀𝗲𝗿𝘁"
  let assume =
    "𝗮𝘀𝘀𝘂𝗺𝗲"
  let cas =
    "𝗰𝗮𝘀"
  let diverge =
    "𝗱𝗶𝘃𝗲𝗿𝗴𝗲"
  let faa =
    "𝗳𝗮𝗮"
  let fail =
    "𝗳𝗮𝗶𝗹"
  let tag =
    "𝘁𝗮𝗴"
  let size =
    "𝘀𝗶𝘇𝗲"
  let id =
    "𝗶𝗱"
  let immediate =
    "𝗶𝗺𝗺𝗲𝗱𝗶𝗮𝘁𝗲"
  let load =
    "𝗹𝗼𝗮𝗱"
  let proph =
    "𝗽𝗿𝗼𝗽𝗵"
  let ref =
    "𝗿𝗲𝗳"
  let resolve =
    "𝗿𝗲𝘀𝗼𝗹𝘃𝗲"
  let skip =
    "𝘀𝗸𝗶𝗽"
  let store =
    "𝘀𝘁𝗼𝗿𝗲"
  let xchg =
    "𝘅𝗰𝗵𝗴"
end

let pp_boolean ppf =
  Fmt.pf ppf "%B"

let pp_integer ppf int =
  if int < 0 then
    Fmt.pf ppf "%s%i%s"
      Punctuation.paren_left
      int
      Punctuation.paren_right
  else
    Fmt.pf ppf "%i"
      int

let pp_local ppf local =
  Fmt.pf ppf {|"%s"|} local

let pp_binder ppf = function
  | None ->
      Fmt.string ppf Punctuation.wildcard
  | Some local ->
      pp_local ppf local

let pp_pattern ppf = function
  | Pat_var local ->
      pp_local ppf local
  | Pat_tuple bdrs ->
      Fmt.(list ~sep:(const string @@ Punctuation.comma ^ " ") pp_binder) ppf bdrs
  | Pat_constr (tag, bdrs) ->
      Fmt.pf ppf "%s%s %a"
        Punctuation.backtick
        tag
        Fmt.(list ~sep:(const string @@ Punctuation.comma ^ " ") pp_binder) bdrs

let pp_unop ppf op =
  let open Unop in
  Fmt.string ppf
    begin match op with
    | Unop_neg ->
        neg
    | Unop_minus ->
        minus
    end

let pp_binop ppf op =
  let open Binop in
  Fmt.string ppf
    begin match op with
    | Binop_plus ->
        plus
    | Binop_minus ->
        minus
    | Binop_mult ->
        mult
    | Binop_quot ->
        quot
    | Binop_rem ->
        rem
    | Binop_land ->
        land_
    | Binop_lor ->
        lor_
    | Binop_lsl ->
        lsl_
    | Binop_lsr ->
        lsr_
    | Binop_eq ->
        eq
    | Binop_ne ->
        ne
    | Binop_le ->
        le
    | Binop_lt ->
        lt
    | Binop_ge ->
        ge
    | Binop_gt ->
        gt
    | Binop_and ->
        and_
    | Binop_or ->
        or_
    | Binop_structeq ->
        structeq
    | Binop_structne ->
        structne
    end

let pp_primitive ppf prim =
  let open Keyword in
  Fmt.string ppf
    begin match prim with
    | Alloc ->
        alloc
    | Assert ->
        assert_
    | Assume ->
        assume
    | Cas ->
        cas
    | Diverge ->
        diverge
    | Faa ->
        faa
    | Fail ->
        fail
    | Tag ->
        tag
    | Size ->
        size
    | Id ->
        id
    | Immediate ->
        immediate
    | Load ->
        load
    | Proph ->
        proph
    | Ref ->
        ref
    | Resolve ->
        resolve
    | Skip ->
        skip
    | Store ->
        store
    | Xchg ->
        xchg
    end

type associativity =
  | Left
  | Right
let associativity = function
  | Binop_lsl
  | Binop_lsr ->
      Right
  | Binop_plus
  | Binop_minus
  | Binop_mult
  | Binop_quot
  | Binop_rem
  | Binop_land
  | Binop_lor
  | Binop_eq
  | Binop_ne
  | Binop_le
  | Binop_lt
  | Binop_ge
  | Binop_gt
  | Binop_and
  | Binop_or
  | Binop_structeq
  | Binop_structne ->
      Left

let max_level =
  200
let next_level lvl =
  lvl - 1
let rec level = function
  | Constr (_, "::", _) ->
      60
  | Global _
  | Local _
  | Bool _
  | Int _
  | If _
  | For _
  | Tuple _
  | Record _
  | Constr _
  | Match _
  | Primitive _ ->
      1
  | Apply (expr, []) when level expr <= 1 ->
      1
  | Proj _ ->
      2
  | Ref_get _
  | Record_get _
  | Atomic_loc _ ->
      9
  | Apply _ ->
      10
  | Binop (Binop_lsl, _, _)
  | Binop (Binop_lsr, _, _) ->
      30
  | Binop (Binop_land, _, _) ->
      31
  | Binop (Binop_lor, _, _) ->
      32
  | Unop (Unop_minus, _)
  | Binop (Binop_quot, _, _)
  | Binop (Binop_rem, _, _) ->
      35
  | Binop (Binop_mult, _, _) ->
      40
  | Binop (Binop_plus, _, _)
  | Binop (Binop_minus, _, _) ->
      50
  | Binop (Binop_eq, _, _)
  | Binop (Binop_ne, _, _)
  | Binop (Binop_le, _, _)
  | Binop (Binop_lt, _, _)
  | Binop (Binop_ge, _, _)
  | Binop (Binop_gt, _, _)
  | Binop (Binop_structeq, _, _)
  | Binop (Binop_structne, _, _) ->
      70
  | Unop (Unop_neg, _) ->
      75
  | Binop (Binop_and, _, _) ->
      76
  | Binop (Binop_or, _, _) ->
      77
  | Ref_set _
  | Record_set _ ->
      80
  | Seq _ ->
      100
  | Let _
  | Letrec _
  | Fun _ ->
      max_level

let rec pp_expression' lvl ppf = function
  | Global spath ->
      Spath.pp ppf spath
  | Local local ->
      pp_local ppf local
  | Bool bool ->
      pp_boolean ppf bool
  | Int int ->
      pp_integer ppf int
  | Let (pat, expr1, expr2) ->
      Fmt.pf ppf "@[<v>@[<hv>%s %a %s@;<1 2>@[%a@]@;%s@]@,%a@]"
        Keyword.let_
        pp_pattern pat
        Punctuation.equal
        (pp_expression max_level) expr1
        Keyword.in_
        (pp_expression max_level) expr2
  | Letrec (rec_flag, local, bdrs, expr1, expr2) ->
      Fmt.pf ppf "@[<v>@[<hv>%s %a %a %s@;<1 2>@[%a@]@;%s@]@,%a@]"
        Keyword.(match rec_flag with Nonrecursive -> let_ | Recursive -> letrec)
        pp_local local
        Fmt.(list ~sep:(const char ' ') pp_binder) bdrs
        Punctuation.equal
        (pp_expression max_level) expr1
        Keyword.in_
        (pp_expression max_level) expr2
  | Seq (expr1, expr2) ->
      Fmt.pf ppf "@[<v>@[" ;
      begin match expr1 with
      | If (expr1, expr2, expr3) ->
          pp_expression_if ~force_else:true ppf expr1 expr2 expr3
      | _ ->
          pp_expression (next_level lvl) ppf expr1
      end ;
      Fmt.pf ppf "@] %s@,%a@]"
        Punctuation.semicolon
        (pp_expression max_level) expr2
  | Fun (bdrs, expr) ->
      Fmt.pf ppf "@[<hv>%s %a %s@;<1 2>@[%a@]@]"
        Keyword.fun_
        Fmt.(list ~sep:(const char ' ') pp_binder) bdrs
        Punctuation.arrow
        (pp_expression max_level) expr
  | Unop (op, expr) ->
      Fmt.pf ppf "@[<hv>@[%a@]@;@[%a@]@]"
        pp_unop op
        (pp_expression lvl) expr
  | Binop (op, expr1, expr2) ->
      let assoc = associativity op in
      Fmt.pf ppf "@[<hv>@[%a@]@;@[%a@]@;@[%a@]@]"
        (pp_expression @@ if assoc = Left then lvl else next_level lvl) expr1
        pp_binop op
        (pp_expression @@ if assoc = Left then next_level lvl else lvl) expr2
  | If (expr1, expr2, expr3) ->
      pp_expression_if ppf expr1 expr2 expr3
  | For (local, expr1, expr2, expr3) ->
      Fmt.pf ppf "@[<v>@[<hv>%s@;<1 2>@[%a@]@;%s@;<1 2>@[%a@]@;%s@;<1 2>@[%a@]@;%s@]@,  @[%a@]@,%s@]"
        Keyword.for_
        pp_binder local
        Punctuation.equal
        (pp_expression max_level) expr1
        Keyword.to_
        (pp_expression max_level) expr2
        Keyword.do_
        (pp_expression max_level) expr3
        Keyword.done_
  | Tuple exprs ->
      Fmt.pf ppf "@[<hv>%s%a@,%s@]"
        Punctuation.paren_left
        Fmt.(
          list
            ~sep:(
              fun ppf () ->
                Fmt.pf ppf "%s@;<1 1>"
                  Punctuation.comma
            )
            pp_expression_box
        ) exprs
        Punctuation.paren_right
  | Record exprs ->
      Fmt.pf ppf "@[<hv>%s %a@;%s@]"
        Punctuation.brace_left
        Fmt.(
          list
            ~sep:(
              fun ppf () ->
                Fmt.pf ppf "%s@;<1 2>"
                  Punctuation.comma
            )
            pp_expression_box
        ) exprs
        Punctuation.brace_right
  | Constr (_, "[]", _) ->
      Fmt.string ppf Punctuation.nil
  | Constr (_, "::", exprs) ->
      let[@warning "-8"] [expr1; expr2] = exprs in
      Fmt.pf ppf "@[<hv>%a %s@;<1 2>@[%a@]@]"
        (pp_expression @@ next_level lvl) expr1
        Punctuation.cons
        (pp_expression lvl) expr2
  | Constr (_, tag, []) ->
      Fmt.pf ppf "%s%s"
        Punctuation.tag
        tag
  | Constr (flag, tag, exprs) ->
      Fmt.pf ppf "@[<hv>%s%s%s %a@;%s@]"
        Punctuation.backtick
        tag
        ( match flag with
          | Mutable ->
              Punctuation.brace_left
          | Immutable_nongenerative ->
              Punctuation.paren_left
          | Immutable_generative_weak ->
              Punctuation.bracket_left
          | Immutable_generative_strong ->
              Punctuation.bracket_left'
        )
        Fmt.(
          list
            ~sep:(
              fun ppf () ->
                Fmt.pf ppf "%s@;<1 2>"
                  Punctuation.comma
            )
            pp_expression_box
        ) exprs
        ( match flag with
          | Mutable ->
              Punctuation.brace_right
          | Immutable_nongenerative ->
              Punctuation.paren_right
          | Immutable_generative_weak
          | Immutable_generative_strong ->
              Punctuation.bracket_right
        )
  | Proj (expr, fld) ->
      Fmt.pf ppf "@[%a@]%s%s%s"
        (pp_expression lvl) expr
        Punctuation.proj_left
        fld
        Punctuation.proj_right
  | Match (expr, brs, fb) ->
      Fmt.pf ppf "@[<v>@[<hv>%s@;<1 2>@[%a@]@;%s@]@,%a%a%s@]"
        Keyword.match_
        (pp_expression max_level) expr
        Keyword.with_
        Fmt.(list ~sep:nop pp_branch) brs
        Fmt.(option pp_fallback) fb
        Keyword.end_
  | Ref_get expr ->
      Fmt.pf ppf "%s@[%a@]"
        Punctuation.ref_get
        (pp_expression lvl) expr
  | Ref_set (expr1, expr2) ->
      Fmt.pf ppf "@[<hv>@[<hv>@[%a@]@;<1 2>%s@]@;<1 2>@[%a@]@]"
        (pp_expression @@ next_level lvl) expr1
        Punctuation.ref_set
        (pp_expression lvl) expr2
  | Record_get (expr, fld) ->
      Fmt.pf ppf "@[%a@]%s%s%s"
        (pp_expression lvl) expr
        Punctuation.record_get_left
        fld
        Punctuation.record_get_right
  | Record_set (expr1, fld, expr2) ->
      Fmt.pf ppf "@[<hv>@[<hv>@[%a@]@;<1 2>%s%s%s@]@;<1 2>@[%a@]@]"
        (pp_expression @@ next_level lvl) expr1
        Punctuation.record_set_left
        fld
        Punctuation.record_set_right
        (pp_expression lvl) expr2
  | Atomic_loc (expr, fld) ->
      Fmt.pf ppf "@[%a@]%s%s%s"
        (pp_expression lvl) expr
        Punctuation.atomic_loc_left
        fld
        Punctuation.atomic_loc_right
  | Primitive prim ->
      pp_primitive ppf prim
  | Apply (expr, exprs) ->
      Fmt.pf ppf "@[<hv>@[%a@]%a@]"
        (pp_expression lvl) expr
        Fmt.(
          list ~sep:nop @@ fun ppf ->
            pf ppf "@;<1 2>@[%a@]"
              (pp_expression @@ next_level lvl)
        ) exprs
and pp_expression lvl ppf expr =
  let lvl_expr = level expr in
  if lvl < lvl_expr then
    Fmt.pf ppf "%s%a%s"
      Punctuation.paren_left
      (pp_expression' lvl_expr) expr
      Punctuation.paren_right
  else
    Fmt.pf ppf "%a"
      (pp_expression' lvl_expr) expr
and pp_expression_box ppf expr =
  Fmt.box (pp_expression max_level) ppf expr
and pp_expression_if_aux ?(nested = false) ?(force_else = false) ppf expr1 expr2 expr3 =
  Fmt.pf ppf "@[<hv>%s%s@;<1 2>@[%a@]@;%s %s@]@,  @[%a@]@,%s"
    (if nested then " 𝗲𝗹𝘀𝗲 " else "")
    Keyword.if_
    (pp_expression max_level) expr1
    Keyword.then_
    Punctuation.paren_left
    (pp_expression max_level) expr2
    Punctuation.paren_right ;
  match expr3 with
  | None ->
      if force_else then
        Fmt.pf ppf " %s %s@,  %s@,%s"
          Keyword.else_
          Punctuation.paren_left
          Punctuation.unit
          Punctuation.paren_right
  | Some expr3 ->
      match expr3 with
      | If (expr1, expr2, expr3) ->
          pp_expression_if_aux ~nested:true ppf expr1 expr2 expr3
      | expr ->
          Fmt.pf ppf " %s %s@,  @[%a@]@,%s"
            Keyword.else_
            Punctuation.paren_left
            (pp_expression max_level) expr
            Punctuation.paren_right
and pp_expression_if ?force_else ppf expr1 expr2 expr3 =
  Fmt.pf ppf "@[<v>" ;
  pp_expression_if_aux ?force_else ppf expr1 expr2 expr3 ;
  Fmt.pf ppf "@]"
and pp_branch ppf br =
  Fmt.pf ppf "%s "
    Punctuation.alt ;
  begin match br.branch_tag with
  | "[]" ->
      Fmt.string ppf Punctuation.nil
  | "::" ->
      let[@warning "-8"] [bdr1; bdr2] = br.branch_fields in
      Fmt.pf ppf "%a %s %a"
        pp_binder bdr1
        Punctuation.cons
        pp_binder bdr2
  | _ ->
      Fmt.pf ppf "%s%s%a"
        br.branch_tag
        (match br.branch_fields with [] -> "" | _ -> " ")
        Fmt.(list ~sep:(const char ' ') pp_binder) br.branch_fields
  end ;
  Fmt.pf ppf "%a %s@,    @[%a@]@,"
    Fmt.(option @@ fun ppf ->
      pf ppf " %s %a"
        Keyword.as_
        pp_local
    ) br.branch_as
    Punctuation.arrow
    (pp_expression max_level) br.branch_expr
and pp_fallback ppf fb =
  Fmt.pf ppf "%s %s%a %s@,    @[%a@]@,"
    Punctuation.alt
    Punctuation.wildcard
    Fmt.(option @@ fun ppf ->
      pf ppf " %s %a"
        Keyword.as_
        pp_local
    ) fb.fallback_as
    Punctuation.arrow
    (pp_expression max_level) fb.fallback_expr
let pp_expression =
  pp_expression max_level

let transl_typ ~lib ~mod_ (global, ty) =
  let spath = String.concat "." [lib; mod_; global] in
  match ty with
  | Type_product flds ->
      flds |> List.mapi @@ fun i fld ->
        Rocq.notation
          LocalityNormal
          fld
          ( fun ppf () ->
              Fmt.pf ppf {|in_type "%s" %i|}
                spath
                i
          )
          "zoo_proj"
  | Type_record flds ->
      flds |> List.mapi @@ fun i fld ->
        Rocq.notation
          LocalityNormal
          fld
          ( fun ppf () ->
              Fmt.pf ppf {|in_type "%s" %i|}
                spath
                i
          )
          "zoo_field"
  | Type_variant tags ->
      tags |> List.mapi @@ fun i tag ->
        Rocq.notation
          LocalityNormal
          tag
          ( fun ppf () ->
              Fmt.pf ppf {|in_type "%s" %i|}
                spath
                i
          )
          "zoo_tag"

let transl_value ~mod_ fresh = function
  | Val_expr (global, expr) ->
      [ Rocq.definition
          LocalityNormal
          Spath.([mod_; global] |> of_list |> to_string)
          (Some "val")
          ( fun ppf () ->
              pp_expression_box ppf expr
          )
      ]
  | Val_fun (global, params, expr) ->
      [ Rocq.definition
          LocalityNormal
          Spath.([mod_; global] |> of_list |> to_string)
          (Some "val")
          ( fun ppf () ->
              Fmt.pf ppf "@[<v>%s %a %s@,  @[%a@]@]"
                Keyword.fun_
                Fmt.(list ~sep:(const char ' ') pp_binder) params
                Punctuation.arrow
                pp_expression expr
          )
      ]
  | Val_recs [global, local, params, body] ->
      [ Rocq.definition
          LocalityNormal
          Spath.([mod_; global] |> of_list |> to_string)
          (Some "val")
          ( fun ppf () ->
              Fmt.pf ppf "@[<v>%s %a %a %s@,  @[%a@]@]"
                Keyword.rec_
                pp_local local
                Fmt.(list ~sep:(const char ' ') pp_binder) params
                Punctuation.arrow
                pp_expression body
          )
      ]
  | Val_recs recs ->
      let id = fresh () in
      List.concat
      [ [ Rocq.definition
            LocalityLocal
            (Printf.sprintf "__zoo_recs_%i" id)
            None
            ( fun ppf () ->
                Fmt.pf ppf "@[<v>( @[<v>%s %a@]@,)%%zoo_recs@]"
                  Keyword.recs
                  Fmt.(
                    list
                      ~sep:(
                        fun ppf () ->
                          Fmt.pf ppf "@,%s "
                            Keyword.with_
                      )
                      ( fun ppf (_, local, params, body) ->
                        pf ppf "%a %a %s@,  @[%a@]"
                          pp_local local
                          (list ~sep:(const char ' ') pp_binder) params
                          Punctuation.arrow
                          pp_expression body
                      )
                  ) recs
            )
        ]
      ; List.mapi (fun i (global, _, _, _) ->
          Rocq.definition
            LocalityNormal
            Spath.([mod_; global] |> of_list |> to_string)
            None
            ( fun ppf () ->
                Fmt.pf ppf "ValRecs %i __zoo_recs_%i"
                  i
                  id
            )
        ) recs
      ; List.mapi (fun i (global, _, _, _) ->
          Rocq.instance
            LocalityGlobal
            None
            ( fun ppf () ->
                Fmt.pf ppf "@[<v>AsValRecs' %a %i __zoo_recs_%i [@,  @[<v>%a@]@,]@]"
                  Spath.pp Spath.([mod_; global] |> of_list)
                  i
                  id
                  Fmt.(
                    list ~sep:(any " ;@,") @@ fun ppf (global, _, _, _) ->
                      Spath.([mod_; global] |> of_list |> pp ppf)
                  ) recs
            )
        ) recs
      ]
  | Val_opaque global ->
      [ Rocq.parameter
          Spath.([mod_; global] |> of_list |> to_string)
          "val"
      ]
let transl_value ~mod_ =
  let gen = ref 0 in
  transl_value ~mod_ (fun () ->
    let i = !gen in
    gen := i + 1 ;
    i
  )

let transl ~code t =
  let rocq =
    if code then
      List.map (transl_value ~mod_:t.module_) (values t)
    else
      List.map (transl_typ ~lib:t.library ~mod_:t.module_) (types t)
  in
  let rocq = List.interleave [Rocq.newline] rocq in
  List.concat (
    [ [ Rocq.require RequireImport "zoo.prelude"
      ; Rocq.require RequireImport "zoo.language.typeclasses"
      ; Rocq.require RequireImport "zoo.language.notations"
      ]
    ; t.dependencies
      |> Hashset.to_list_sort String.compare
      |> List.map (Rocq.require RequireImport)
    ; if code then
        [ Rocq.require RequireImport (Printf.sprintf "%s.%s__types" t.library t.module_)
        ]
      else
        []
    ; [ Rocq.require RequireImport "zoo.options"
      ; Rocq.newline
      ]
    ] @
    rocq
  )
let transl_types =
  transl ~code:false
let transl_code =
  transl ~code:true
