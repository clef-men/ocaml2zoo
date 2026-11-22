open Annotation

let transl_attribute ~ctx attr =
  if Attribute.has_zoo [attr] then
    match Attribute.payload attr with
    | None ->
        []
    | Some (loc, str) ->
        str
        |> Annotation_parsing.parse_declarations ~loc
        |> Annotation_typing.transl_declarations ~ctx
  else
    []

let transl_spec ~ctx ~ty attr =
  match Attribute.payload attr with
  | None ->
      []
  | Some (loc, str) ->
      str
      |> Annotation_parsing.parse_spec ~loc
      |> Annotation_typing.transl_spec ~ctx ~ty
      |> List.singleton

let transl_value_description ~ctx (val_ : Typedtree.value_description) =
  let path = Context.add_local ctx Ident_value val_.val_id in
  match Attribute.has_spec val_.val_attributes with
  | [] ->
      []
  | attrs ->
      let ty = val_.val_val.val_type in
      let specs = attrs |> List.concat_map (transl_spec ~ctx ~ty) in
      [Item_spec (path, specs)]

let rec transl_module_type ~ctx ~mod_ (mty : Typedtree.module_type) =
  match mty.mty_desc with
  | Tmty_signature sig_ ->
      transl_signature ~ctx ~mod_ sig_
  | Tmty_functor _ ->
      Error.raise ~loc:mty.mty_loc (Unsupported Functor)
  | Tmty_ident _
  | Tmty_alias _
  | Tmty_with _
  | Tmty_typeof _ ->
      []

and transl_module_declaration ~ctx (md : Typedtree.module_declaration) =
  match md.md_id with
  | None ->
      []
  | Some mod_ ->
      let _path = Context.add_local ctx Ident_module mod_ in
      transl_module_type ~ctx ~mod_ md.md_type

and transl_signature_item ~ctx (sig_item : Typedtree.signature_item) =
  match sig_item.sig_desc with
  | Tsig_value val_ ->
      transl_value_description ~ctx val_
  | Tsig_module md ->
      transl_module_declaration ~ctx md
  | Tsig_attribute attr ->
      transl_attribute ~ctx attr
  | Tsig_primitive _
  | Tsig_type _
  | Tsig_typesubst _
  | Tsig_typext _
  | Tsig_exception _
  | Tsig_modsubst _
  | Tsig_recmodule _
  | Tsig_modtype _
  | Tsig_modtypesubst _
  | Tsig_open _
  | Tsig_include _
  | Tsig_class _
  | Tsig_class_type _ ->
      []

and transl_signature' ~ctx (sig_ : Typedtree.signature) =
  sig_.sig_items |> List.concat_map (transl_signature_item ~ctx)
and transl_signature ~ctx ?mod_ sig_ =
  match mod_ with
  | None ->
      transl_signature' ~ctx sig_
  | Some mod_ ->
      let final_env = sig_.sig_final_env in
      Context.with_module ctx ~mod_ ~final_env @@ fun () ->
        transl_signature' ~ctx sig_

let transl ~lib ~mod_ (sig_ : Typedtree.signature) =
  let final_env = Envaux.env_of_only_summary sig_.sig_final_env in
  let ctx = Context.create ~lib ~mod_ ~final_env in
  let items = transl_signature ~ctx sig_ in
  { library= lib
  ; module_= mod_
  ; items
  }
