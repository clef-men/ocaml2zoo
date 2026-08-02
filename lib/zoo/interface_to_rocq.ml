let separator =
  Implementation_to_rocq.separator

let transl (t : Interface.t) =
  List.concat
  [ [ Rocq.require RequireImport (Printf.sprintf "%s.%s__code" t.library t.module_)
    ; Rocq.newline
    ]
  ; List.map (fun path ->
      Rocq.opaque LocalityGlobal Lpath.(path |> cons t.module_ |> to_string ~sep:separator)
    ) t.values
  ]
