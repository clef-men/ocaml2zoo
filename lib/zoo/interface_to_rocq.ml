let transl (t : Interface.t) =
  List.concat
  [ [ Rocq.require RequireImport (Printf.sprintf "%s.%s__code" t.library t.module_)
    ; Rocq.newline
    ]
  ; List.map (fun global ->
      Rocq.opaque LocalityGlobal Spath.([t.module_; global] |> of_list |> to_string)
    ) t.values
  ]
