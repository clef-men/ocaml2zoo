let transl (t : Interface.t) =
  List.concat
  [ [ Rocq.require RequireImport t.library [t.module_ ^ "__code"]
    ; Rocq.newline
    ]
  ; List.map (fun global ->
      Rocq.opaque LocalityGlobal Spath.([t.module_; global] |> of_list |> to_string)
    ) t.values
  ]
