let transl (t : Interface.t) =
  List.concat
  [ [ Rocq.require RequireImport t.library [t.module_ ^ "__code"]
    ]
  ; List.map (fun global ->
      Rocq.opaque LocalityGlobal (Fmt.str "%s_%s" t.module_ global)
    ) t.values
  ]
