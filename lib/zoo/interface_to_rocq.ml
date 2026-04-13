let transl (t : Interface.t) =
  List.concat
  [ [ Rocq.require RequireImport t.library [t.module_ ^ "__code"]
    ; Rocq.newline
    ]
  ; List.map (fun global ->
      Rocq.opaque LocalityGlobal (Fmt.str "%s%s%s" t.module_ Common.separator global)
    ) t.values
  ]
