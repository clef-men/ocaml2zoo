let pp ppf (t : Interface.t) =
  Fmt.pf ppf "@[<v>" ;
  Fmt.pf ppf "From %s Require Import@,  %s__code.@,@,"
    t.library
    t.module_ ;
  Fmt.(list @@ fun ppf -> pf ppf "#[global] Opaque %s_%s." t.module_) ppf t.values ;
  Fmt.pf ppf "@]@."
