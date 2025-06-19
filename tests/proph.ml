let test1 () =
  let proph = Zoo.proph () in
  Zoo.resolve_with (1 + 1) proph ()

let test2 () =
  let proph = Zoo.proph () in
  Zoo.resolve_silent proph ()

let test3 () =
  let proph = Zoo.proph () in
  Zoo.resolve proph ()
