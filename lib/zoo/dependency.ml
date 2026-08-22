type t =
  string

let make ?(suff = "") lib mod_ =
  Printf.sprintf "%s.%s%s" lib mod_ suff

module Builtin = struct
  let assert_ =
    "zoo.program_logic.assert"
  let assume =
    "zoo.program_logic.assume"
  let diverge =
    "zoo.program_logic.diverge"
  let for_ =
    "zoo.program_logic.for_"
  let identifier =
    "zoo.program_logic.identifier"
  let structeq =
    "zoo.program_logic.structural_equality"
end
