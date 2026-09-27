type t =
  string

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
  let while_ =
    "zoo.program_logic.while_"
end

let self =
  "."

let make ?mode lib mod_ =
  let mode = Option.fold ~none:"" ~some:Mode.to_string mode in
  Printf.sprintf "%s.%s%s" lib mod_ mode

let compare =
  String.compare

let to_rocq ~require_kind t =
  Rocq.require require_kind t
