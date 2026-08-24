type t =
  | Types
  | Code
  | Opaque

let to_string = function
  | Types ->
      "__types"
  | Code ->
      "__code"
  | Opaque ->
      "__opaque"
