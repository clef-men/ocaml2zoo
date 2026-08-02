include Stdlib.List

let rec make n x =
  if n <= 0 then
    []
  else
    x :: make (n - 1) x

let rec interleave ~sep xs =
  match xs with
  | []
  | [_] ->
      xs
  | x :: xs ->
      x :: sep :: interleave ~sep xs
[@@tail_mod_cons]
