include Stdlib.Stack

let of_list xs =
  xs
  |> List.to_seq
  |> of_seq
