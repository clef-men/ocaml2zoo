type t =
  unit -> int

let create () =
  let gen = ref 0 in
  fun () ->
    let i = !gen in
    gen := i + 1 ;
    i

let next t =
  t ()
