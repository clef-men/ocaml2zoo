type 'a t =
  | A of 'a [@generative] [@zoo.reveal]
  | B of 'a

let test () =
  let _ = A () in
  let _ = B () in
  ()
