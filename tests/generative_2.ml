type 'a t =
  | A of 'a [@generative] [@zoo.generative_strong]
  | B of 'a

let test () =
  let _ = A () in
  let _ = B () in
  ()
