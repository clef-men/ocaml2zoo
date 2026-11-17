let test1 ?x () =
  match x with
  | None ->
      0
  | Some x ->
      x

let test2 ?(x = 0) ?y:(_ = 1) z =
  x + z

let test3 ?(x = 0) ?(y = 1) () =
  x + y

let test4 ?(x = 0) = function
  | None ->
      x
  | Some y ->
      y

let test5 () =
  let _ = test1 () in
  let _ = test1 ~x:1 () in
  let _ = test1 ?x:None () in
  let _ = test2 2 in
  let _ = test2 ~y:1 2 in
  let _ = test3 ~y:1 () in
  test4 ~x:1 None

let y =
  0
let test6 ?(x = y) y =
  x + y

let test7 ?(x = let y = 1 in y) y =
  x + y

let test8 ?x:(a = 1) ?x2:(b = 2) ?x:(c = 3) () =
  a + b + c

let test9 ?x () =
  test1 ?x ()

let test10 ?x ?z y =
  test2 ?x ?y:z y
