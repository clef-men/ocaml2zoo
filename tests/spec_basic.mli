[@@@zoo{|
  predicate p1 :
    int -> iprop

  predicate p2 :
    val -> int -> iprop

  predicate p3 :
    bool -> iprop
|}]

val test1 :
  unit -> unit
[@@zoo.spec{|
|}]

val test2 :
  unit -> unit
[@@zoo.spec{|
  specification
    one
|}]

val test3 :
  unit -> unit
[@@zoo.spec{|
  parameters
    ()
|}]

val test4 :
  int -> int -> int
[@@zoo.spec{|
  specification
    one
  parameters
    x1 x2
  requires
    0 ≤ x1 < 10
  , p1 x2
  ensures
    x1 + x2 ≤ return
  , return ≤ x1 + x2
|}]

val test5 :
  'a -> int -> unit
[@@zoo.spec{|
  parameters
    v n
  requires
    p2 v n
  ensures
    p2 v (n + 1)
|}]

val test6 :
  int -> int -> int
[@@zoo.spec{|
  parameters
    x
  requires
    p1 x
|}]

val test7 :
  int -> bool
[@@zoo.spec{|
  parameters
    x
  ensures
    p3 return
|}]

val test8 :
  unit -> int
[@@zoo.spec{|
  specification
    one
  requires
    p1 n
  ensures
    p1 m
  , n < m
|}]
[@@zoo.spec{|
  specification
    two
  parameters
    ()
  ensures
    0x10 * 2 - 1 < return + 3
|}]
