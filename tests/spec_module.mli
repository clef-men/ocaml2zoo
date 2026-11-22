[@@@zoo{|
  predicate p :
    int -> iprop
|}]

module M : sig
  [@@@zoo{|
    predicate p1 :
      int -> iprop

    predicate p2 :
      int -> iprop
  |}]

  val test1 :
    int -> unit
  [@@zoo.spec{|
    parameters
      x
    requires
      p1 x
    , p2 x
  |}]

  module N : sig
    [@@@zoo{|
      predicate p :
        int -> iprop
    |}]

    val test :
      int -> unit
    [@@zoo.spec{|
      parameters
        x
      requires
        p1 x
      , p2 x
      , p x
    |}]
  end

  val test2 :
    int -> unit
  [@@zoo.spec{|
    parameters
      x
    requires
      N.p x
  |}]
end

val test :
  int -> unit
[@@zoo.spec{|
  parameters
    x
  requires
    p x
  , M.p1 x
  , M.p2 x
  , M.N.p x
|}]
