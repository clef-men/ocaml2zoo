[@@@zoo{|
|}]

[@@@zoo{|
  predicate p1 :
    iprop

  predicate p2 :
    int -> iprop
  : persistent

  predicate p3 :
    val -> bool -> iprop
  : exclusive

  predicate p4 :
    unit -> int -> val -> iprop
  : persistent
    exclusive
|}]

[@@@zoo{|
  predicate p5 :
    (int -> iprop) -> iprop
|}]
