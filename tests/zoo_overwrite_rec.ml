type t1 =
  | A

let test1 =
  object end
[@@zoo.overwrite_rec
  fun () -> test1 ()
]

let test2 =
  object end
[@@zoo.overwrite_rec
  fun () -> A
]

module Test = struct
  type t =
    | B

  let test1 =
    object end
  [@@zoo.overwrite_rec
    fun () -> A, B
  ]
end

module Opened = struct
  type t =
    | C
end

type t2 =
  | D

open Opened

let test3 =
  object end
[@@zoo.overwrite
  fun () -> C, D
]
