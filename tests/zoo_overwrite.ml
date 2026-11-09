type t1 =
  | A

let test1 =
  object end
[@@zoo.overwrite
  fun () -> ()
]

let test2 =
  object end
[@@zoo.overwrite
  fun () -> A
]

module Test = struct
  type t =
    | B

  let test1 =
    object end
  [@@zoo.overwrite
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
