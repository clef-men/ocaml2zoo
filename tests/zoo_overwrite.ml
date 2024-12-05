let test1 =
  object end
[@@zoo.overwrite
  fun () -> ()
]

let test2 =
  object end
[@@zoo.overwrite_rec
  fun () -> test2 ()
]

let test3 =
  object end
[@@zoo.overwrite_raw "foo.bar.raw"]

let test4 () =
  object end
[@@zoo.overwrite_raw "foo.bar.raw"]
