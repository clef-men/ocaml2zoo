let test1 =
  object end
[@@zoo.overwrite_raw "foo.bar.raw"]

let test2 () =
  object end
[@@zoo.overwrite_raw "foo.bar.raw"]
