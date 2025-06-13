type t =
  Parsetree.attribute

let has attr =
  List.exists (fun attr' -> attr'.Parsetree.attr_name.txt = attr)

let ignore =
  "zoo.ignore"
let has_ignore =
  has ignore

let prefix =
  "zoo.prefix"
let has_prefix =
  has prefix

let force_record =
  "zoo.force_record"
let has_force_record =
  has force_record

let reveal =
  "zoo.reveal"
let has_reveal =
  has reveal

let opaque =
  "zoo.opaque"
let has_opaque =
  has opaque

type overwrite_kind =
  | Overwrite of Asttypes.rec_flag
  | Raw
let overwrite_kind_to_string = function
  | Raw ->
      "_raw"
  | Overwrite rec_ ->
      match rec_ with
      | Nonrecursive ->
          ""
      | Recursive ->
          "_rec"
let overwrite =
  "zoo.overwrite"
let rec has_overwrite = function
  | [] ->
      None
  | attr :: attrs ->
      let attr_name = attr.Parsetree.attr_name.txt in
      if String.starts_with ~prefix:overwrite attr_name then
        let overwrite_len = String.length overwrite in
        let attr_len = String.length attr_name in
        match String.sub attr_name overwrite_len (attr_len - overwrite_len) with
        | "" ->
            Some (Overwrite Nonrecursive, attr)
        | "_rec" ->
            Some (Overwrite Recursive, attr)
        | "_raw" ->
            Some (Raw, attr)
        | _ ->
            has_overwrite attrs
      else
        has_overwrite attrs
