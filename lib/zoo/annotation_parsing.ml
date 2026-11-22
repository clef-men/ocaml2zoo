let parse ~(loc : Location.t) parser str =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_start_p <- loc.loc_start ;
  lexbuf.lex_curr_p <- loc.loc_start ;
  try
    parser Annotation_lexer.token lexbuf
  with Annotation_parser.Error ->
    let loc = Location.curr lexbuf in
    Error.raise ~loc (Annotation Parsing)
let parse_declarations =
  parse Annotation_parser.declarations
let parse_spec =
  parse Annotation_parser.spec
