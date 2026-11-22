{

open Annotation_parser

let error lexbuf err =
  let loc = Location.curr lexbuf in
  Error.raise ~loc (Annotation (Lexing err))

let keywords =
  [|"ensures", ENSURES
  ; "parameters", PARAMETERS
  ; "predicate", PREDICATE
  ; "requires", REQUIRES
  ; "return", RETURN
  ; "specification", SPECIFICATION
  |]
let keywords =
  let tbl = Hashtbl.create () in
  keywords |> Array.iter (fun (str, token) ->
    Hashtbl.add tbl str token
  ) ;
  tbl

let find_keyword id =
  try
    Hashtbl.find keywords id
  with Not_found ->
    LIDENT id

}

let newline =
  ('\013'* '\010')
let blank =
  [' ' '\009' '\012']

let uppercase =
  ['A'-'Z']
let lowercase =
  ['a'-'z' '_']
let ident_char =
  ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let uident =
  uppercase ident_char*
let lident =
  lowercase ident_char*

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

rule token = parse
  | newline
      { Lexing.new_line lexbuf ;
        token lexbuf
      }
  | blank+
      { token lexbuf }
  | "->"
      { LARROW }
  | ':'
      { COLON }
  | ','
      { COMMA }
  | '.'
      { DOT }
  | "≤"
      { LE }
  | '('
      { LPAREN }
  | '<'
      { LT }
  | '-'
      { MINUS }
  | '+'
      { PLUS }
  | ')'
      { RPAREN }
  | '*'
      { TIMES }
  | uident as id
      { UIDENT id }
  | lident as id
      { find_keyword id }
  | int_literal as lit
      { try
          INT (int_of_string lit)
        with Failure _ ->
          error lexbuf Invalid_integer_literal
      }
  | eof
      { EOF }
  | (_ as chr)
      { error lexbuf (Illegal_character chr) }
