%{

open Annotation_parsetree

let make_loc startpos endpos : Location.t =
  { loc_start= startpos
  ; loc_end= endpos
  ; loc_ghost= false
  }
let make_located startpos endpos x =
  let loc = make_loc startpos endpos in
  Location.mkloc x loc

%}

%token COLON ":"
%token COMMA ","
%token DOT "."
%token ENSURES "ensures"
%token EOF ""
%token <int> INT "42"
%token LARROW "->"
%token LE "≤"
%token <string> LIDENT "lident"
%token LPAREN "("
%token LT "<"
%token MINUS "-"
%token PARAMETERS "parameters"
%token PLUS "+"
%token PREDICATE "predicate"
%token REQUIRES "requires"
%token RETURN "return"
%token RPAREN ")"
%token SPECIFICATION "specification"
%token TIMES "*"
%token <string> UIDENT "uident"

%right LARROW
%left MINUS PLUS
%left TIMES

%start <declarations> declarations
%start <spec> spec

%%

let located(x) ==
  ~ = x ;
    { make_located $startpos $endpos x }

let int ==
  located(INT)

let uident ==
  located(UIDENT)

let lident ==
  located(LIDENT)

let longident_prefix :=
  located(
    | ~ = UIDENT ;
        < Longident.Lident >
    | ~ = longident_prefix ; DOT ; ~ = uident ;
        < Longident.Ldot >
  )
let longident :=
  located(
    | ~ = LIDENT ;
        < Longident.Lident >
    | ~ = longident_prefix ; DOT ; ~ = lident ;
        < Longident.Ldot >
  )

let typ_atom :=
  located(
    | id = longident ;
        { Type_constr (id, []) }
    | LPAREN ; ty = typ ; RPAREN ;
        { ty.Location.txt }
  )
let typ :=
  located(
    | ~ = longident ; ~ = typ_atom * ;
        < Type_constr >
    | ty1 = typ ; LARROW ; ty2 = typ ;
        < Type_arrow >
    | LPAREN ; ty = typ ; RPAREN ;
        { ty.Location.txt }
  )

let predicate_properties ==
  COLON ; lident +
let predicate :=
  | PREDICATE ; name = lident ; COLON ; ty = typ ; properties = predicate_properties ? ;
      { { predicate_name= name
        ; predicate_type= ty
        ; predicate_properties= Option.value ~default:[] properties
        }
      }

let declaration :=
  | ~ = predicate ;
      < Decl_predicate >

let declarations :=
  | decls = declaration * ; EOF ;
      { decls }

let parameter :=
  located(
    | LPAREN ; RPAREN ;
        { Param_unit }
    | ~ = LIDENT ;
        < Param_ident >
  )

let binop ==
  located(
    | PLUS ;
        { Binop_plus }
    | MINUS ;
        { Binop_minus }
    | TIMES ;
        { Binop_mult }
  )

let comparison ==
  located(
    | LE ;
        { Comparison_le }
    | LT ;
        { Comparison_lt }
  )

let expr_atom :=
  located(
    | ~ = int ;
        < Expr_int >
    | ~ = lident ;
        < Expr_ident >
    | RETURN ;
        { Expr_return }
    | LPAREN ; ~ = expr ; RPAREN ;
        { expr.Location.txt }
  )
let expr :=
  | expr_atom
  | located(
      expr1 = expr ; op = binop ; expr2 = expr ;
        { Expr_binop (op, expr1, expr2) }
    )

let assertion_basic ==
  located(
    | expr1 = expr ; cmp = comparison ; expr2 = expr ;
        { Assertion_comparison (cmp, expr1, expr2) }
    | lid = longident ; args = expr_atom * ;
        { Assertion_predicate (lid, args) }
  )
let assertion :=
  | assertion = assertion_basic ;
      { [assertion] }
  | expr1 = expr ; cmp1 = comparison ; expr2 = expr ; cmp2 = comparison ; expr3 = expr ;
      { let assertion1 = Assertion_comparison (cmp1, expr1, expr2) in
        let assertion1 = make_located $startpos $endpos(expr2) assertion1 in
        let assertion2 = Assertion_comparison (cmp2, expr2, expr3) in
        let assertion2 = make_located $startpos(expr2) $endpos assertion2 in
        [assertion1; assertion2]
      }

let spec_name ==
  SPECIFICATION ; lident
let spec_parameters ==
  PARAMETERS ; parameter *
let spec_requires ==
  REQUIRES ; flatten(separated_list(COMMA, assertion))
let spec_ensures ==
  ENSURES ; flatten(separated_list(COMMA, assertion))
let spec :=
  | name = spec_name ? ;
    params = located(loption(spec_parameters)) ;
    requires = loption(spec_requires) ;
    ensures = loption(spec_ensures) ;
    EOF ;
      { { spec_name= name
        ; spec_parameters= params
        ; spec_requires= requires
        ; spec_ensures= ensures
        }
      }
