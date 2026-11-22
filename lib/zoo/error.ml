module Unsupported = struct
  module Shadowing = struct
    type t =
      | Constructor
      | Label
      | Predicate

    let to_string = function
      | Constructor ->
          "constructor"
      | Label ->
          "label"
      | Predicate ->
          "predicate"
  end

  type t =
    | Literal_non_integer
    | Pattern_alias
    | Pattern_constant
    | Pattern_variant
    | Pattern_record
    | Pattern_array
    | Pattern_or
    | Pattern_lazy
    | Pattern_guard
    | Pattern_constr
    | Pattern_nested
    | Pattern_invalid
    | Pattern_non_trivial
    | Default_variable
    | Handler_exception
    | Expr_let_rec_non_function
    | Expr_let_mutual
    | Expr_for_downward
    | Expr_array
    | Expr_try
    | Expr_variant
    | Expr_send
    | Expr_new
    | Expr_inst_var
    | Expr_set_inst_var
    | Expr_overwrite
    | Expr_lazy
    | Expr_object
    | Expr_pack
    | Expr_let_op
    | Expr_unreachable
    | Expr_extension
    | Expr_struct_item
    | Argument_omitted
    | Module_packed
    | Functor
    | Type_extensible
    | Type_external
    | Decl_recursive
    | Decl_invalid
    | Decl_pattern
    | Decl_eval
    | Decl_external
    | Decl_exception
    | Decl_module_unnamed
    | Decl_module_rec
    | Decl_module_alias
    | Decl_module_type
    | Decl_class
    | Decl_class_type
    | Decl_include
    | Open
    | Shadowing of Shadowing.t

  let to_string = function
    | Literal_non_integer ->
        "non-integer literal"
    | Pattern_alias ->
        {|"as" pattern|}
    | Pattern_constant ->
        "constant pattern"
    | Pattern_variant ->
        "variant pattern"
    | Pattern_record ->
        "invalid record pattern"
    | Pattern_array ->
        "array pattern"
    | Pattern_or ->
        "disjunction pattern"
    | Pattern_lazy ->
        {|"lazy" pattern|}
    | Pattern_guard ->
        "guard expression"
    | Pattern_constr ->
        "invalid constructor pattern"
    | Pattern_nested ->
        "nested pattern"
    | Pattern_invalid ->
        "invalid pattern"
    | Pattern_non_trivial ->
        "non-trivial pattern in function parameter"
    | Default_variable ->
        "default value of optional parameter refers to a local variable"
    | Handler_exception ->
        "exception handler"
    | Expr_let_rec_non_function ->
        "recursive binding must bind a function"
    | Expr_let_mutual ->
        "mutually recursive let-bindings"
    | Expr_for_downward ->
        {|downward "for" loop|}
    | Expr_array ->
        "array expression"
    | Expr_try ->
        {|"try" expression|}
    | Expr_variant ->
        "variant expression"
    | Expr_send ->
        "method call"
    | Expr_new ->
        {|"new" expression|}
    | Expr_inst_var ->
        "instance variable"
    | Expr_set_inst_var ->
        "instance variable assignment"
    | Expr_overwrite ->
        "overwrite expression"
    | Expr_lazy ->
        {|"lazy" expression|}
    | Expr_object ->
        "object expression"
    | Expr_pack ->
        "module expression"
    | Expr_let_op ->
        "binding operator"
    | Expr_unreachable ->
        "unreachable branch"
    | Expr_extension ->
        "extension"
    | Argument_omitted ->
        "omitted function argument"
    | Module_packed ->
        "first-class module"
    | Functor ->
        "module functor"
    | Type_extensible ->
        "extensible variant"
    | Type_external ->
        "external type"
    | Decl_recursive ->
        "recursive toplevel declaration must be a function"
    | Decl_invalid ->
        "toplevel declaration must be a constant or a function"
    | Decl_pattern ->
        "toplevel declaration pattern must be a variable"
    | Decl_eval ->
        "evaluated expression"
    | Decl_external ->
        "external declaration"
    | Decl_exception ->
        "exception declaration"
    | Decl_module_unnamed ->
        "unnamed module"
    | Decl_module_rec ->
        "recursive module"
    | Decl_module_alias ->
        "module alias"
    | Decl_module_type ->
        "module type declaration"
    | Decl_class ->
        "class declaration"
    | Decl_class_type ->
        "class type declaration"
    | Decl_include ->
        {|"include" declaration|}
    | Open ->
        "opened module must be an identifier"
    | Shadowing shadowing ->
        Printf.sprintf "%s shadowing"
          (Shadowing.to_string shadowing)
    | Expr_struct_item ->
        "unsupported local structure item (only basic open statements are supported)"

  let pp ppf t =
    Fmt.string ppf (to_string t)
end

module Overwrite = struct
  type t =
    | Invalid
    | Typing

  let pp (kind : Attribute.overwrite_kind) ppf = function
    | Invalid ->
        Fmt.pf ppf "payload must be %s"
          begin match kind with
          | Overwrite _ ->
              "an expression"
          | Raw ->
              "of the form library.module.identifier"
          end
    | Typing ->
        Fmt.pf ppf "cannot infer type"
end

module Annotation = struct
  module Lexing = struct
    type t =
      | Illegal_character of char
      | Invalid_integer_literal

    let pp ppf = function
      | Illegal_character chr ->
          Fmt.pf ppf "illegal character: %c"
            chr
      | Invalid_integer_literal ->
          Fmt.pf ppf "invalid integer literal"
  end

  module Unbound = struct
    type t =
      | Module of Longident.t
      | Predicate of Longident.t
      | Type of Longident.t
      | Variable of string

    let pp ppf = function
      | Module lid ->
          Fmt.pf ppf "unbound module: %a"
            Longident.pp lid
      | Predicate lid ->
          Fmt.pf ppf "unbound predicate: %a"
            Longident.pp lid
      | Type lid ->
          Fmt.pf ppf "unbound ghost type: %a"
            Longident.pp lid
      | Variable id ->
          Fmt.pf ppf "unbound variable in spec: %s"
            id
  end

  module Typing = struct
    type t =
      | Ident of string
      | Return
      | Expr

    let pp ppf = function
      | Ident id ->
          Fmt.pf ppf "identifier %s"
            id
      | Return ->
          Fmt.pf ppf "return"
      | Expr ->
          Fmt.pf ppf "expression"
  end

  type t =
    | Lexing of Lexing.t
    | Parsing
    | Unbound of Unbound.t
    | Predicate_return
    | Predicate_property of string
    | Predicate_arity of
      { predicate: Lpath.t
      ; arity: int
      ; num_argument: int
      }
    | Spec_unit_parameter
    | Spec_duplicate_parameter of string
    | Spec_too_many_parameters
    | Typing of
      { kind: Typing.t
      ; expected: Annotation.typ
      ; inferred: Annotation.typ
      }

  let pp ppf = function
    | Lexing err ->
        Fmt.pf ppf "lexing error: %a"
          Lexing.pp err
    | Parsing ->
        Fmt.pf ppf "parsing error"
    | Unbound err ->
        Unbound.pp ppf err
    | Predicate_return ->
        Fmt.pf ppf "predicate return type should be %a"
          Annotation_pp.pp_typ Type_iprop
    | Predicate_property property ->
        Fmt.pf ppf "invalid predicate property: %s"
          property
    | Predicate_arity { predicate; arity; num_argument } ->
        Fmt.pf ppf "too %s arguments for predicate %a"
          (if arity < num_argument then "many" else "few")
          (Lpath.pp ~sep:Separator.dot ?mod_:None) predicate
    | Spec_unit_parameter ->
        Fmt.pf ppf "spec parameter should have type unit"
    | Spec_duplicate_parameter arg ->
        Fmt.pf ppf "duplicate parameter in spec: %s"
          arg
    | Spec_too_many_parameters ->
        Fmt.pf ppf "too many parameters in spec"
    | Typing { kind; expected; inferred } ->
        Fmt.pf ppf "%a should have type %a but has type %a"
          Typing.pp kind
          Annotation_pp.pp_typ expected
          Annotation_pp.pp_typ inferred
end

type t =
  | Envaux of Ocaml_common.Envaux.error
  | Unsupported of Unsupported.t
  | Overwrite of Attribute.overwrite_kind * Overwrite.t
  | Annotation of Annotation.t

let pp ppf = function
  | Envaux err ->
      Fmt.pf ppf "internal Envaux error: %a"
        Ocaml_common.Envaux.report_error err
  | Unsupported unsupported ->
      Fmt.pf ppf "unsupported feature: %a"
        Unsupported.pp unsupported
  | Overwrite (kind, err) ->
      Fmt.pf ppf {|attribute "%s%s": %a|}
        Attribute.overwrite
        (Attribute.overwrite_kind_to_string kind)
        (Overwrite.pp kind) err
  | Annotation err ->
      Annotation.pp ppf err

exception Error of Location.t * t

let raise ~loc t =
  raise @@ Error (loc, t)
