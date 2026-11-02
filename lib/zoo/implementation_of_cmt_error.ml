module Unsupported = struct
  module Shadowing = struct
    type t =
      | Constructor
      | Label

    let to_string = function
      | Constructor ->
          "constructor"
      | Label ->
          "label"
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
    | Argument_optional
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
    | Argument_optional ->
        "optional function argument"
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

type t =
  | Envaux of Envaux.error
  | Unsupported of Unsupported.t
  | Overwrite of Attribute.overwrite_kind * Overwrite.t

let pp ppf = function
  | Envaux err ->
      Fmt.pf ppf "internal Envaux error: %a"
        Envaux.report_error err
  | Unsupported unsupported ->
      Fmt.pf ppf "unsupported feature: %a"
        Unsupported.pp unsupported
  | Overwrite (kind, err) ->
      Fmt.pf ppf {|attribute "%s%s": %a|}
        Attribute.overwrite
        (Attribute.overwrite_kind_to_string kind)
        (Overwrite.pp kind) err

exception Error of Location.t * t

let raise ~loc t =
  raise @@ Error (loc, t)
