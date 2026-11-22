module Unsupported : sig
  module Shadowing : sig
    type t =
      | Constructor
      | Label
      | Predicate
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
end

module Overwrite : sig
  type t =
    | Invalid
    | Typing
end

module Annotation : sig
  module Lexing : sig
    type t =
      | Illegal_character of char
      | Invalid_integer_literal
  end

  module Unbound : sig
    type t =
      | Module of Longident.t
      | Predicate of Longident.t
      | Type of Longident.t
      | Variable of string
  end

  module Typing : sig
    type t =
      | Ident of string
      | Return
      | Expr
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
end

type t =
  | Envaux of Ocaml_common.Envaux.error
  | Unsupported of Unsupported.t
  | Overwrite of Attribute.overwrite_kind * Overwrite.t
  | Annotation of Annotation.t

val pp :
  t Fmt.t

exception Error of Location.t * t

val raise :
  loc:Location.t -> t -> 'a
