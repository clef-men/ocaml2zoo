open Implementation

val raisings :
  Path.Set.t

type application =
  | Raising of expression
  | Nonraising of ((Types.type_expr * expression) list -> expression option)
val applications :
  application Path.Map.t

val values :
  expression Path.Map.t

val constant_constructors :
  expression Longident.Map.t

val types :
  string array
