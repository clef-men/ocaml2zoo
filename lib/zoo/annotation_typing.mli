val transl_declarations :
  ctx:Context.t ->
  Annotation_parsetree.declarations ->
  Annotation.item list

val transl_spec :
  ctx:Context.t ->
  ty:Types.type_expr ->
  Annotation_parsetree.spec ->
  Annotation.spec
