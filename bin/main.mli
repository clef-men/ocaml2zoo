type arguments =
  { input: string;
    output: string;
    force: bool;
    quiet: bool;
    exclude: string list;
  }

exception Error of unit Cmdliner.Term.ret

val main :
  arguments -> unit
