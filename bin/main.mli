type arguments =
  { input: string
  ; output: string
  ; force: bool
  ; quiet: bool
  ; ignore: string list
  ; only: string list
  ; describe: bool
  }

val main :
  arguments -> unit
