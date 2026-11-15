include Stdlib.Format

type 'a t =
  formatter -> 'a -> unit

let rec pp_print_list ~sep pp ppf i = function
  | [] ->
      ()
  | x :: xs ->
      sep ppf () ;
      pp ppf i x ;
      pp_print_list ~sep pp ppf (i + 1) xs
let pp_print_list ?(sep = pp_print_cut) pp ppf = function
  | [] ->
      ()
  | x :: xs ->
      pp ppf 0 x ;
      pp_print_list ~sep pp ppf 1 xs
