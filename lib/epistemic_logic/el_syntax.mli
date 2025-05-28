(*************************************************************************)
(*                                Syntax                                 *)
(*************************************************************************)

type binop = And | Or | Imp | Eq

type fmla =
    True
  | False
  | AP of int
  | Not of fmla
  | Bin of fmla * binop * fmla
  | Know of int * fmla

val string_of_fmla : fmla -> string

val pp_of_fmla : fmla -> string

val modal_depth_of_fmla : fmla -> int

val size_of_fmla : fmla -> int

val max_ap_in_fmla : fmla -> int

val max_ag_in_fmla : fmla -> int