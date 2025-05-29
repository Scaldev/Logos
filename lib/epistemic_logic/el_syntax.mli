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

(**
  [string_of_fmla f] returns the string representation of [f].
*)
val string_of_fmla : fmla -> string

(**
  [modal_depth f] returns the modal depth of the formula [f].
*)
val modal_depth_of_fmla : fmla -> int

(**
  [size_of_fmla f] returns the size of the formula [f].
*)
val size_of_fmla : fmla -> int

(**
  [max_ap_in_fmla f] returns the maximum int for an atomic proposition in [f].
*)
val max_ap_in_fmla : fmla -> int

(**
  [max_ag_in_fmla f] returns the maximum int for an agent in [f].
*)
val max_ag_in_fmla : fmla -> int

(**
  [default_legend f] returns a [(aps, ags)] pair legend for a formula,
  mapping each atomic proposition and agent index to its string representation, that is:
  [i -> string_of_int i].
*)
val default_legend : fmla -> string array * string array

(**
  Preconditions:
    - [max_ap_in_fmla f = Array.length aps - 1]
    - [max_ag_in_fmla f = Array.length ags - 1]

  [pp_of_fmla aps ags f] returns a pretty representation of [f],
  using [aps] and [ags] to stringify atomic propositions and agents
  indexes.

*)
val pp_of_fmla : string array -> string array -> fmla -> string