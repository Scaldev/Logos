type binop = And | Or | Imp | Eq

type fmla =
    True
  | False
  | AP of string
  | Not of fmla
  | Bin of fmla * binop * fmla
  | Know of string * fmla

(**
  [aps_of_fmla f] returns the atomic propositions in [f].
*)
val aps_of_fmla : fmla -> string list

(**
  [ags_of_fmla f] returns the agents in [f].
*)
val ags_of_fmla : fmla -> string list

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
  Preconditions:
    - [max_ap_in_fmla f = Array.length aps - 1]
    - [max_ag_in_fmla f = Array.length ags - 1]

  [pp_of_fmla aps ags f] returns a pretty representation of [f],
  using [aps] and [ags] to stringify atomic propositions and agents
  indexes.

*)
val pp_of_fmla : fmla -> string