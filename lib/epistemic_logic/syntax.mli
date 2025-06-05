type binop = And | Or | Imp | Eq

(**
  [fmla] describes an epistemic formula. Atomic proposition integers and
  agent integers should be >= 0.
*)
type fmla =
    True
  | False
  | AP of int
  | Not of fmla
  | Bin of fmla * binop * fmla
  | Know of int * fmla

(**
  [context] gives a better string representation for atomic propositions
  and agents.
*)
type context = {
  aps: string array;
  ags: string array;
}

(*****************************************************************************)

(**
  [max_ap_in_fmla f] returns the greatest atomic proposition integer in [f].
*)
val max_ap_in_fmla : fmla -> int

(**
  [max_ag_in_fmla f] returns the greatest agent integer in [f].
*)
val max_ag_in_fmla : fmla -> int

(**
  [reduce_fmla f] returns the formula [f] whose atomic propositions
  are between 1 and max_ap_in_fmla f, and whose agents are between 1
  and max_ag_in_fmla f.
*)
val reduce_fmla : fmla -> fmla

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
  [pp_of_fmla c f] returns the pretty-print of [f] given the
  context [c].
*)
val pp_of_fmla : context -> fmla -> string