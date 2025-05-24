type fmla =
    True
  | False
  | AP of int
  | Not of fmla
  | And of fmla * fmla
  | Or of fmla * fmla
  | Imp of fmla * fmla
  | Eq of fmla * fmla
  | Know of int * fmla

type kfmla =
    AP of int
  | Not of kfmla
  | And of kfmla * kfmla
  | Know of int * kfmla

(**
  [equal_kfmla f1 f2] returns [true] iff f1 and f2 have the same AST.
*)
val equal_kfmla : kfmla -> kfmla -> bool

(**
  [string_of_kfmla f] returns the string representation of f.
*)
val string_of_kfmla : kfmla -> string

(**
  [el_form_of f] returns an equivalent formula [f'] such that
  [f'] is in the language of epistemic logic.
*)
val el_form_of : fmla -> kfmla

(**
  [modal_depth f] returns the modal depth of [f].
*)
val modal_depth : kfmla -> int

(**
  [size f] returns the size of [f].
*)
val size : kfmla -> int
