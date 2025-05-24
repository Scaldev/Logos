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

val el_form_of : fmla -> kfmla

val modal_depth : kfmla -> int

val size : kfmla -> int
