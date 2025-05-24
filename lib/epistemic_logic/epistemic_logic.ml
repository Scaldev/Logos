type fmla =
  | True
  | False
  | AP of int
  | Not of fmla
  | And of fmla * fmla
  | Or of fmla * fmla
  | Imp of fmla * fmla
  | Eq of fmla * fmla
  | Know of int * fmla

type kfmla =
  | AP of int
  | Not of kfmla
  | And of kfmla * kfmla
  | Know of int * kfmla

(**
  @param f a formula.
  @return f' such that f' is in the language of epistemic logic.
*)
let rec el_form_of (f: fmla) : kfmla =
  match f with
  | True        -> el_form_of (Or (AP 0, Not (AP 0)))
  | False       -> And (AP 0, Not (AP 0))
  | AP p        -> AP p
  | Not g       -> Not (el_form_of g)
  | And (g, h)  -> And (el_form_of g, el_form_of h)
  | Or (g, h)   -> el_form_of (Not (And (Not g, Not h)))
  | Imp (g, h)  -> el_form_of (Or (Not g, h))
  | Eq (g, h)   -> el_form_of (And (Imp (g, h), Imp (h, g)))
  | Know (a, g) -> Know (a, el_form_of g)

(**
  @param f an epistemic formula.
  @return the modal depth of f.
*)
let rec modal_depth (f: kfmla) : int =
  match f with
  | AP (_)      -> 0
  | Not (g)     -> modal_depth g
  | And (g, h)  -> max (modal_depth g) (modal_depth h)
  | Know (_, g) -> 1 + modal_depth g

(**

*)
let rec size (f: kfmla) : int =
  match f with
  | AP(_)      -> 1
  | Not(g)     -> 1 + size g
  | And(g, h)  -> 1 + size g + size h
  | Know(_, g) -> 1 + size g