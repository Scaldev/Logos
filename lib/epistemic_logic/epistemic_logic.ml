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

let rec equal_kfmla (f1: kfmla) (f2: kfmla) =
  match f1, f2 with
  | AP p, AP q                   -> p = q
  | Not g1, Not g2               -> equal_kfmla g1 g2
  | And (g1, h1), And (g2, h2)   -> equal_kfmla g1 g2 && equal_kfmla h1 h2
  | Know (i1, g1), Know (i2, g2) -> i1 = i2 && equal_kfmla g1 g2
  | _, _ -> false

let rec string_of_kfmla (f: kfmla) =
  match f with
  | AP x        -> "AP(" ^ string_of_int x ^ ")"
  | Not g       -> "Not(" ^ string_of_kfmla g ^ ")"
  | And (g, h)  -> "And(" ^ string_of_kfmla g ^ ", " ^ string_of_kfmla h ^ ")"
  | Know (i, g) -> "Know(" ^ string_of_int i ^ ", " ^ string_of_kfmla g ^ ")"

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


let rec modal_depth (f: kfmla) : int =
  match f with
  | AP (_)      -> 0
  | Not (g)     -> modal_depth g
  | And (g, h)  -> max (modal_depth g) (modal_depth h)
  | Know (_, g) -> 1 + modal_depth g


let rec size (f: kfmla) : int =
  match f with
  | AP(_)      -> 1
  | Not(g)     -> 1 + size g
  | And(g, h)  -> 1 + size g + size h
  | Know(_, g) -> 1 + size g