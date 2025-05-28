(*************************************************************************)
(*                                Syntax                                 *)
(*************************************************************************)

type binop = And | Or | Imp | Eq

let string_of_binop (op: binop) : string =
  match op with
  | And -> "And"
  | Or  -> "Or"
  | Imp -> "Imp"
  | Eq  -> "Eq"

let pp_of_binop (op: binop) : string =
  match op with
  | And -> "\u{2227}"
  | Or  -> "\u{2228}"
  | Imp -> "\u{2192}"
  | Eq  -> "\u{2194}"

(*************************************************************************)

type fmla =
  | True
  | False
  | AP of int
  | Not of fmla
  | Bin of fmla * binop * fmla
  | Know of int * fmla

(**
  [string_of_fmla f] returns the string representation of [f].
*)
let rec string_of_fmla (f: fmla) =
  match f with
  | True           -> "True"
  | False          -> "False"
  | AP x           -> "AP(" ^ string_of_int x ^ ")"
  | Not g          -> "Not(" ^ string_of_fmla g ^ ")"
  | Bin (g, op, h) -> "Bin(" ^ string_of_fmla g ^ ", " ^ string_of_binop op ^ ", " ^ string_of_fmla h ^ ")"
  | Know (i, g)    -> "Know(" ^ string_of_int i ^ ", " ^ string_of_fmla g ^ ")"

(**
  [pp_of_fmla f] returns a pretty representation of [f].
*)
let rec pp_of_fmla (f: fmla) : string =
  match f with
  | True           -> "\u{22a4}"
  | False          -> "\u{22a5}"
  | AP x           -> "p_" ^ string_of_int x
  | Not g          -> "\u{00ac}" ^ pp_of_fmla g
  | Bin (g, op, h) -> "(" ^ pp_of_fmla g ^ " " ^ pp_of_binop op ^ " " ^ pp_of_fmla h ^ ")"
  | Know (i, g)    -> "K_" ^ string_of_int i ^ " " ^ pp_of_fmla g ^ ""


(*************************************************************************)

(**
  [modal_depth f] returns the modal depth of the formula [f].
*)
let rec modal_depth_of_fmla (f: fmla) : int =
  match f with
  | Not (g)       -> modal_depth_of_fmla g
  | Bin (g, _, h) -> max (modal_depth_of_fmla g) (modal_depth_of_fmla h)
  | Know (_, g)   -> 1 + modal_depth_of_fmla g
  | _             -> 0

(**
  [size_of_fmla f] returns the size of the formula [f].
*)
let rec size_of_fmla (f: fmla) : int =
  match f with
  | Not(g)        -> 1 + size_of_fmla g
  | Bin (g, _, h) -> 1 + size_of_fmla g + size_of_fmla h
  | Know(_, g)    -> 1 + size_of_fmla g
  | _             -> 1

(**
  [max_ap_in_fmla f] returns the maximum int for an atomic proposition in [f].
*)
let rec max_ap_in_fmla (f: fmla) : int =
  match f with
  | AP(i)         -> i
  | Not(g)        -> max_ap_in_fmla g
  | Bin (g, _, h) -> max (max_ap_in_fmla g) (max_ap_in_fmla h)
  | Know(_, g )   -> max_ap_in_fmla g
  | _             -> 0

(**
  [max_ag_in_fmla f] returns the maximum int for an agent in [f].
*)
let rec max_ag_in_fmla (f: fmla) : int =
  match f with
  | Not(g)        -> max_ag_in_fmla g
  | Bin (g, _, h) -> max (max_ag_in_fmla g) (max_ag_in_fmla h)
  | Know(a, g)    -> max a (max_ag_in_fmla g)
  | _             -> 0