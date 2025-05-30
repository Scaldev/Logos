let (<<) = Fun.compose

(*************************************************************************)
(*                                Syntax                                 *)
(*************************************************************************)

type binop = And | Or | Imp | Eq

type fmla =
  | True
  | False
  | AP of int
  | Not of fmla
  | Bin of fmla * binop * fmla
  | Know of int * fmla

(*****************************************************************************)

let string_of_binop (op: binop) : string =
  match op with
  | And -> "And"
  | Or  -> "Or"
  | Imp -> "Imp"
  | Eq  -> "Eq"

let rec string_of_fmla (f: fmla) =
  match f with
  | True           -> "True"
  | False          -> "False"
  | AP x           -> "AP(" ^ string_of_int x ^ ")"
  | Not g          -> "Not(" ^ string_of_fmla g ^ ")"
  | Bin (g, op, h) -> "Bin(" ^ string_of_fmla g ^ ", " ^ string_of_binop op ^ ", " ^ string_of_fmla h ^ ")"
  | Know (i, g)    -> "Know(" ^ string_of_int i ^ ", " ^ string_of_fmla g ^ ")"

(*****************************************************************************)

let rec modal_depth_of_fmla (f: fmla) : int =
  match f with
  | Not (g)       -> modal_depth_of_fmla g
  | Bin (g, _, h) -> max (modal_depth_of_fmla g) (modal_depth_of_fmla h)
  | Know (_, g)   -> 1 + modal_depth_of_fmla g
  | _             -> 0

let rec size_of_fmla (f: fmla) : int =
  match f with
  | Not(g)        -> 1 + size_of_fmla g
  | Bin (g, _, h) -> 1 + size_of_fmla g + size_of_fmla h
  | Know(_, g)    -> 1 + size_of_fmla g
  | _             -> 1

(*****************************************************************************)

let rec max_ap_in_fmla (f: fmla) : int =
  match f with
  | AP(i)         -> i
  | Not(g)        -> max_ap_in_fmla g
  | Bin (g, _, h) -> max (max_ap_in_fmla g) (max_ap_in_fmla h)
  | Know(_, g )   -> max_ap_in_fmla g
  | _             -> 0

let rec max_ag_in_fmla (f: fmla) : int =
  match f with
  | Not(g)        -> max_ag_in_fmla g
  | Bin (g, _, h) -> max (max_ag_in_fmla g) (max_ag_in_fmla h)
  | Know(a, g)    -> max a (max_ag_in_fmla g)
  | _             -> 0

(*****************************************************************************)

(**
  [legend pre n] returns an array of size [n+1] with elements
  [pre ^ "0"], [pre ^ "1"], ..., [pre ^ (string_of_int n)].
*)
let legend (pre: string) (n: int) =
  Array.init (n+1) (((^) pre) << string_of_int)

let default_legend (f: fmla) : string array * string array =
  let aps = max_ap_in_fmla f |> legend "p_" in
  let ags = max_ag_in_fmla f |> legend ""   in
  (aps, ags)
  
(*****************************************************************************)

(**
  [pp_of_binop op] returns the string representation of binary operator [op].
*)
let pp_of_binop (op: binop) : string =
  match op with
  | And -> "\u{2227}"
  | Or  -> "\u{2228}"
  | Imp -> "\u{2192}"
  | Eq  -> "\u{2194}"

let rec pp_of_fmla (aps: string array) (ags: string array) (f: fmla) : string =
  match f with
  | True           -> "⊤"
  | False          -> "⊥"
  | AP i           -> aps.(i)
  | Not g          -> "\u{00ac}" ^ pp_of_fmla aps ags g
  | Bin (g, op, h) -> "(" ^ pp_of_fmla aps ags g ^ " " ^ pp_of_binop op ^ " " ^ pp_of_fmla aps ags h ^ ")"
  | Know (i, g)    -> "K_" ^ ags.(i) ^ " " ^ pp_of_fmla aps ags g ^ ""