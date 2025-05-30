module StringSet = Set.Make(String)

(*************************************************************************)
(*                                Syntax                                 *)
(*************************************************************************)

type binop = And | Or | Imp | Eq

type fmla =
  | True
  | False
  | AP of string
  | Not of fmla
  | Bin of fmla * binop * fmla
  | Know of string * fmla

(*****************************************************************************)

let string_of_binop (op: binop) : string =
  match op with
  | And -> "And"
  | Or  -> "Or"
  | Imp -> "Imp"
  | Eq  -> "Eq"

let rec string_of_fmla (f: fmla) : string =
  match f with
  | True           -> "True"
  | False          -> "False"
  | AP p           -> "AP \"" ^ p ^ "\""
  | Not g          -> "Not (" ^ string_of_fmla g ^ ")"
  | Bin (g, op, h) -> "Bin (" ^ string_of_fmla g ^ ", " ^ string_of_binop op ^ ", " ^ string_of_fmla h ^ ")"
  | Know (a, g)    -> "Know (\"" ^ a ^ "\", " ^ string_of_fmla g ^ ")"

(*****************************************************************************)

let aps_of_fmla (f: fmla) : string list =
  let rec aux (f: fmla) : StringSet.t =
    match f with
    | Not (g)       -> aux g
    | Bin (g, _, h) -> StringSet.union (aux g) (aux h)
    | Know (_, g)   -> aux g
    | AP p          -> StringSet.singleton p
    | _             -> StringSet.empty
  in StringSet.to_list (aux f)

let ags_of_fmla (f: fmla) : string list =
  let rec aux (f: fmla) : StringSet.t =
    match f with
    | Not (g)       -> aux g
    | Bin (g, _, h) -> StringSet.union (aux g) (aux h)
    | Know (a, g)   -> StringSet.add a (aux g)
    | _             -> StringSet.empty
  in StringSet.to_list (aux f)
  
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

(**
  [pp_of_binop op] returns the string representation of binary operator [op].
*)
let pp_of_binop (op: binop) : string =
  match op with
  | And -> "\u{2227}"
  | Or  -> "\u{2228}"
  | Imp -> "\u{2192}"
  | Eq  -> "\u{2194}"

let rec pp_of_fmla (f: fmla) : string =
  match f with
  | True           -> "⊤"
  | False          -> "⊥"
  | AP p           -> p
  | Not g          -> "\u{00ac}" ^ pp_of_fmla g
  | Bin (g, op, h) -> "(" ^ pp_of_fmla g ^ " " ^ pp_of_binop op ^ " " ^ pp_of_fmla h ^ ")"
  | Know (a, g)    -> "K_" ^ a ^ " " ^ pp_of_fmla g ^ ""