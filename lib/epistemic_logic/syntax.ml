module StringSet = Set.Make(String)

(*****************************************************************************)
(*                                  Syntax                                   *)
(*****************************************************************************)

type binop = And | Or | Imp | Eq

type fmla =
  | True
  | False
  | AP of int
  | Not of fmla
  | Bin of fmla * binop * fmla
  | Know of int * fmla

type context = {
  aps: string array;
  ags: string array;
}

(*****************************************************************************)

(**
  [string_of_binop op] returns the string representation of [op].
*)
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
  | AP p           -> "AP " ^ string_of_int p ^ ""
  | Not g          -> "Not (" ^ string_of_fmla g ^ ")"
  | Bin (g, op, h) -> "Bin (" ^ string_of_fmla g ^ ", " ^ string_of_binop op ^ ", " ^ string_of_fmla h ^ ")"
  | Know (a, g)    -> "Know (" ^ string_of_int a ^ ", " ^ string_of_fmla g ^ ")"

(*****************************************************************************)

let rec max_ap_in_fmla (f: fmla) : int =
  match f with
  | AP p          -> p
  | Not (g)       -> max_ap_in_fmla g
  | Bin (g, _, h) -> max (max_ap_in_fmla g) (max_ap_in_fmla h)
  | Know (_, g)   -> max_ap_in_fmla g
  | _             -> -1

(*****************************************************************************)

let rec max_ag_in_fmla (f: fmla) : int =
  match f with
  | Not (g)       -> max_ag_in_fmla g
  | Bin (g, _, h) -> max (max_ag_in_fmla g) (max_ag_in_fmla h)
  | Know (a, g)   -> max a (max_ag_in_fmla g)
  | _             -> -1

(*****************************************************************************)

(**
  [map_fmla aps ags f] returns the formula [f] after mapping each atomic proposition
  integer [p] to [aps.(p)] and each agent integer [a] to [ags.(a)].
*)
let rec map_fmla (aps: int array) (ags: int array) (f: fmla) : fmla =
  match f with
  | True           -> True
  | False          -> False
  | AP p           -> AP aps.(p)
  | Not (g)        -> Not (map_fmla aps ags g)
  | Bin (g, op, h) -> Bin (map_fmla aps ags g, op, map_fmla aps ags h)
  | Know (a, g)    -> Know (ags.(a), map_fmla aps ags g)

(**
  [count_trues arr] returns an [arr'] array of integers, such that [arr.(i)]
  is the [n]-th value of [arr] to be true, left to right.
*)
let count_trues (arr: bool array) : int array =
  let arr' = Array.make (Array.length arr) 0 in
  let c = ref (-1) in
  Array.iteri (fun i b -> if b then c := !c + 1; arr'.(i) <- !c) arr;
  arr'

let reduce_fmla (f: fmla) : fmla =

  let arr_aps = Array.make (max_ap_in_fmla f + 1) false in
  let arr_ags = Array.make (max_ag_in_fmla f + 1) false in

  let rec aux (f: fmla) : unit =
    match f with
    | Not (g)       -> aux g
    | Bin (g, _, h) -> max (aux g) (aux h)
    | Know (a, g)   -> arr_ags.(a) <- true; aux g
    | AP p          -> arr_aps.(p) <- true;
    | _             -> ()
  in aux f;
  map_fmla (count_trues arr_aps) (count_trues arr_ags) f


  
(*****************************************************************************)

let rec modal_depth_of_fmla (f: fmla) : int =
  match f with
  | Not (g)       -> modal_depth_of_fmla g
  | Bin (g, _, h) -> max (modal_depth_of_fmla g) (modal_depth_of_fmla h)
  | Know (_, g)   -> 1 + modal_depth_of_fmla g
  | _             -> 0

(*****************************************************************************)

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

(**
  [pp_of_value vars p] returns a pretty-print of [p] according to [vars]
  if possible, and [p] as a string if not.
*)
let pp_of_value (vars: string array) (p: int) : string =
  if p < 0 || p >= Array.length vars then string_of_int p else vars.(p)

let rec pp_of_fmla (c: context) (f: fmla) : string =
  match f with
  | True           -> "⊤"
  | False          -> "⊥"
  | AP p           ->  pp_of_value c.aps p
  | Not g          -> "\u{00ac}" ^ pp_of_fmla c g
  | Bin (g, op, h) -> "(" ^ pp_of_fmla c g ^ " " ^ pp_of_binop op ^ " " ^ pp_of_fmla c h ^ ")"
  | Know (a, g)    -> "K_" ^ pp_of_value c.ags a ^ " " ^ pp_of_fmla c g ^ ""