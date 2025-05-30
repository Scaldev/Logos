include Actions

let (<<) = Fun.compose

(*************************************************************************)
(*                              Kripke model                             *)
(*************************************************************************)

type world = {
  history: event list;            (* events leading to this world *)
  valuation: string list          (* atomic propositions true in that world *)
}

type kripke_model = {
  domain: world list;                  (* W *)
  rels: (string * world relation) list; (* subset of W x W *)
}

(**
  [size_of_kripke_model km] returns the size of the Kripke model [km].
*)
let size_of_kripke_model (km: kripke_model) : int =
  List.length km.domain
  + List.fold_right ((+) << List.length << snd) km.rels 0
  + List.fold_right ((+) << (fun w -> List.length w.valuation)) km.domain 0

(**
  [is_symmetric r] returns [true] iff each [x -> x'] in [r] implies that [x' -> x] is in [r].
*)
let is_symmetric (r: 'a relation) : bool =
  let tbl = Hashtbl.create (List.length r) in
  List.iter (fun e -> Hashtbl.replace tbl e ()) r;
  List.for_all (fun (u, v) -> Hashtbl.mem tbl (v, u)) r
  
let is_S5_model (km: kripke_model) : bool =
  List.for_all (is_symmetric << snd) km.rels

(*****************************************************************************)

type state = kripke_model * world (* id of the actual world *)

let relation_of_agent (rels: (string * world relation) list) (a: string) : world relation =
  match List.find_opt (((=) a) << fst) rels with
  | None   -> []
  | Some r -> snd r
  
(**
  (s |= K_a phi) if and only if (for all w' in W, w ->_a w' implies (M, w') |= phi)
*)
let rec is_non_contradictory_belief (s: state) (f: fmla) (rel_a: world relation) : bool =
  let (km, w) = s in
  List.for_all (fun (u, w') -> w <> u || eval (km, w') f) rel_a

and eval (s: state) (f: fmla) : bool =
  let (km, w) = s in
  match f with
  | True           -> true
  | False          -> false
  | AP(p)          -> List.mem p w.valuation
  | Not(g)         -> not (eval s g)
  | Bin(g, And, h) -> eval s g && eval s h
  | Bin(g, Or,  h) -> eval s g || eval s h
  | Bin(g, Imp, h) -> not (eval s g) || eval s h
  | Bin(g, Eq,  h) -> eval s g == eval s h
  | Know(a, g)     -> is_non_contradictory_belief s g (relation_of_agent km.rels a)

let (|=) = eval