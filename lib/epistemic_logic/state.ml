include Action

open Table

let (<<) = Fun.compose

(*****************************************************************************)
(*                            World & Kripke model                           *)
(*****************************************************************************)

type world = {
  history: (world * event) list; (* events leading to this world *)
  valuation: string list         (* atomic propositions true in that world *)
}

type kripke_model = {
  domain: world list;
  relations: world relations;
}

exception UnknownAgent of string

(*****************************************************************************)

(**
  [size_of_kripke_model km] returns the size of the Kripke model [km].
*)
let size_of_kripke_model (km: kripke_model) : int =
  List.length km.domain
  + List.fold_right ((+) << List.length << snd) km.relations 0
  + List.fold_right ((+) << (fun w -> List.length w.valuation)) km.domain 0

(*****************************************************************************)

(**
  [is_symmetric r] returns [true] iff each [x -> x'] in [r] implies that [x' -> x] is in [r].
*)
let is_symmetric (r: 'a relation) : bool =
  let tbl = Hashtbl.create (List.length r) in
  List.iter (fun e -> Hashtbl.replace tbl e ()) r;
  List.for_all (fun (u, v) -> Hashtbl.mem tbl (v, u)) r
  
let is_S5_model (km: kripke_model) : bool =
  List.for_all (is_symmetric << snd) km.relations

(*****************************************************************************)

let pp_of_world_name (asso: (world * int) list) (w: world) : string =
  "w_" ^ string_of_int (List.assoc w asso)

let pp_of_world (asso: (world * int) list) (w: world) : string =
  let pp_val = String.concat ", " w.valuation in
  "   " ^ pp_of_world_name asso w ^ " : " ^ pp_val ^ " "

let pp_of_worlds (asso: (world * int) list) (ws: world list) : string list =
  " worlds: " :: List.map (pp_of_world asso) ws

let pp_of_edge (asso: (world * int) list) ((u, u'): world * world) : string =
  "(" ^ (pp_of_world_name asso u) ^ ", " ^ (pp_of_world_name asso u') ^ ")"

let pp_of_relation (asso: (world * int) list) ((a, r): string * world relation) : string =
  let pp_edges = String.concat ", " (List.map (pp_of_edge asso) r) in
  "   \u{2192}_" ^ a ^ " = { " ^ pp_edges ^ " } "

let pp_of_relations (asso: (world * int) list) (rs: world relations) : string list =
  " relations: " :: List.map (pp_of_relation asso) rs

let pp_of_kripke_model (km: kripke_model) : string =
  let asso = List.mapi (fun i w -> (w, i+1)) km.domain in
  table_of_cells [ pp_of_worlds asso km.domain ; pp_of_relations asso km.relations ]

(*****************************************************************************)
(*                                   State                                   *)
(*****************************************************************************)

type state = kripke_model * world

exception UnknownActualWorld of world

(*****************************************************************************)

let pp_of_state (s: state) : string =
  let (km, w) = s in
  let w_i = List.find_index ((=) w) km.domain in
  match w_i with
  | None -> raise (UnknownActualWorld w)
  | Some i -> 
    "State with actual world w_" ^ (string_of_int (i+1)) ^ ":\n" ^ (pp_of_kripke_model (fst s))


(*****************************************************************************)

let relation_of_agent (relations: (string * world relation) list) (a: string) : world relation =
  match List.find_opt (((=) a) << fst) relations with
  | None   -> raise (UnknownAgent a)
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
  | True            -> true
  | False           -> false
  | AP p            -> List.mem p w.valuation
  | Not g           -> not (eval s g)
  | Bin (g, And, h) -> eval s g && eval s h
  | Bin (g, Or,  h) -> eval s g || eval s h
  | Bin (g, Imp, h) -> not (eval s g) || eval s h
  | Bin (g, Eq,  h) -> eval s g == eval s h
  | Know (a, g)     -> is_non_contradictory_belief s g (relation_of_agent km.relations a)

let (|=) = eval