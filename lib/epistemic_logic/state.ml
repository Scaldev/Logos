include Action

open Table
module StringSet = Set.Make(String)

let (<<) = Fun.compose

(*****************************************************************************)
(*                                     World                                 *)
(*****************************************************************************)

type world = {
  wid: int;
  valuation: bool array;       (* atomic propositions true in that world *)
  history: (int * event) list; (* events leading to this world. This is very costly, but useful for representing a solution. *)
}

(*****************************************************************************)

let max_ap_of_world (w: world) : int =
  let i = ref (Array.length w.valuation - 1) in
  let continue = ref true in
  while !continue && !i >= 0 do
    if w.valuation.(!i) then continue := false
    else i := !i - 1
  done; !i

let max_ap_of_worlds (ws: world array) : int =
  Array.fold_right (max << max_ap_of_world) ws 0

let valuation_of_world (w: world) : int list =
  w.valuation
  |> Array.to_list
  |> List.mapi (fun i b -> (i, b))
  |> List.filter snd
  |> List.map fst

let size_of_world (w: world) : int =
  Array.fold_right ((+) << fun b -> if b then 1 else 0) w.valuation 0
  
(*****************************************************************************)
(*                               Kripke model                                *)
(*****************************************************************************)

type kripke_model = {
  domain: world array;
  relations: relations;
}

exception UnknownAgent of int
exception UnknownProp of int

(*****************************************************************************)

(**
  [size_of_kripke_model km] returns the size of the Kripke model [km].
*)
let size_of_kripke_model (km: kripke_model) : int =
  Array.length km.domain
  + Array.fold_right ((+) << size_of_relation) km.relations 0
  + Array.fold_right ((+) << size_of_world) km.domain 0

(*****************************************************************************)

let pairs_of_relation (r: relation) : (int * int) list =
  r
  |> Array.mapi (fun i -> List.map (fun j -> (i, j)))
  |> fun arr -> Array.fold_right (@) arr []

(**
  [is_symmetric r] returns [true] iff each [x -> x'] in [r] implies that [x' -> x] is in [r].
*)
let is_symmetric (r: relation) : bool =
  let tbl = Hashtbl.create (Array.length r) in
  let lst = pairs_of_relation r in
  List.iter (fun e -> Hashtbl.replace tbl e ()) lst;
  List.for_all (fun (u, v) -> Hashtbl.mem tbl (v, u)) lst
  
let is_S5_model (km: kripke_model) : bool =
  Array.for_all is_symmetric km.relations

(*****************************************************************************)

let pp_of_world_name (w: world) : string =
  "w^" ^ (string_of_int (List.length w.history)) ^ "_" ^ string_of_int w.wid

let pp_of_history (history: (int * event) list) : string =
  List.fold_right ((^) << (fun (i, e) -> 
    "(w_" ^ string_of_int i ^ ", e_" ^ string_of_int e.eid ^ ") ")
  ) history ""

let pp_of_world (c: context) (w: world) : string =
  let pp_val = String.concat ", " (List.map (fun p -> if p < 0 || p >= (Array.length c.aps) then string_of_int p else c.aps.(p)) (valuation_of_world w)) in
  let pp_hist = "[ " ^ pp_of_history w.history ^ "]" in
  "   " ^ pp_of_world_name w ^ " : " ^ pp_val ^ " " ^ pp_hist ^ " "

let pp_of_worlds (c: context) (ws: world array) : string list =
  " worlds: " :: (Array.to_list (Array.map (pp_of_world c) ws))

let pp_of_edge ((u, u'): world * world) : string =
  "(" ^ (pp_of_world_name u) ^ ", " ^ (pp_of_world_name u') ^ ")"

let pp_of_relation (c: context) (ws: world array) (a: int) (r: relation) : string =
  let pp_edges = String.concat ", " (List.map (fun (i, i') -> pp_of_edge (ws.(i), ws.(i'))) (List.filter (fun (i, j) -> i < j) (pairs_of_relation r))) in
  let ag = if a < 0 || a >= (Array.length c.ags) then string_of_int a else c.ags.(a) in
  "   \u{2192}_" ^ ag ^ " = { " ^ pp_edges ^ " } "

let pp_of_relations (c: context) (ws: world array) (rs: relations) : string list =
  " relations: " :: Array.to_list (Array.mapi (pp_of_relation c ws) rs)

let pp_of_kripke_model (c: context) (km: kripke_model) : string =
  table_of_cells [ pp_of_worlds c km.domain ; pp_of_relations c km.domain km.relations ]

(*****************************************************************************)
(*                                   State                                   *)
(*****************************************************************************)

type state = kripke_model * int

exception UnknownWorld of int

(*****************************************************************************)

let size_of_state (s: state) : int =
  size_of_kripke_model (fst s)

(*****************************************************************************)

let pp_of_state (c: context) (s: state) : string =
  let (km, wi) = s in
  if wi < 0 || wi >= (Array.length km.domain) then
    raise (UnknownWorld wi)
  else
    let pp_header = " State (actual world " ^ pp_of_world_name km.domain.(wi) ^ "): " in
    table_of_cells [ [pp_header] ; pp_of_worlds c km.domain ; pp_of_relations c km.domain km.relations ]

(*****************************************************************************)

(**
  (s |= K_a phi) if and only if (for all w' in W, w ->_a w' implies (M, w') |= phi)
*)
let rec is_non_contradictory_belief (km: kripke_model) (wi: int) (a: int) (f: fmla) : bool =
  if a < 0 || a >= (Array.length km.relations) then raise (UnknownAgent a) else
  List.for_all (fun i' -> eval (km, i') f) km.relations.(a).(wi)

and eval (s: state) (f: fmla) : bool =
  let (km, wi) = s in
  match f with
  | True            -> true
  | False           -> false
  | AP p            ->
    if p < 0 || p >= Array.length km.domain.(wi).valuation then raise (UnknownProp p)
    else km.domain.(wi).valuation.(p)
  | Not g           -> not (eval s g)
  | Bin (g, And, h) -> eval s g && eval s h
  | Bin (g, Or,  h) -> eval s g || eval s h
  | Bin (g, Imp, h) -> not (eval s g) || eval s h
  | Bin (g, Eq,  h) -> eval s g == eval s h
  | Know (a, g)     -> is_non_contradictory_belief km wi a g

let (|=) = eval