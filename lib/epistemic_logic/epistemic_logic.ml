include El_syntax

let (<<) = Fun.compose

(*************************************************************************)
(*                           Epistemic actions                           *)
(*************************************************************************)

type rel = (int * int) list

type event = {
  id: int;
  mutable pre: fmla;
  mutable post: fmla option array;
}  

type event_model = {
  events: event array;          (* E *)
  rels: rel array; (* subset of E x E *)
}

type action = event_model * int (* id of the actual event *)

(**
  [max_ap em] returns the maximum int for an atomic proposition.
*)
let max_ap_in_event_model (em: event_model) =
  Array.fold_right max (
    Array.map (fun e -> max_ap_in_fmla e.pre) em.events
  ) 0

(**
  [size_of_event max_ap e] returns the size of the event [e] where the
  maximum int for an atomic proposition in [e]'s postconditions is [max_ap].
*)
let size_of_event (max_ap: int) (e: event) : int = 
  let s = ref (size_of_fmla e.pre) in
  for p = 0 to max_ap do
    match e.post.(p) with
    | None -> ()
    | Some f -> s := !s + size_of_fmla f
  done; !s

(**
  [size em] returns the size of the event model [em].
*)
let size_of_event_model (em: event_model) : int =
  let max_ap =  max_ap_in_event_model em in
  Array.length em.events
  + Array.fold_right ((+) << List.length) em.rels 0
  + Array.fold_right ((+) << size_of_event max_ap) em.events 0

(*************************************************************************)
(*                              Kripke model                             *)
(*************************************************************************)

type world = {
  aps: int list;                (* atomic propositions true in this world *)
  history: int list;            (* event ids leading to this world *)
}

type valuation = bool array     (* valuation.(n) is the boolean value of the n-th ap *)

type kripke_model = {
  domain: world array;          (* W *)
  rels: rel array;              (* subset of W x W *)
  valuation: valuation array;   (* V : W -> 2^AP *)
}

type state = kripke_model * int (* id of the actual world *)

(**
  [size_km km] returns the size of the Kripke model [m].
*)
let size_km (km: kripke_model) : int =
  Array.length (km.domain)
  + Array.fold_right ((+) << List.length) km.rels 0
  + Array.length km.valuation

(**
  [is_symmetric rel] returns [true] iff each [W -> W'] in [rel] implies that [W' -> W] is in [rel].
*)
let is_symmetric (r: rel) : bool =
  let tbl = Hashtbl.create (List.length r) in
  List.iter (fun e -> Hashtbl.replace tbl e ()) r;
  List.for_all (fun (u, v) -> Hashtbl.mem tbl (v, u)) r

(* In O(n) time complexity, where n is the total amount of relations defined for all agents. *)
let is_S5_model (km: kripke_model) : bool =
  Array.for_all is_symmetric km.rels

let rec is_non_contradictory_belief (s: state) (g: fmla) (rs : (int * int) list) : bool =
  let (km, wi) = s in
  match rs with
  | [] -> true
  | (w, w') :: ws -> wi <> w || (eval (km, w') g && is_non_contradictory_belief s g ws)

and eval (s: state) (f: fmla) : bool =
  let (km, wi) = s in
  match f with
  | True           -> true
  | False          -> false
  | AP(i)          -> km.valuation.(wi).(i)
  | Not(g)         -> not (eval s g)
  | Bin(g, And, h) -> eval s g && eval s h
  | Bin(g, Or,  h) -> eval s g || eval s h
  | Bin(g, Imp, h) -> not (eval s g) || eval s h
  | Bin(g, Eq,  h) -> eval s g == eval s h
  | Know(a, g)     -> is_non_contradictory_belief s g km.rels.(a)

(*************************************************************************)
(*                             Product update                            *)
(*************************************************************************)

let is_applicable (s: state) (alpha: action) : bool =
  let (ev_model, e_id) = alpha in
  eval s ev_model.events.(e_id).pre

(*************************************************************************)

(**
  Each [extend_worlds_aux km es wi us' ei] call adds a world [u'] to [us'] such that [u'] is the world [km.domain.(ui)]
  with a new event [es.(ei)] in its history, iff [evals (km, u) e.pre == true] and [ei < |es|].
*)
let rec extend_worlds_aux (km: kripke_model) (es: event array) (ui: int) (us': world list) (ei: int) : world list =
  let e = es.(ei) in
  if ei < Array.length es && eval (km, ui) e.pre then
    let u = km.domain.(ui) in
    let u' = { aps = u.aps; history = e.id :: u.history } in
    extend_worlds_aux km es ui (u' :: us') (ei+1)
  else us'

(**
  [extend_worlds km es ui] returns an array [us'] of the worlds [u = km.domain.(ui)] with a new event [e] of [es]
  such that [evals (km, u) e.pre == true].
*)
let extend_worlds (km: kripke_model) (es: event array) (ui: int) : world array =
  Array.of_list (extend_worlds_aux km es ui [] 0)

(**
  [worlds_of_update km em] returns an array such that foral all [w'] of [ws'],
  [w'] is a world of [km.domain] with a new event [e] such that [eval km (e.pre) == true].
*)
let worlds_of_update (km: kripke_model) (es: event array) : world array =
  let size_W = Array.length (km.domain) in
  Array.concat (List.init size_W (extend_worlds km es))

(*************************************************************************)

(**
  [connected ws ws_rels ev_rels a (i, j)] returns [true] iff [w ->_a w'] and [e ->_a e'], where
  [w] has id [i] and its last event is [e], [w'] has id [j] and its last event is [e'].

  It corresponds to the fact that agent [a] cannot distinguish the worlds [w] and [w'].
*)
let undistinguishable (ws: world array) (ws_rels: rel array) (ev_rels: rel array) (a: int) (p: int * int) : bool =
  let (i, j) = p in
  let e  = List.hd ws.(i).history in
  let e' = List.hd ws.(j).history in
  List.mem (i, j) ws_rels.(a) && List.mem (e, e') ev_rels.(e)

(**
  [make_square size] return a list matrix [m] of [(i, j)] pairs, such that
  [((List.get j) << (List.get i)) m == (i, j)].
*)
let make_square (size: int) : (int * int) list list =
  List.init size (fun i -> List.init size (fun j -> (i, j)))

(**
  [relation_of_agent ws ws_rels ev_rels a] returns the relation [r] of agent [a] on the domain [ws],
  that is, the links between worlds that [a] cannot tell apart.
*)
let relation_of_agent (ws: world array) (ws_rels: rel array) (ev_rels: rel array) (a: int) : rel =
  make_square (Array.length ws)
  |> List.concat
  |> List.filter (undistinguishable ws ws_rels ev_rels a)

(**
  [relation_of_agents ws ws_rels ev_rels] returns the relation of every agent on the domain [ws].
*)
let relation_of_agents (ws: world array) (ws_rels: rel array) (ev_rels: rel array) : rel array =
  Array.init (Array.length ws_rels) (relation_of_agent ws ws_rels ev_rels)

(**
  [is_ap_in_world s alpha w ap] returns [true] iff the atomic proposition [ap] is [true] in
  the world [w] after the action [alpha] on state [s].
*)
let is_ap_in_world (s: state) (alpha: action) (w: int) (ap: int) : bool =
  let (km, w_id) = s in
  let (em, e_id) = alpha in
  match em.events.(e_id).post.(ap) with
  | None   -> km.valuation.(w_id).(ap)
  | Some q -> eval (km, w) q

(*************************************************************************)

(**
  [get_valuation s alpha] returns an array [vss] of valuations,
  such that vss.(i) is the valuation of world i.
*)
let valuation_of_update (s: state) (alpha: action) : valuation array =
  let size_W = Array.length (fst s).domain in
  let size_Ag = max_ap_in_event_model (fst alpha) in
  Array.init size_W (Array.init size_Ag << (is_ap_in_world s alpha))

(**
  [world_of_event km u e] returns the index of the world [u'] in [km.domain]
  such that [u] after the event [e] is [u'].
*)
let world_of_event (km: kripke_model) (u: world) (e: event) : int =
  let rec aux (i: int) =
    match i with
    | 0 -> failwith "world not found"
    | _ -> 
      let u' = { aps = u.aps; history = e.id :: u.history } in
      if km.domain.(i) = u' then i else aux (i-1)
    in aux (Array.length km.domain)

(*************************************************************************)

(**
  [product_update s alpha] returns the product update of [s] and [alpha].
*)
let product_update (s: state) (alpha: action) : state =

  let (km, wi) = s in
  let (em, ei) = alpha in
  let w = km.domain.(wi) in
  let e = em.events.(ei) in

  let ws' = worlds_of_update km em.events in
  let ws_rels' = relation_of_agents ws' km.rels em.rels in
  let val' = valuation_of_update s alpha in

  let km' = { domain = ws'; rels = ws_rels'; valuation = val' } in
  let u' = world_of_event km w e in

  (km', u')


let (@) = product_update