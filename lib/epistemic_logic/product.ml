include State

(*************************************************************************)
(*                             Product update                            *)
(*************************************************************************)

(**
  [is_application s alpha] returns [true] iff [s |= e.pre], where [e] is the
  actual event of the action [alpha].
*)
let is_applicable (s: state) (alpha: action) : bool =
  s |= (alpha.actual).pre

(*************************************************************************)

let valuation_of_world (km: kripke_model) (events: event list) (u: world) (e: event) : string list =
  List.filter (fun p -> (km, u) |= post e p) (aps_of_events events)

let world_after_event (km: kripke_model) (events: event list) (w: world) (e: event) : world =
  { history = (w, e) :: w.history; valuation = valuation_of_world km events w e }
    
let extend_world (km: kripke_model) (events: event list) (u: world) : world list =
  events
  |> List.filter (fun e -> (km, u) |= e.pre) 
  |> List.map (world_after_event km events u)

let extend_worlds (km: kripke_model) (events: event list) : world list =
  km.domain
  |> List.map (extend_world km events)
  |> List.concat

(*************************************************************************)

let undistinguishable_for_agent (km: kripke_model) (eas: event relations) (a: string) ((w, w'): world * world) : bool =
  let (u, e) = List.hd w.history in
  let (u', e') = List.hd w'.history in
  List.mem (u, u') (List.assoc a km.relations) && List.mem (e, e') (List.assoc a eas)

let cartesian_product (xs: 'a list) (ys: 'b list) : ('a * 'b) list =
  List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs)

(**
  [relation_of_agent ws_rels ev_rels ws a] returns the relation [r] of agent [a] on the domain [ws],
  that is, the links between worlds that [a] cannot tell apart.
*)
let new_relation_of_agent (km: kripke_model) (eas: event relations) (ws: world list) (a: string) : world relation =
  List.filter (undistinguishable_for_agent km eas a) (cartesian_product ws ws)

(**
  [relation_of_agents ws ws_rels ev_rels] returns the relation of every agent on the domain [ws].
*)
let new_relation_of_agents (km: kripke_model) (eas: event relations) (ws: world list) : world relations =
  List.map (fun (a, _) -> a, new_relation_of_agent km eas ws a) km.relations

(*************************************************************************)

(**
  [product_update s alpha] returns the product update of [s] and [alpha].
*)
let product_update (s: state) (alpha: action) : state =

  let (km, w) = s in

  let ws' = extend_worlds km alpha.model.events in
  let ws_rels' = new_relation_of_agents km alpha.model.relations ws' in

  let km' = { domain = ws'; relations = ws_rels' } in
  let u' = world_after_event km alpha.model.events w alpha.actual in

  (km', u')

let (@) = product_update