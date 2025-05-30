include State

let (<<) = Fun.compose

(*************************************************************************)
(*                             Product update                            *)
(*************************************************************************)

(**
  [is_application s alpha] returns [true] iff [s |= e.pre], where [e] is the
  actual event of the action [alpha].
*)
let is_applicable (s: state) (alpha: action) : bool =
  s |= (snd alpha).pre

(*************************************************************************)

let valuation_of_world (km: kripke_model) (events: event list) (u: world) (e: event) : string list =
  aps_of_events events
  |> List.map (fun p -> (p, (km, u) |= post e p))
  |> List.filter snd
  |> List.map fst

let world_after_event (km: kripke_model) (events: event list) (w: world) (e: event) : world =
  { history = e :: w.history; valuation = valuation_of_world km events w e }
    
let extend_world (km: kripke_model) (events: event list) (u: world) : world list =
  events
  |> List.filter (fun e -> (km, u) |= e.pre) 
  |> List.map (world_after_event km events u)

let extend_worlds (km: kripke_model) (events: event list) : world list =
  km.domain
  |> List.map (extend_world km events)
  |> List.concat

(*************************************************************************)

let relation_of_agent (ars: 'a arrows) (a: string) : 'a relation =
  snd (List.find ((=) a << fst) ars)

(**
  [undistinguishable_for_agent ws_rels ev_rels a p] returns [true] iff [u ->_a u']
  and [e ->_a e'], where [p = (u, u')], [e] is the last event of world [u] and [e']
  is the last event of world [u'].
*)
let undistinguishable_for_agent (was: world arrows) (eas: event arrows) (a: string) (p: world * world) : bool =
  let (u, u') = p in
  let e  = List.hd u.history in
  let e' = List.hd u'.history in
  List.mem (u, u') (relation_of_agent was a) && List.mem (e, e') (relation_of_agent eas a)

(**
  [relation_of_agent ws_rels ev_rels ws a] returns the relation [r] of agent [a] on the domain [ws],
  that is, the links between worlds that [a] cannot tell apart.
*)
let relation_of_agent (was: world arrows) (eas: event arrows) (ws: world list) (a: string) : world relation =
  List.filter (undistinguishable_for_agent was eas a) (List.combine ws ws)

(**
  [relation_of_agents ws ws_rels ev_rels] returns the relation of every agent on the domain [ws].
*)
let relation_of_agents (was: world arrows) (eas: event arrows) (ws: world list) : world arrows =
  List.map (fun (a, _) -> a, relation_of_agent was eas ws a) was

(*************************************************************************)

(**
  [product_update s alpha] returns the product update of [s] and [alpha].
*)
let product_update (s: state) (alpha: action) : state =

  let (km, w) = s in
  let (em, e) = alpha in

  let ws' = extend_worlds km em.events in
  let ws_rels' = relation_of_agents km.rels em.rels ws' in

  let km' = { domain = ws'; rels = ws_rels' } in
  let u' = world_after_event km em.events w e in

  (km', u')

let (@) = product_update