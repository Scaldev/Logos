include State

let (<<) = Fun.compose

(*****************************************************************************)
(*                                is_applicable                              *)
(*****************************************************************************)

(**
  [is_application s alpha] returns [true] iff [s |= e.pre], where [e] is the
  actual event of the action [alpha].
*)
let is_applicable (s: state) (alpha: action) : bool =
  s |= alpha.model.events.(alpha.aid).pre

(*****************************************************************************)
(*                                  New domain                               *)
(*****************************************************************************)

(**
  Maps each (u, e) pair to Some u' such that u' is [u] after [e] if it exists,
  or None otherwise
*)
type table_worlds = world option array array

(**
  [valuation_of_world km u e] returns the updated valuation array of [u] after
  according the [e]'s postconditions, in the [km] Kripke model.

  This is in O([|AP|] * [d]), [|AP|] being the length of [u.valuation] and [l] the
  the largest postcondition formula size of [e].
*)
let valuation_of_world (km: kripke_model) (u: world) (e: event) : bool array =
  let vs = Array.copy u.valuation in
  List.iter (fun (p, f) -> vs.(p) <- (km, u.wid) |= f) e.post;
  vs

(**
  [world_after_event km w e] returns the world [u'] after the event [e].
  This is also in O([|AP|] * [l]).
*)
let world_after_event (km: kripke_model) (u: world) (e: event) : world =
  {
    wid = -1; (* unknown for now, -1 by default *)
    history = (u.wid, e) :: u.history;
    valuation = valuation_of_world km u e
  }
    
(**
  [extend_world km es u] returns an array [arr] of world options, such that,
  if [ei] is [e]'s id:
  - if [e] is applicable to the state [(km, u)], then [arr.(ei)] is [Some u'],
    where [u'] is the world [u] after the event [e].
  - if not, then [arr.(ei)] is [None].

  In O([|E|] * [|AP|] * [|l|]) time complexity, with :
  - [|E|] the length of the events array [es].
  - [|AP|] the longest [e.post] list, for an event [e] of [es].
  - [l] the largest postcodition formula size of [e].

  In particular, for a given problem, the actions are fixed, so [|E|], [|AP|] and
  [d] are too. This means that, in that case, one extending world is in O(1),
  as we have an upper bound with [|E|] * [|AP|] * [l].
  *)
let extend_world (km: kripke_model) (es: event array) (u: world) : world option array =
  Array.map (fun e ->
    if (km, u.wid) |= e.pre then Some (world_after_event km u e) else None
  ) es

(**
  [extend_worlds km es] returns an table mapping each [(u, e)] of
  [km.domain * es] to [Some u'] where [u'] is [u] after [e] if possible, or
  [None] if not.

  This function is in [O(|W| * |E| * n * d)] time complexity, with :
  - [|W|] the number of worlds, i.e. the length of [km.domain].
  - [|E|] the number of events, i.e. the length of [es].
  - [|AP|] the longest [e.post] list, for an event [e] of [es].
  - [l] the largest postcodition formula size of [e].

  In particular, for a given problem, the actions are fixed, so extending worlds
  is in O([|W|]).
*)
let extend_worlds (km: kripke_model) (es: event array) : table_worlds =
  Array.map (fun u -> extend_world km es u) km.domain

(**
  [set_new_ids tbl] returns the table of worlds after mapping each world
  to an unique id, starting at 0.
*)
let set_new_ids (tbl: table_worlds) : table_worlds =
  let id = ref (-1) in
  Array.map (
    Array.map (fun o ->
    match o with 
    | None -> None
    | Some w -> id := !id + 1; Some { w with wid = !id }  
    )
  ) tbl

(**
  [worlds_of_table tbl] returns the array of worlds in [tbl].
*)
let worlds_of_table (tbl: table_worlds) : world array =
  Array.fold_right Array.append tbl [||]
  |> Array.to_list
  |> List.filter ((<>) None)
  |> List.map Option.get
  |> List.mapi (fun i w -> { w with wid = i })
  |> Array.of_list

(*************************************************************************)
(*                              New relations                            *)
(*************************************************************************)

(*
  A naive implementation of 
    [{  ->_a = { ((u, e), (u', e')) in W * E | u ->_a u' and e ->_a e' )}  }]
  would be to compute each [(v, v') = ((u, e), (u', e'))] pair, then filter.
  This would sadly be in O([|W|^2]) time complexity, which doesn't scale well
  at all.

  Our implementation relies on the previous [->_a] value to compute the new
  one:

  +-------------------------------------------------------------------+
  + Compute ->_a' :                                                   |
  +-------------------------------------------------------------------+
  | For all (u, u') in ->_a :                                         |
  |   For all e in tbl.(u) such that tbl.(u).(e) <> None :            |
  |     For all e' in Im(e ->_a) such that tbl.(u').(e') <> None :    |
  |       ->_a'.((u, e)) += (Option.get tbl.(u').(e')).wid            |
  +-------------------------------------------------------------------+

  This is much better, as we now have exactly |->_a'| steps.

  Intuitively, this corresponds to:
    for each [(u, u')] pair in [->_a], we test for all events [e] possible.
    When possible, we try for all [e'] such that [e ->_a e'], if [e'] is
    applicable to [u']. If so, we have our [((u, e), (u', e'))] pair !

*)

(**
  [expand_world_relation_of_event tbl em a ui' ei] returns a list of worlds
  ids [vis'] after applying the events [ei'] of [e_i ->_a] on the world of
  id [ui'].
  
  Intuitively, it's the list of worlds created by applying any possible event
  [e'] such that [e ->_a e'] on the world [u'].
*)
let expand_relation_of_event (tbl: table_worlds) (em: event_model) (a: int) (ui': int) (ei: int) : int list =
  em.relations.(a).(ei)
  |> List.map (fun ei' -> (ei', tbl.(ui').(ei')))
  |> List.filter ((<>) None << snd)
  |> List.map ((fun (_, o) -> (Option.get o).wid))

(**
  [create_relation tbl em len a ui ui' vis'] returns a partial relation [rs]
  for agent [a] on the domain of length [len], looking only at the worlds
  created by applying an event to the world [u].
*)
let create_relation (tbl: table_worlds) (em: event_model) (len: int) (a: int) (ui: int) (ui': int) (eis: int list) : relation =
  let rs = Array.make len [] in
  List.iter (fun ei ->
    let vi = (Option.get tbl.(ui).(ei)).wid in
    rs.(vi) <- expand_relation_of_event tbl em a ui' ei
  ) eis;
  rs

(**
  [expand_relation tbl em len a ui ui' vis'] returns a partial relation [rs]
  for agent [a] on the domain of length [len], looking only at the worlds
  created by applying an event to the world [u].
*)
let expand_relation (tbl: table_worlds) (em: event_model) (len: int) (a: int) (ui: int) (ui': int) : relation =
  tbl.(ui)
  |> Array.to_list
  |> List.mapi (fun ei o -> (ei, o))
  |> List.filter ((<>) None << snd)
  |> List.map fst
  |> create_relation tbl em len a ui ui'

(**
  [combine_relations rs1 rs2] returns a new relation [rs] by merging [rs1] and
  [rs2] together.

  Here, it means that for all world [u] of id [ui], we concatenate [rs1.(ui)]
  and [rs2.(ui)].
*)
let combine_relations (rs1: relation) (rs2: relation) : relation =
  let rs = Array.make (Array.length rs1) [] in
  for ui = 0 to (Array.length rs1 - 1) do
    rs.(ui) <- rs1.(ui) @ rs2.(ui)
  done;
  rs

(**
  [new_relations_of_agent tbl em len a rs] returns a new relation for agent [a] on the domain of
  length [len] given [tbl], [em] and its previous relation [rs].
*)
let new_relation_of_agent (tbl: table_worlds) (em: event_model) (len: int) (a: int) (rs: relation) : relation =
  let rs' = ref (Array.make len []) in
  let _ = rs
  |> Array.to_list
  |> List.mapi (fun ui uis' ->
    let lst = List.map (expand_relation tbl em len a ui) uis' in
    rs' := List.fold_right combine_relations lst !rs';
  ) in
  !rs'

(**
    [new_relations tbl em len rss] maps each relation [rs] of [rss] to a new relation [rs']
    on the domain of length [len], given [tbl] and [em]. 
*)
let new_relations (tbl: table_worlds) (em: event_model) (len: int) (rss: relations) : relations =
  Array.mapi (new_relation_of_agent tbl em len) rss

(*************************************************************************)
(*                             Product update                            *)
(*************************************************************************)

exception UnapplicableAction of int * int

(**
  [product_update s alpha] returns the product update of [s] and [alpha].
*)
let product_update (s: state) (alpha: action) : state =

  let (km, wi) = s in
  let (em, aid) = alpha.model, alpha.aid in

  let tw = extend_worlds km em.events in
  let tw' = set_new_ids tw in
  let ws' = worlds_of_table tw in

  let ws_rels' = new_relations tw' em (Array.length ws') km.relations in

  let km' = { domain = ws'; relations = ws_rels' } in

  let wi' = match tw'.(wi).(aid) with
    | Some u' -> u'.wid
    | None -> raise (UnapplicableAction (wi, aid))
  in

  let s = (km', wi') in
  s

let (@) = product_update