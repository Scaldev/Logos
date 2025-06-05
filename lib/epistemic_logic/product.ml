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
  let actual = alpha.model.events.(alpha.aid) in
  s |= actual.pre

(*************************************************************************)

(* Maps each (u, e) pair to the only world u' such that u @ e = u' *)
type table_we_to_w' = world option array array

(* [O(n)] time complexity, with [n] the length of [e.post] *)
let valuation_of_world (km: kripke_model) (u: world) (e: event) : bool array =
  let arr = Array.copy u.valuation in
  List.iter (fun (p, f) ->
    arr.(p) <- (km, u.wid) |= f;
  ) e.post;
  arr

(* [O(n)] time complexity, with [n] the length of [e.post] *)
let world_after_event (km: kripke_model) (w: world) (e: event) : world =
  { wid = 0; history = (w.wid, e) :: w.history; valuation = valuation_of_world km w e }
    
(* [O(|es| * n)] time complexity, with :
  - [|es|] the length of the events array [es].
  - [n] the longest [e.post] list, for an event [e] of [es].

  Note that only events that can be applied will be processed. Thus, if we have an upper
  applicable events, then we have an upper bound for this section overall.

  In particular, for a given problem, the number of actions are fixed, so the number of
  events are too, and so are the length of the events.
  This means that, in that case, extending worlds is in O(1), as we have an upper bound.
  *)
let extend_world (km: kripke_model) (events: event array) (u: world) : world option array =
  Array.map (fun e -> if (km, u.wid) |= e.pre then Some (world_after_event km u e) else None) events

(* 
  [O(|d| * |es| * n)] time complexity, with :
  - [|d|] the size of the domain = the number of worlds before the event.
  - [|es|] the length of the events array [es].
  - [n] the longest [e.post] list, for an event [e] of [es].
  *)
let extend_worlds (km: kripke_model) (events: event array) : table_we_to_w' =
  Array.map (fun u -> extend_world km events u) km.domain

let new_ids (ws: table_we_to_w') : table_we_to_w' =
  let id = ref (-1) in
  Array.map (
    Array.map (fun o ->
    match o with 
    | None -> None
    | Some w -> id := !id + 1; Some { w with wid = !id }  
    )
  ) ws

let worlds_of_table (tbl: table_we_to_w') : world array =
  Array.fold_right Array.append tbl [||]
  |> Array.to_list
  |> List.filter ((<>) None)
  |> List.map Option.get
  |> List.mapi (fun i w -> { w with wid = i })
  |> Array.of_list


(*************************************************************************)

(*
  At most |->_a| * |->_a^E| elements

  For all (u, u') in ->_a :
    For all e in tbl.(u) such that tbl.(u).(e) <> None :
      For all e' in Im(e ->_a) such that tbl.(u').(e') <> None :
        ->_a.((u, e)) += (Option.get tbl.(u').(e')).wid
*)

let expand_world_relation_of_event (tbl': table_we_to_w') (em: event_model) (a: int) (ui': int) (ei: int) : int list =
  em.relations.(a).(ei)
  |> List.map (fun ei' -> (ei', tbl'.(ui').(ei')))
  |> List.filter ((<>) None << snd)
  |> List.map ((fun (_, o) -> (Option.get o).wid))

let expand_world_relation (tbl': table_we_to_w') (em: event_model) (len: int) (a: int) (ui: int) (ui': int) : relation =
  let arr = Array.make len [] in
  tbl'.(ui)
  |> Array.to_list
  |> List.mapi (fun ei o -> (ei, o))
  |> List.filter ((<>) None << snd)
  |> List.map fst
  |> List.iter (fun ei -> 
    let vi = (Option.get tbl'.(ui).(ei)).wid in
    arr.(vi) <- expand_world_relation_of_event tbl' em a ui' ei
  );
  arr

let combine_relations (arr1: relation) (arr2: relation) : relation =
  for i = 0 to (Array.length arr1 - 1) do
      arr1.(i) <- arr1.(i) @ arr2.(i)
  done;
  arr1

let new_relation_of_agents (tbl': table_we_to_w') (em: event_model) (len: int) (a: int) (rs: relation) : relation =
  let rs' = ref (Array.make len []) in
  let _ = rs
  |> Array.to_list
  |> List.mapi (fun ui uis' ->
    let lst = List.map (expand_world_relation tbl' em len a ui) uis' in
    rs' := List.fold_right combine_relations lst !rs';
  ) in
  !rs'

  

let new_relations (tbl': table_we_to_w') (em: event_model) (len: int) (rss: relations) : relations =
  Array.mapi (new_relation_of_agents tbl' em len) rss

(*************************************************************************)

exception UnapplicableAction of world * event

(**
  [product_update s alpha] returns the product update of [s] and [alpha].
*)
let product_update (s: state) (alpha: action) : state =

  let (km, wi) = s in
  let (em, aid) = alpha.model, alpha.aid in

  let tw = extend_worlds km em.events in
  let tw' = new_ids tw in
  let ws' = worlds_of_table tw in

  let ws_rels' = new_relations tw' em (Array.length ws') km.relations in

  let km' = { domain = ws'; relations = ws_rels' } in

  let wi' = match tw'.(wi).(aid) with
    | Some u' -> u'.wid
    | None -> raise (UnapplicableAction (km.domain.(wi), em.events.(aid)))
  in

  let s = (km', wi') in
  s

let (@) = product_update