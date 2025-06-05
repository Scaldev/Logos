include Syntax

open Table

module StringSet = Set.Make(String)

let (<<) = Fun.compose

(*****************************************************************************)
(*                                   Event                                   *)
(*****************************************************************************)

type event = {
  eid: int;
  pre: fmla;
  post: (int * fmla) list;
}
  
(*****************************************************************************)

let post (e: event) (p: int) : fmla =
  match List.find_opt ((=) p << fst) e.post with
  | None -> AP p
  | Some (_, f) -> f
    
(*****************************************************************************)

let size_of_event (e: event) : int = 
  size_of_fmla e.pre + List.fold_right ((+) << size_of_fmla << snd) e.post 0
  
(*****************************************************************************)

(**
  [aps_of_events e] returns all atomic propositions mentionned in the precondition
  or one of the postconditions of [e].
  In particular, the returned list contains all atomic propositions such that
  [e.pre.(p) <> p].
*)
let max_ap_of_event (e: event) : int =
  max
    (max_ap_in_fmla e.pre)
    (List.fold_right (max << fun (p, f) -> max p (max_ap_in_fmla f)) e.post 0)

let max_ap_of_events (es: event list): int =
  List.fold_right (max << max_ap_of_event) es 0

(*****************************************************************************)

let pp_of_event (c: context) (e: event) : string =
  
  let pp_post (p, f) = "   " ^ c.aps.(p) ^ " := " ^ pp_of_fmla c f ^ " " in

  let pp_header = " Event e_" ^ string_of_int e.eid ^ " " in
  let pp_pre = " pre: " ^ pp_of_fmla c e.pre ^ " " in
  let pp_hpost = " post: " in
  let pp_posts = List.map pp_post e.post in

  table_of_cells [ [pp_header] ; [pp_pre] ; pp_hpost :: pp_posts ]

(*****************************************************************************)
(*                                  Event model                              *)
(*****************************************************************************)

(* 
   [relations.(a).(i)] gives the list of event ids [i'] such that
   [events.(i) ->_a events.(i')].
   Thus, going through all relations of an event model that [O(n)] time,
   where [n] is the number of [(e, e')] pairs such that [e ->_a e'].
*)
type relation = int list array

let size_of_relation (r: relation) : int =
  Array.fold_right ((+) << List.length) r 0

type relations = relation array

type event_model = {
  events: event array;
  relations: relations;
}

(*****************************************************************************)

let size_of_event_model (em: event_model) : int =
  Array.length em.events
  + Array.fold_right ((+) << size_of_relation) em.relations 0
  + Array.fold_right ((+) << size_of_event) em.events 0

(*****************************************************************************)
(*                                    Actions                                *)
(*****************************************************************************)

type action = {
  name: string;
  model: event_model;
  aid: int
}

exception InvalidEventIndex of int

let size_of_action (alpha: action) : int =
  size_of_event_model alpha.model