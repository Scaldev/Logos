include Syntax

open Table

module StringSet = Set.Make(String)

let (<<) = Fun.compose

(*****************************************************************************)
(*                                   Event                                   *)
(*****************************************************************************)

type 'a relation = ('a * 'a) list

type event = {
  pre: fmla;
  post: (string * fmla) list;
}
  
(*****************************************************************************)

let post (e: event) (p: string) : fmla =
  match List.find_opt ((=) p << fst) e.post with
  | None -> AP p
  | Some (_, f) -> f
    
(*****************************************************************************)

let size_of_event (e: event) : int = 
  size_of_fmla e.pre + List.fold_right ((+) << size_of_fmla << snd) e.post 0
  
(*****************************************************************************)

let union_aps_post ((p, f): string * fmla) : StringSet.t -> StringSet.t =
  aps_of_fmla f
  |> StringSet.of_list
  |> StringSet.add p
  |> StringSet.union

(**
  [aps_of_events e] returns all atomic propositions mentionned in the precondition
  or one of the postconditions of [e].
  In particular, the returned list contains all atomic propositions such that
  [e.pre.(p) <> p].
*)
let aps_of_event (e: event) : StringSet.t =
  StringSet.union
    (StringSet.of_list (aps_of_fmla e.pre))
    (List.fold_right union_aps_post e.post StringSet.empty)

let aps_of_events (es: event list): string list =
  List.fold_right (StringSet.union << aps_of_event) es (StringSet.empty)
  |> StringSet.to_list
  |> List.sort String.compare

(*****************************************************************************)

let pp_of_event (e: event) : string =
  
  let pp_pre = " pre: " ^ pp_of_fmla e.pre ^ " " in
  let pp_post = " post: " in
  let pp_posts = List.map (fun (p,f) -> "   " ^ p ^ " := " ^ pp_of_fmla f ^ " ") e.post in

  table_of_cells [ [pp_pre] ; pp_post :: pp_posts ]

(*****************************************************************************)
(*                            Event model & action                           *)
(*****************************************************************************)

(* is [(->)_a], with [a] an agent *)
type 'a relations = (string * 'a relation) list

type event_model = {
  events: event list;
  relations: event relations;
}

type action = {
  name: string;
  model: event_model;
  actual: event
}

exception UnknownActualEvent of event

(*****************************************************************************)

let size_of_event_model (em: event_model) : int =
  List.length em.events
  + List.fold_right ((+) << List.length << snd) em.relations 0
  + List.fold_right ((+) << size_of_event) em.events 0