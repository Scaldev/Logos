include module type of Syntax

type event = {
  eid: int;
  pre: fmla;
  post: (int * fmla) list;
}

(**
  [post e p] returns the formula to which the atomic proposition [p]
  is mapped to after the event [e] happens.
*)
val post : event -> int -> fmla

(**
    [size_of_event e] returns the size of the event [e].
*)
val size_of_event : event -> int

(**
  [max_ap_of_events es] returns the greatest atomic proposition integer mentionned
  in this event.
*)
val max_ap_of_events : event list -> int

(**
  [pp_of_event c e] returns a pretty-print of an event [e] given [c].
  It's a table with one column and 2 rows. The first row contains the
  precondition formula, and the second row contains the postconditions formulas,
  one per line.

  Example:
  {[
    +----------------+
    | pre: (m_a ∧ d) |
    +----------------+
    | post:          |
    |   m_a := ⊥     |
    |   m_b := ⊤     |
    +----------------+
  ]}
*)
val pp_of_event : context -> event -> string

(*****************************************************************************)

(* 
   [relations.(a).(i)] gives the list of event ids [i'] such that
   [events.(i) ->_a events.(i')].
   Thus, going through all relations of an event model that [O(n)] time,
   where [n] is the number of [(e, e')] pairs such that [e ->_a e'].
*)
type relation = int list array

val size_of_relation : relation -> int

type relations = relation array

type event_model = {
  events: event array;
  relations: relations;
}

type action = {
  name: string;
  model: event_model;
  aid: int
}

exception InvalidEventIndex of int

(**
  [size_of_event_model em] returns the size of the event model [em].
*)
val size_of_event_model : event_model -> int

(**
  [size_of_event_model alpha] returns the size of the underlying
  event model of the action [alpha].
*)
val size_of_action : action -> int