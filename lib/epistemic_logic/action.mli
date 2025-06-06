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
  [max_ap_of_events es] returns the greatest atomic proposition integer in an
  event [e] of [es].
*)
val max_ap_of_events : event list -> int

(**
  [pp_of_event c e] returns a pretty-print of an event [e] given [c].
  It's a table with one column and 2 rows. The first row contains the
  precondition formula, and the second row contains the postconditions
  formulas, one per line.

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
   [relation.(n)] gives the list of nodes [n'] such that
   [relation.(n) ->_a relation.(n)].
*)
type relation = int list array

(*
  [size_of_relation r] returns the number of pairs [(n, m)] such that
  [n ->_a m]n with [->_a] the described relation.
*)
val size_of_relation : relation -> int

(*
  [relations.(a)] gives the relation [->_a] for agent [a].
*)
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

(**
  [size_of_event_model em] returns the size of the event model [em].
*)
val size_of_event_model : event_model -> int

(**
  [size_of_event_model alpha] returns the size of the underlying
  event model of the action [alpha].
*)
val size_of_action : action -> int