include module type of Syntax

type 'a relation = ('a * 'a) list

type event = {
    pre : fmla;
    post : (string * fmla) list;
}

(**
  [post e p] returns the formula to which the atomic proposition [p]
  is mapped to after the event [e] happens.
*)
val post : event -> string -> fmla

(**
  [aps_of_events es] returns the list of all unique atomic propositions found in the events [es].
*)
val aps_of_events : event list -> string list

(**
    [size_of_event e] returns the size of the event [e].
*)
val size_of_event : event -> int

(**
  [pp_of_event e] returns a pretty-print of an event [e]. It's a table with
  one column and 2 rows. The first row contains the precondition formula,
  and the second row contains the postconditions formulas, one per line.

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
val pp_of_event : event -> string

type 'a relations = (string * 'a relation) list

type event_model = {
  events: event list;
  relations: event relations;
}

type action = event_model * event

(**
  [size_of_event_model em] returns the size of the event model [em].
*)
val size_of_event_model : event_model -> int