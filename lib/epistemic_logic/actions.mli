include module type of Syntax

type 'a relation = ('a * 'a) list

type event = {
    pre : fmla;
    post : (string * fmla) list;
}

val post : event -> string -> fmla

val aps_of_events : event list -> string list

val size_of_event : event -> int

type 'a arrows = (string * 'a relation) list (* is [(->)_a], with [a] an agent *)

type event_model = {
    events : event list;
    rels : event arrows;
}

type action = event_model * event

val size_of_event_model : event_model -> int

val pp_of_event : event -> string