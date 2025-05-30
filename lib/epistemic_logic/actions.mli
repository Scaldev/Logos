include module type of Syntax

type 'a relation = ('a * 'a) list

type event = {
    pre : fmla;
    post : fmla option array;
}

val post : event -> int -> fmla

val max_ap_in_events : event list -> int

val size_of_event : int -> event -> int

type event_model = {
    events : event list;
    rels : event relation array;
}

type action = event_model * event

val size_of_event_model : event_model -> int

val pp_of_event : string array -> string array -> event -> string