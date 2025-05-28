include module type of El_syntax

type rel = (int * int) list

type event = {
  id : int;
  mutable pre : fmla;
  mutable post : fmla option array;
}

type event_model = {
    events : event array;
    rels : rel array;
}

type action = event_model * int

val max_ap_in_event_model : event_model -> int

val size_of_event_model : event_model -> int

type world = {
    aps : int list;
    history : int list;
}

type valuation = bool array

type kripke_model = {
  domain : world array;
  rels : rel array;
  valuation : valuation array;
}

type state = kripke_model * int

val size_km : kripke_model -> int

val is_symmetric : rel -> bool

val is_S5_model : kripke_model -> bool

val is_non_contradictory_belief : state -> fmla -> rel -> bool

val eval : state -> fmla -> bool

val is_applicable : state -> action -> bool

val ( @ ) : state -> action -> state