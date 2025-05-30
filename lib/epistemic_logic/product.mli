include module type of State

val is_applicable : state -> action -> bool

val valuation_of_world : kripke_model -> event list -> world -> event -> int list

val ( @ ) : state -> action -> state
