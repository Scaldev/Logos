include module type of State

val is_applicable : state -> action -> bool

val ( @ ) : state -> action -> state
