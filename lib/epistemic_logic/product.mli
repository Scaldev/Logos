include module type of State

exception UnapplicableAction of int * int

(**
    [is_applicable s alpha] returns [true] iff [s |= actual.pre], [actual]
    being the event of id [alpha.aid].
*)
val is_applicable : state -> action -> bool

(**
    [s @ alpha] returns the product update of [s] and [alpha].
*)
val ( @ ) : state -> action -> state
