include module type of Action

type 'a relation = ('a * 'a) list

type world = {
    history : (world * event) list;
    valuation : string list;
}

type kripke_model = {
    domain : world list;
    relations : (string * world relation) list;
}

val size_of_kripke_model : kripke_model -> int

val is_S5_model : kripke_model -> bool

val pp_of_kripke_model : kripke_model -> string

exception UnknownAgent of string

type state = kripke_model * world

exception UnknownActualWorld of world

val pp_of_state : state -> string

val ( |= ) : state -> fmla -> bool