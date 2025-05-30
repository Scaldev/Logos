include module type of Actions

type 'a relation = ('a * 'a) list

type world = {
    history : event list;
    valuation : string list;
}

type kripke_model = {
    domain : world list;
    rels : (string * world relation) list;
}

type state = kripke_model * world

val size_of_kripke_model : kripke_model -> int

val is_S5_model : kripke_model -> bool

val ( |= ) : state -> fmla -> bool