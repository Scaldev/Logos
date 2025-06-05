include module type of Action


type world = {
  wid: int;
  valuation: bool array;       (* atomic propositions true in that world *)
  history: (int * event) list; (* events leading to this world. This is very costly, but useful for representing a solution. *)
}


val max_ap_of_worlds : world array -> int

type kripke_model = {
  domain: world array;
  relations: relations;
}

exception UnknownAgent of int

val size_of_kripke_model : kripke_model -> int

val is_S5_model : kripke_model -> bool

val pp_of_kripke_model : context -> kripke_model -> string

type state = kripke_model * int

exception UnknownWorld of int

val size_of_state : state -> int

val pp_of_state : context -> state -> string

val ( |= ) : state -> fmla -> bool