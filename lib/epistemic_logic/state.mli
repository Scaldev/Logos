include module type of Action

type world = {
  wid: int;
  valuation: bool array;
  history: (int * event) list;
}

exception UnknownProp of int

(**
  [max_ap_of_worlds ws] returns the greatest atomic proposition integer in
  a world [w] of [ws], by checking their respective valuation.
*)
val max_ap_of_worlds : world array -> int

(**
  [pp_of_world c w] returns the pretty-print of [w] given the context [c].
*)
val pp_of_world : context -> world -> string

type kripke_model = {
  domain: world array;
  relations: relations;
}

exception UnknownAgent of int

(**
  [size_of_kripke_model km] returns the size of the Kripke model.
*)
val size_of_kripke_model : kripke_model -> int

(**
  [is_S5_model km] returns [true] iff [km] is an S5 model, that is,
  each relation of the model is symetric.
*)
val is_S5_model : kripke_model -> bool

(**
  [pp_of_kripke_model c km] returns the pretty-print of the Kripke model
  given the context [c].
*)
val pp_of_kripke_model : context -> kripke_model -> string

type state = kripke_model * int

(* If the integer [wi] of a state doesn't map to a world of [km.domain] *)
exception UnknownWorld of int

(**
  [size_of_state s] returns the size of the state [s].
*)
val size_of_state : state -> int

(**
  [s |= f] returns [true] iff the formula [f] is true in the state [s].
*)
val ( |= ) : state -> fmla -> bool

(**
  [pp_of_state c s] returns the pretty-print of the state [s] given
  the context [c].
*)
val pp_of_state : context -> state -> string