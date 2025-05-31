open Epistemic_logic

(* Event model for the two generals problem *)

(* Create send_ab action *)

let e1 = {
  pre  = Bin (AP "m_a", And, AP "d");
  post = [ "m_a", False ; "m_b", True ]
}

let e2 = {
  pre  = True;
  post = [ "m_a", False ; "m_b", False ]
}

let r1a = [ e1, e1 ; e1, e2 ; e2, e2 ]
let r1b = [ e1, e1 ; e2, e2 ]

let em1 = {
  events    = [ e1 ; e2 ];
  relations = [ "a", r1a ; "b", r1b ]
}

let send_ab = (em1, e1)

(* Create send_ba action *)

let e3 = {
  pre  = Bin (AP "m_b", And, AP "d");
  post = [ "m_a", True ; "m_b", False ]
}

let e4 = {
  pre  = True;
  post = [ "m_a", False ; "m_b", False ]
}

let r2a = [ e3, e3 ; e4, e4 ]
let r2b = [ e3, e3 ; e3, e4 ; e4, e4 ]

let em2 = {
  events    = [ e3 ; e4 ];
  relations = [ "a", r2a ; "b", r2b ]
}

let send_ba = (em2, e3)

let b = send_ab == send_ba

let () = print_string (string_of_bool b) ; print_newline ()

let s = pp_of_event e1

let exec () = print_string s