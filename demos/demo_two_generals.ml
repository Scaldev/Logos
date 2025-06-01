open Epistemic_logic
let e1 = {
  pre  = Bin (AP "m_a", And, AP "d");
  post = [ "m_a", False ; "m_b", True ]
}
let e2 = {
  pre  = True;
  post = [ "m_a", False ; "m_b", False ]
}

let r1a = [ e1, e1 ; e1, e2 ; e2, e1 ; e2, e2 ]
let r1b = [ e1, e1 ; e2, e2 ]

let em1 = {
  events    = [ e1 ; e2 ];
  relations = [ "a", r1a ; "b", r1b ]
}

let send_ab = {
  name   = "send_ab success";
  model  = em1;
  actual = e1
}

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
let r2b = [ e3, e3 ; e3, e4 ; e4, e3 ; e4, e4 ]

let em2 = {
  events    = [ e3 ; e4 ];
  relations = [ "a", r2a ; "b", r2b ]
}

let send_ba = {
  name   = "send_ba success";
  model  = em2;
  actual = e3
}

(* State 0 *)

let w01 = { valuation = ["d" ; "m_a"]; history = []; }
let w02 = { valuation = ["m_a"]; history = []; }

let ra1 = "a", [ w01, w01 ; w02, w02 ]
let rb1 = "b", [ w01, w01 ; w01, w02 ; w02, w01 ; w02, w02 ]

let km0 = {
  domain = [w01 ; w02];
  relations = [ra1 ; rb1]
}

let s0 = km0, w01

let actions_many = Array.init 50 (fun (n: int) -> if n mod 2 == 0 then send_ab else send_ba)

let execute_actions (s0: state) (actions: action array) : unit =
  let s' = ref s0 in
  for i = 0 to Array.length actions_many - 1 do
    print_string ("i = " ^ (string_of_int (i + 1)) ^ " :");
    print_string (" action = \"" ^ actions.(i).name ^ "\"");
    print_string (" (size = " ^ string_of_int (size_of_kripke_model (fst !s')) ^ ")\n");
    flush stdout;
    s' := !s' @ actions.(i);
  done;;

let exec () = execute_actions s0 actions_many