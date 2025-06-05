open Epistemic_logic

let aps = [| "m_a" ; "m_b" ; "d" |]
let ags = [| "a" ; "b" |]

let ctx = { aps = aps; ags = ags }

let e_bot = {
  eid = 0;
  pre  = True;
  post = [ 0, False ; 0, False ]
}

(* Send_ab success *)

let e1 = {
  eid = 1;
  pre  = Bin (AP 0, And, AP 1);
  post = [ 0, False ; 1, True ]
}

let r1a = [| [1;2] ; [2;1] |]
let r1b = [| [1] ; [2] |]

let em1 = {
  events    = [| e_bot ; e1 |];
  relations = [| r1a ; r1b |]
}

let send_ab = {
  name   = "send_ab success";
  model  = em1;
  aid = 1
}

(* Send_ba success *)

let e2 = {
  eid = 1;
  pre  = Bin (AP 0, And, AP 1);
  post = [ 0, True ; 1, False ]
}

let r2a = [| [1] ; [2] |]
let r2b = [| [1;2] ; [1;2] |]

let em2 = {
  events    = [| e_bot ; e2 |];
  relations = [| r2a ; r2b |]
}

let send_ba = {
  name   = "send_ba success";
  model  = em2;
  aid = 1
}

(* State 0 *)

let w01 = { wid = 0; valuation = [|true ; false ; true|]; history = []; }
let w02 = { wid = 1; valuation = [|true ; false ; false|]; history = []; }

let ra1 = [| [1] ; [2] |]
let rb1 = [| [1;2] ; [1;2] |]

let km0 = {
  domain = [| w01 ; w02 |];
  relations = [| ra1 ; rb1 |]
}

let s0 = (km0, 0)

let actions_many = Array.init 50 (fun (n: int) -> if n mod 2 == 0 then send_ab else send_ba)

let execute_actions (s0: state) (actions: action array) : unit =
  let s' = ref s0 in
  for i = 0 to Array.length actions_many - 1 do
    print_string ("i = " ^ (string_of_int (i + 1)) ^ " :");
    print_string (" action = \"" ^ actions.(i).name ^ "\"");
    print_string (" (size = " ^ string_of_int (size_of_kripke_model (fst !s')) ^ ")\n");
    print_string (pp_of_state ctx !s');
    flush stdout;
    s' := !s' @ actions.(i);
  done;;

let exec () = execute_actions s0 actions_many