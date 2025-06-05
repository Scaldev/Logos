open Epistemic_logic

(* Two generals problem *)


let aps = [| "m_a" ; "m_b" ; "d" |]
let ags = [| "a" ; "b" |]

let ctx = { aps = aps; ags = ags }

(* Send_ab success *)

let e0 = {
  eid = 0;
  pre  = Bin (AP 0, And, AP 2);
  post = [ 0, False ; 1, True ]
}

let e1 = {
  eid = 1;
  pre  = True;
  post = [ 0, False ; 0, False ]
}

let r1a = [| [0;1] ; [0;1] |]
let r1b = [| [0] ; [1] |]

let em1 = {
  events    = [| e0 ; e1 |];
  relations = [| r1a ; r1b |]
}

let send_ab = {
  name  = "send_ab success";
  model = em1;
  aid   = 0
}

(* Send_ba success *)

let e0' = {
  eid = 0;
  pre  = Bin (AP 1, And, AP 2);
  post = [ 0, True ; 1, False ]
}

let e1' = {
  eid = 1;
  pre  = True;
  post = [ 0, False ; 1, False ]
}

let r2a = [| [0] ; [1] |]
let r2b = [| [0;1] ; [0;1] |]

let em2 = {
  events    = [| e0' ; e1' |];
  relations = [| r2a ; r2b |]
}

let send_ba = {
  name  = "send_ba success";
  model = em2;
  aid   = 0
}

(* State 0 *)

let w00 = {
  wid = 0; valuation = [| true ; false ; true |]; history = [];
}
let w01 = {
  wid = 1; valuation = [| true ; false ; false |]; history = [];
}

let r0a = [| [0] ; [1] |]
let r0b = [| [0;1] ; [0;1] |]

let km0 = {
  domain = [|w00 ; w01|];
  relations = [|r0a ; r0b|]
}

let s0 = km0, 0

let n_max = 500
let actions_many = Array.init n_max (fun (n: int) -> if n mod 2 == 0 then send_ab else send_ba)

let execute_actions (s0: state) (actions: action array) : unit =
  let s' = ref s0 in
  for i = 0 to Array.length actions_many - 1 do
    print_string ("i = " ^ (string_of_int (i + 1)) ^ " :");
    print_string (" action = \"" ^ actions.(i).name ^ "\" ");
    print_string (" (size = " ^ string_of_int (size_of_state !s') ^ ")\n");
    flush stdout;
    s' := !s' @ actions.(i);
  done;;

let exec () = execute_actions s0 actions_many