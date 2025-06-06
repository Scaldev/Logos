open Epistemic_logic

(* Send_ab *)

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
  name  = "send_ab";
  model = em1;
  aid   = 0
}


(* Send_ba *)

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
  name  = "send_ba";
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

let goal = Bin (
  Know (0, Know (1, AP 2)),
  And,
  Know (1, Know (0, AP 2))
)

let ctx = {
  aps = [| "m_a" ; "m_b" ; "d" |];
  ags = [| "a" ; "b" |]
}

let get_state () : state =
  s0

let rec know max_depth a =
  match max_depth with
  | 0 -> AP 2
  | _ -> Know (a, know (max_depth-1) ((a+1) mod 2))

let make_goal (max_depth: int) =
  Bin(know max_depth 0, And, know max_depth 1)

let instanciate (max_depth: int) : planning_task * context =
    let t = {
    init = s0 ;
    actions = [ send_ab ; send_ba ] ;
    goal = make_goal max_depth
  } in
  (t, ctx)