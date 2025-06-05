open Epistemic_logic

(* Two generals problem *)

let ctx1 = {
  aps = [| "m_a" ; "m_b" ; "d" |];
  ags = [| "a" ; "b" |]
}

let e_bot = {
  eid = 0;
  pre  = True;
  post = [ 0, False ; 1, False ]
}

(* Send_ab success *)

let e1 = {
  eid = 1;
  pre  = Bin (AP 0, And, AP 2);
  post = [ 0, False ; 1, True ]
}

let r1a = [| [0;1] ; [0;1] |]
let r1b = [| [0] ; [1] |]

let em1 = {
  events    = [| e_bot ; e1 |];
  relations = [| r1a ; r1b |]
}

let send_ab = {
  name  = "send_ab success";
  model = em1;
  aid   = 1
}

(* Send_ba success *)

let e2 = {
  eid = 1;
  pre  = Bin (AP 1, And, AP 2);
  post = [ 0, True ; 1, False ]
}

let r2a = [| [0] ; [1] |]
let r2b = [| [0;1] ; [0;1] |]

let em2 = {
  events    = [| e_bot ; e2 |];
  relations = [| r2a ; r2b |]
}

let send_ba = {
  name  = "send_ba success";
  model = em2;
  aid   = 1
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

(* State 1 *)

let w10 = {
  wid = 0; valuation = [| false ; false ; true |]; history = [ (0, e_bot) ];
}
let w11 = {
  wid = 1; valuation = [| false ; true ; true |]; history = [ (0, e1) ];
}
let w12 = {
  wid = 2; valuation = [| false ; false ; false |]; history = [ (1, e_bot) ];
}

let r2a' = [| [0;1] ; [0;1] ; [2] |]
let r2b' = [| [0;2] ; [1] ; [0;2] |]

let km1 = {
  domain = [|w10 ; w11 ; w12|];
  relations = [|r2a' ; r2b'|]
}

let s1 = km1, 1

(*****************************************************************************)
(*                                is_applicable                              *)
(*****************************************************************************)

(* The messager is with general [a] and the event is [send_ab] *)
let test_is_applicable_0 () =
  let obtained = is_applicable s0 send_ab in
  let expected = true in
  Alcotest.(check bool) "" expected obtained

(* The messager is with general [a] and the event is [send_ba] *)
let test_is_applicable_1 () =
  let obtained = is_applicable s0 send_ba in
  let expected = false in
  Alcotest.(check bool) "" expected obtained

let tests_is_applicable = "is_applicable", [
  test_is_applicable_0;
  test_is_applicable_1;
]

(*****************************************************************************)
(*                               product_update                              *)
(*****************************************************************************)

(* The messager is with general [a] and the event is [send_ab] *)
let test_product_update_0 () =
  let obtained = s0 @ send_ab in
  let expected = s1 in
  Alcotest.(check string) "" (pp_of_state ctx1 expected) (pp_of_state ctx1 obtained)

let tests_product_update = "product_update", [
  test_product_update_0;
]

(*****************************************************************************)

let tests = [
  tests_is_applicable;
  tests_product_update;
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Epistemic logic - actions" (format_tests tests) 