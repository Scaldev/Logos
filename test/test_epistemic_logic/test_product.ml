open Epistemic_logic

(* Add state equality *)

let pp_state fmt f =
  Format.fprintf fmt "%s" (pp_of_state f)

let testable_state = Alcotest.testable (pp_state) (=)

(* Two generals problem *)

(* Create send_ab action *)

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

let km1 = {
  domain = [w01 ; w02];
  relations = [ra1 ; rb1]
}

let s1 = km1, w01

(* State 1 = State 1 after send_ab *)

let w11 = { valuation = ["d" ; "m_b"]; history = [ w01, e1 ]; }
let w12 = { valuation = ["d"]; history = [ w01, e2 ]; }
let w13 = { valuation = []; history = [ w02, e2 ]; }

let ra2 = "a", [ w11, w11 ; w11, w12 ; w12, w11 ; w12, w12 ; w13, w13 ]
let rb2 = "b", [ w11, w11 ; w12, w12 ; w12, w13 ; w13, w12 ; w13, w13 ]

let km2 = {
  domain = [w11 ; w12 ; w13];
  relations = [ra2 ; rb2]
}

let s2 = km2, w11

(*****************************************************************************)
(*                                is_applicable                              *)
(*****************************************************************************)

(* The messager is with general [a] and the event is [send_ab] *)
let test_is_applicable_0 () =
  let obtained = is_applicable s1 send_ab in
  let expected = true in
  Alcotest.(check bool) "" expected obtained

(* The messager is with general [a] and the event is [send_ba] *)
let test_is_applicable_1 () =
  let obtained = is_applicable s1 send_ba in
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
  let obtained = s1 @ send_ab in
  let expected = s2 in
  Alcotest.(check testable_state) "" expected obtained

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