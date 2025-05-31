open Epistemic_logic

let w1 = {
  valuation = ["d" ; "m_a"]; history = [];
}
let w2 = {
  valuation = ["m_a"]; history = [];
}

let r_a = "a", [ w1, w1 ; w2, w2 ]
let r_b = "b", [ w1, w1 ; w1, w2 ; w2, w1 ; w2, w2 ]

let r_b' = "b", [ w1, w1 ; w1, w2 ; w2, w2 ]

let km1 = {
  domain = [w1 ; w2];
  relations = [r_a ; r_b]
}

let s1 = km1, w1
let s2 = km1, w2

let km2 = {
  domain = [w1 ; w2];
  relations = [r_a ; r_b']
}

(*************************************************************************)
(*                          size_of_kripke_model                         *)
(*************************************************************************)

let test_size_of_kripke_model_0 () =
  let expected = 11 in (* 2 + (2+4) + (2+1) *)
  let obtained = size_of_kripke_model km1 in
  Alcotest.(check int) "" expected obtained
  
let tests_size_of_kripke_model = "size_of_kripke_model", [
  test_size_of_kripke_model_0;
]

(*************************************************************************)
(*                              is_S5_model                              *)
(*************************************************************************)

let test_is_S5_model_0 () =
  let expected = true in
  let obtained = is_S5_model km1 in
  Alcotest.(check bool) "" expected obtained
  
let test_is_S5_model_1 () =
  let expected = false in
  let obtained = is_S5_model km2 in
  Alcotest.(check bool) "" expected obtained

let tests_is_S5_model = "is_S5_model", [
  test_is_S5_model_0;
  test_is_S5_model_1;
]

(*************************************************************************)
(*                                  eval                                 *)
(*************************************************************************)

let test_eval_0 () =
  let expected = true in
  let obtained = s1 |= (Bin (AP "d", And, AP "m_a")) in
  Alcotest.(check bool) "" expected obtained

let test_eval_1 () =
  let expected = true in
  let obtained = s2 |= (Bin (Not (AP "d"), And, AP "m_a")) in
  Alcotest.(check bool) "" expected obtained

let test_eval_2 () =
  let expected = true in
  let obtained = s2 |= (Bin (True, Or, AP "m_b")) in
  Alcotest.(check bool) "" expected obtained

let test_eval_3 () =
  let expected = true in
  let obtained = s2 |= (Bin (False, Eq, Bin (True, Imp, AP "m_b"))) in
  Alcotest.(check bool) "" expected obtained

let test_eval_4 () =
  let expected = true in
  let obtained = s2 |= Bin (False, Imp, True) in
  Alcotest.(check bool) "" expected obtained

(*
  For the two following tests:
    - agent [a] can distinguish the world [w1] where [d] is [true]
      from the world [w2] where [d] is false. Thus, [a] *know* that
      [d] is indeed [true] in the state [s1] where the actual world
      is [w1].
    - agent [b] on the other hand canno't distinguish the world [w1]
      from the world [w2]. Thus, [b] doesn't know for sure that [d]
      is true.
*)

let test_eval_5 () =
  let expected = true in
  let obtained = s1 |= Know ("a", AP "d") in
  Alcotest.(check bool) "" expected obtained

let test_eval_6 () =
  let expected = false in
  let obtained = s1 |= Know ("b", AP "d") in
  Alcotest.(check bool) "" expected obtained

(*
  Agent [z] has an empty epistemic relation;
  that is
*)
let test_eval_7 () =
  try
    let _ = s1 |= Know ("z", AP "d") in
    Alcotest.fail "Aucune exception levée"
  with
  | UnknownAgent "z" -> ()
  | _ -> Alcotest.fail "Mauvaise exception levée"
    
let tests_eval = "test_eval", [
  test_eval_0;
  test_eval_1;
  test_eval_2;
  test_eval_3;
  test_eval_4;
  test_eval_5;
  test_eval_6;
  test_eval_7;
]

(*************************************************************************)

let tests = [
  tests_size_of_kripke_model;
  tests_is_S5_model;
  tests_eval;
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Epistemic logic - state" (format_tests tests) 