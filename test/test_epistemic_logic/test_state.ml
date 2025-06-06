open Epistemic_logic

(*****************************************************************************)
(*                                    Set up                                 *)
(*****************************************************************************)

let ctx1 = {
  aps = [| "m_a" ; "m_b" ; "d" |];
  ags = [| "a" ; "b" |]
}

let w1 = {
  wid = 0; valuation = [| true ; false ; true |]; history = [];
}
let w2 = {
  wid = 1; valuation = [| true ; false ; false |]; history = [];
}

let r_a = [| [0] ; [1] |]
let r_b = [| [0;1] ; [0;1] |]

let r_b' = [| [0;1] ; [1] |]

let km1 = {
  domain = [|w1 ; w2|];
  relations = [|r_a ; r_b|]
}

let s1 = km1, 0
let s2 = km1, 1

let km2 = {
  domain = [|w1 ; w2|];
  relations = [|r_a ; r_b'|]
}

let km3 = {
  domain = [| |];
  relations = [||];
}

(*************************************************************************)
(*                            max_ap_of_worlds                           *)
(*************************************************************************)

let test_max_ap_of_worlds_0 () =
  let expected = 2 in
  let obtained = max_ap_of_worlds [|w1 ; w2|] in
  Alcotest.(check int) "" expected obtained
  
let tests_max_ap_of_worlds = "max_ap_of_worlds", [
  test_max_ap_of_worlds_0;
]

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
(*                           pp_of_kripke_model                          *)
(*************************************************************************)

let test_pp_of_kripke_model_0 () =
  let expected = "\
    +------------+\n\
    | worlds:    |\n\
    +------------+\n\
    | relations: |\n\
    +------------+\n"
  in
  let obtained = pp_of_kripke_model ctx1 km3 in
  Alcotest.(check string) "" expected obtained

let test_pp_of_kripke_model_1 () =
  let expected = "\
  +----------------------------+\n\
  | worlds:                    |\n\
  |   w^0_0 : m_a, d [ ]       |\n\
  |   w^0_1 : m_a [ ]          |\n\
  +----------------------------+\n\
  | relations:                 |\n\
  |   →_a = {  }               |\n\
  |   →_b = { (w^0_0, w^0_1) } |\n\
  +----------------------------+\n"
  in
  let obtained = pp_of_kripke_model ctx1 km1 in
  Alcotest.(check string) "" expected obtained

let test_pp_of_kripke_model_2 () =
  let expected = "\
  +----------------------------+\n\
  | worlds:                    |\n\
  |   w^0_0 : 0, 2 [ ]         |\n\
  |   w^0_1 : 0 [ ]            |\n\
  +----------------------------+\n\
  | relations:                 |\n\
  |   →_0 = {  }               |\n\
  |   →_1 = { (w^0_0, w^0_1) } |\n\
  +----------------------------+\n"
  in
  let obtained = pp_of_kripke_model { aps=[||] ; ags=[||] } km1 in
  Alcotest.(check string) "" expected obtained

let tests_pp_of_kripke_model = "pp_of_kripke_model", [
  test_pp_of_kripke_model_0;
  test_pp_of_kripke_model_1;
  test_pp_of_kripke_model_2;
]

(*************************************************************************)
(*                          tests_size_of_state                          *)
(*************************************************************************)

let test_size_of_state_0 () =
  let expected = 11 in (* 2 + (2+4) + (2+1) *)
  let obtained = size_of_state s1 in
  Alcotest.(check int) "" expected obtained
  
let tests_size_of_state = "tests_size_of_state", [
  test_size_of_state_0;
]

(*************************************************************************)
(*                                  eval                                 *)
(*************************************************************************)

let test_eval_0 () =
  let expected = true in
  let obtained = s1 |= (Bin (AP 0, And, AP 0)) in
  Alcotest.(check bool) "" expected obtained

let test_eval_1 () =
  let expected = true in
  let obtained = s2 |= (Bin (Not (AP 2), And, AP 0)) in
  Alcotest.(check bool) "" expected obtained

let test_eval_2 () =
  let expected = true in
  let obtained = s2 |= (Bin (True, Or, AP 1)) in
  Alcotest.(check bool) "" expected obtained

let test_eval_3 () =
  let expected = true in
  let obtained = s2 |= (Bin (False, Eq, Bin (True, Imp, AP 1))) in
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
  let obtained = s1 |= Know (0, AP 2) in
  Alcotest.(check bool) "" expected obtained

let test_eval_6 () =
  let expected = false in
  let obtained = s1 |= Know (1, AP 2) in
  Alcotest.(check bool) "" expected obtained

let test_eval_7 () =
  try
    let _ = s1 |= AP (-1) in
    Alcotest.fail "Aucune exception levée"
  with
  | UnknownProp -1 -> ()
  | _ -> Alcotest.fail "Mauvaise exception levée"
    
let test_eval_8 () =
  try
    let _ = s1 |= AP 99 in
    Alcotest.fail "Aucune exception levée"
  with
  | UnknownProp 99 -> ()
  | _ -> Alcotest.fail "Mauvaise exception levée"

let test_eval_9 () =
  try
    let _ = s1 |= Know (99, AP 0) in
    Alcotest.fail "Aucune exception levée"
  with
  | UnknownAgent 99 -> ()
  | _ -> Alcotest.fail "Mauvaise exception levée"
    
let test_eval_10 () =
  try
    let _ = s1 |= Know (-1, AP 0) in
    Alcotest.fail "Aucune exception levée"
  with
  | UnknownAgent -1 -> ()
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
  test_eval_8;
  test_eval_9;
  test_eval_10;
]

(*************************************************************************)
(*                              pp_of_state                              *)
(*************************************************************************)

let test_pp_of_state_0 () =
  let expected = "\
  +-----------------------------+\n\
  | State (actual world w^0_0): |\n\
  +-----------------------------+\n\
  | worlds:                     |\n\
  |   w^0_0 : m_a, d [ ]        |\n\
  |   w^0_1 : m_a [ ]           |\n\
  +-----------------------------+\n\
  | relations:                  |\n\
  |   →_a = {  }                |\n\
  |   →_b = { (w^0_0, w^0_1) }  |\n\
  +-----------------------------+\n"
  in
  let obtained = pp_of_state ctx1 s1 in
  Alcotest.(check string) "" expected obtained

let test_pp_of_state_1 () =
  try
    let _ = pp_of_state ctx1 (km1, -1) in
    Alcotest.fail "Aucune exception levée"
  with
  | UnknownWorld _ -> ()
  | _ -> Alcotest.fail "Mauvaise exception levée"

let test_pp_of_state_2 () =
  try
    let _ = pp_of_state ctx1 (km1, 99) in
    Alcotest.fail "Aucune exception levée"
  with
  | UnknownWorld _ -> ()
  | _ -> Alcotest.fail "Mauvaise exception levée"

let tests_pp_of_state = "pp_of_state", [
  test_pp_of_state_0;
  test_pp_of_state_1;
  test_pp_of_state_2;
]

(*************************************************************************)

let tests = [
  tests_max_ap_of_worlds;
  tests_size_of_kripke_model;
  tests_is_S5_model;
  tests_pp_of_kripke_model;
  tests_size_of_state;
  tests_eval;
  tests_pp_of_state;
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Epistemic logic - state" (format_tests tests) 