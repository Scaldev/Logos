open Epistemic_logic

(*************************************************************************)
(*                             string_of_fmla                            *)
(*************************************************************************)

let test_string_of_fmla_0 () =
  let expected = "Know (0, AP 0)" in
  let obtained = string_of_fmla (Know (0, AP 0)) in
  Alcotest.(check string) "" expected obtained

let test_string_of_fmla_1 () =
  let expected = "Not (Bin (True, Or, False))" in
  let obtained = string_of_fmla (Not (Bin (True, Or, False))) in
  Alcotest.(check string) "" expected obtained

let test_string_of_fmla_2 () =
  let expected = "Bin (Bin (AP 2, Imp, AP 0), And, Bin (AP 1, Eq, AP 2))" in
  let obtained = string_of_fmla (Bin (Bin (AP 2, Imp, AP 0), And, Bin(AP 1, Eq, AP 2))) in
  Alcotest.(check string) "" expected obtained
  
let tests_string_of_fmla = "string_of_fmla", [
  test_string_of_fmla_0;
  test_string_of_fmla_1;
  test_string_of_fmla_2;
]

(*************************************************************************)
(*                          modal_depth_of_fmla                          *)
(*************************************************************************)

let test_modal_depth_of_fmla_0 () =
  let expected = 0 in
  let obtained = modal_depth_of_fmla (AP 0) in
  Alcotest.(check int) "" expected obtained

let test_modal_depth_of_fmla_1 () =
  let expected = 1 in
  let obtained = modal_depth_of_fmla (Know (0, Bin(AP 0, And, Not (AP 1)))) in
  Alcotest.(check int) "" expected obtained

let tests_modal_depth_of_fmla = "modal_depth_of_fmla", [
  test_modal_depth_of_fmla_0;
  test_modal_depth_of_fmla_1;
]

(*************************************************************************)
(*                             size_of_fmla                              *)
(*************************************************************************)

let test_size_of_fmla_0 () =
  let expected = 1 in
  let obtained = size_of_fmla (AP 0) in
  Alcotest.(check int) "" expected obtained

let test_size_of_fmla_1 () =
  let expected = 5 in
  let obtained = size_of_fmla (Know (0, Bin(AP 0, And, Not (AP 1)))) in
  Alcotest.(check int) "" expected obtained

let tests_size_of_fmla = "size_of_fmla", [
  test_size_of_fmla_0;
  test_size_of_fmla_1;
]

(*************************************************************************)
(*                                pp_of_fmla                             *)
(*************************************************************************)

let test_pp_of_fmla_0 () =
  let f = Know (0, AP 0) in
  let c = { aps = [|"p"|] ; ags = [|"a"|] } in
  let expected = "K_a p" in
  let obtained = pp_of_fmla c f in
  Alcotest.(check string) "" expected obtained

let test_pp_of_fmla_1 () =
  let f = Not(Bin(True, Or, False)) in
  let c = { aps = [||] ; ags = [||] } in
  let expected = "¬(⊤ ∨ ⊥)" in
  let obtained = pp_of_fmla c f in
  Alcotest.(check string) "" expected obtained

let test_pp_of_fmla_2 () =
  let f = Bin (Bin (AP 0, Imp, AP 1), And, Bin (AP 2, Eq, AP 3)) in
  let c = { aps = [|"p" ; "q" ; "r" ; "s"|] ; ags = [||] } in
  let expected = "((p → q) ∧ (r ↔ s))" in
  let obtained = pp_of_fmla c f in
  Alcotest.(check string) "" expected obtained
  
let test_pp_of_fmla_3 () =
  let f = Bin (Not (AP 0), Or, Know (0, Know (1, AP 1))) in
  let c = { aps = [|"p" ; "q"|] ; ags = [|"a" ; "b"|] } in
  let expected = "(¬p ∨ K_a K_b q)" in
  let obtained = pp_of_fmla c f in
  Alcotest.(check string) "" expected obtained

let tests_pp_of_fmla = "pp_of_fmla", [
  test_pp_of_fmla_0;
  test_pp_of_fmla_1;
  test_pp_of_fmla_2;
  test_pp_of_fmla_3;
]

(*************************************************************************)

let tests = [
  tests_string_of_fmla;
  tests_pp_of_fmla;
  tests_modal_depth_of_fmla;
  tests_size_of_fmla;
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Epistemic logic - syntax" (format_tests tests) 