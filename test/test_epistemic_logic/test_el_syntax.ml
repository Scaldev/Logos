open Epistemic_logic

(*
let pp_fmla fmt f =
  Format.fprintf fmt "%s" (string_of_fmla f)

let formula_testable = Alcotest.testable pp_fmla (=)
*)

(*************************************************************************)
(*                             string_of_fmla                            *)
(*************************************************************************)

let test_string_of_fmla_0 () =
  let expected = "Know (\"a\", AP \"p\")" in
  let obtained = string_of_fmla (Know ("a", AP "p")) in
  Alcotest.(check string) "" expected obtained

let test_string_of_fmla_1 () =
  let expected = "Not (Bin (True, Or, False))" in
  let obtained = string_of_fmla (Not (Bin (True, Or, False))) in
  Alcotest.(check string) "" expected obtained

let test_string_of_fmla_2 () =
  let expected = "Bin (Bin (AP \"a\", Imp, AP \"p\"), And, Bin (AP \"q\", Eq, AP \"r\"))" in
  let obtained = string_of_fmla (Bin (Bin (AP "a", Imp, AP "p"), And, Bin(AP "q", Eq, AP "r"))) in
  Alcotest.(check string) "" expected obtained
  
let tests_string_of_fmla = [
  test_string_of_fmla_0;
  test_string_of_fmla_1;
  test_string_of_fmla_2;
]

(*************************************************************************)
(*                          modal_depth_of_fmla                          *)
(*************************************************************************)

let test_modal_depth_of_fmla_0 () =
  let expected = 0 in
  let obtained = modal_depth_of_fmla (AP "p") in
  Alcotest.(check int) "" expected obtained

let test_modal_depth_of_fmla_1 () =
  let expected = 1 in
  let obtained = modal_depth_of_fmla (Know ("a", Bin(AP "p", And, Not (AP "q")))) in
  Alcotest.(check int) "" expected obtained

let tests_modal_depth_of_fmla = [
  test_modal_depth_of_fmla_0;
  test_modal_depth_of_fmla_1;
]

(*************************************************************************)
(*                             size_of_fmla                              *)
(*************************************************************************)

let test_size_of_fmla_0 () =
  let expected = 1 in
  let obtained = size_of_fmla (AP "p") in
  Alcotest.(check int) "" expected obtained

let test_size_of_fmla_1 () =
  let expected = 5 in
  let obtained = size_of_fmla (Know ("a", Bin(AP "p", And, Not (AP "q")))) in
  Alcotest.(check int) "" expected obtained

let tests_size_of_fmla = [
  test_size_of_fmla_0;
  test_size_of_fmla_1;
]

(*************************************************************************)
(*                                pp_of_fmla                             *)
(*************************************************************************)

let test_pp_of_fmla_0 () =
  let f = Know ("a", AP "p") in
  let expected = "K_a p" in
  let obtained = pp_of_fmla f in
  Alcotest.(check string) "" expected obtained

let test_pp_of_fmla_1 () =
  let f = Not(Bin(True, Or, False)) in
  let expected = "¬(⊤ ∨ ⊥)" in
  let obtained = pp_of_fmla f in
  Alcotest.(check string) "" expected obtained

let test_pp_of_fmla_2 () =
  let f = Bin (Bin (AP "p", Imp, AP "q"), And, Bin (AP "r", Eq, AP "s")) in
  let expected = "((p → q) ∧ (r ↔ s))" in
  let obtained = pp_of_fmla f in
  Alcotest.(check string) "" expected obtained
  
let test_pp_of_fmla_3 () =
  let f = Bin (Not (AP "p"), Or, Know ("a", Know ("b", AP "q"))) in
  let expected = "(¬p ∨ K_a K_b q)" in
  let obtained = pp_of_fmla f in
  Alcotest.(check string) "" expected obtained

let tests_pp_of_fmla = [
  test_pp_of_fmla_0;
  test_pp_of_fmla_1;
  test_pp_of_fmla_2;
  test_pp_of_fmla_3;
]

(*************************************************************************)

let tests = [
  "string_of_fmla",       tests_string_of_fmla;
  "tests_pp_of_fmla",     tests_pp_of_fmla;
  "modal_depth_of_fmla",  tests_modal_depth_of_fmla;
  "size_of_fmla",         tests_size_of_fmla;
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Epistemic logic (syntax)" (format_tests tests) 