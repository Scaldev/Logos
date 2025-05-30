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
  let expected = "Know(0, AP(0))" in
  let obtained = string_of_fmla (Know(0, AP 0)) in
  Alcotest.(check string) "" expected obtained

let test_string_of_fmla_1 () =
  let expected = "Not(Bin(True, Or, False))" in
  let obtained = string_of_fmla (Not(Bin(True, Or, False))) in
  Alcotest.(check string) "" expected obtained

let test_string_of_fmla_2 () =
  let expected = "Bin(Bin(AP(0), Imp, AP(1)), And, Bin(AP(1), Eq, AP(2)))" in
  let obtained = string_of_fmla (Bin(Bin(AP(0), Imp, AP(1)), And, Bin(AP(1), Eq, AP(2)))) in
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
  let obtained = modal_depth_of_fmla (AP 0) in
  Alcotest.(check int) "" expected obtained

let test_modal_depth_of_fmla_1 () =
  let expected = 1 in
  let obtained = modal_depth_of_fmla (Know(1, Bin(AP 0, And, Not(AP 1)))) in
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
  let obtained = size_of_fmla (AP 0) in
  Alcotest.(check int) "" expected obtained

let test_size_of_fmla_1 () =
  let expected = 5 in
  let obtained = size_of_fmla (Know(1, Bin(AP 0, And, Not(AP 1)))) in
  Alcotest.(check int) "" expected obtained

let tests_size_of_fmla = [
  test_size_of_fmla_0;
  test_size_of_fmla_1;
]

(*************************************************************************)
(*                             max_ap_in_fmla                            *)
(*************************************************************************)

let test_max_ap_in_fmla_0 () =
  let expected = 0 in
  let obtained = max_ap_in_fmla False in
  Alcotest.(check int) "" expected obtained

let test_max_ap_in_fmla_1 () =
  let expected = 1 in
  let obtained = max_ap_in_fmla (AP 1) in
  Alcotest.(check int) "" expected obtained

let test_max_ap_in_fmla_2 () =
  let expected = 5 in
  let obtained = max_ap_in_fmla (Know(1, Bin(AP 5, And, Not(AP 1)))) in
  Alcotest.(check int) "" expected obtained

let tests_max_ap_in_fmla = [
  test_max_ap_in_fmla_0;
  test_max_ap_in_fmla_1;
  test_max_ap_in_fmla_2;
]

(*************************************************************************)
(*                             max_ap_in_fmla                            *)
(*************************************************************************)

let test_max_ag_in_fmla_0 () =
  let expected = 0 in
  let obtained = max_ag_in_fmla False in
  Alcotest.(check int) "" expected obtained

let test_max_ag_in_fmla_1 () =
  let expected = 1 in
  let obtained = max_ag_in_fmla (Know(1, False)) in
  Alcotest.(check int) "" expected obtained

let test_max_ag_in_fmla_2 () =
  let expected = 4 in
  let obtained = max_ag_in_fmla (Know(4, Bin(AP 1, And, Not(AP 1)))) in
  Alcotest.(check int) "" expected obtained

let tests_max_ag_in_fmla = [
  test_max_ag_in_fmla_0;
  test_max_ag_in_fmla_1;
  test_max_ag_in_fmla_2;
]

(*************************************************************************)
(*                                pp_of_fmla                             *)
(*************************************************************************)

let test_pp_of_fmla_0 () =
  let f = Know(0, AP 0) in
  let aps, ags = default_legend f in
  let expected = "K_0 p_0" in
  let obtained = pp_of_fmla aps ags f in
  Alcotest.(check string) "" expected obtained

let test_pp_of_fmla_1 () =
  let f = Not(Bin(True, Or, False)) in
  let aps, ags = default_legend f in
  let expected = "\194\172(\226\138\164 \226\136\168 \226\138\165)" in
  let obtained = pp_of_fmla aps ags f in
  Alcotest.(check string) "" expected obtained

let test_pp_of_fmla_2 () =
  let f = Bin(Bin(AP(0), Imp, AP(1)), And, Bin(AP(1), Eq, AP(2))) in
  let aps, ags = default_legend f in
  let expected = "((p_0 \226\134\146 p_1) \226\136\167 (p_1 \226\134\148 p_2))" in
  let obtained = pp_of_fmla aps ags f in
  Alcotest.(check string) "" expected obtained
  
let test_pp_of_fmla_3 () =
  let f = Bin((Not(AP(1))), Or, (Know(0, Know(1, AP(0))))) in
  let aps = [| "p"; "q" |] in
  let ags = [| "a"; "b" |] in
  let expected = "(\194\172q \226\136\168 K_a K_b p)" in
  let obtained = pp_of_fmla aps ags f in
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
  "tests_max_ap_in_fmla", tests_max_ap_in_fmla;
  "tests_max_ag_in_fmla", tests_max_ag_in_fmla
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Epistemic logic (syntax)" (format_tests tests) 