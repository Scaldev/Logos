
open Alcotest

open Epistemic_logic

(*************************************************************************)

(**
  Fail with msg iff eq a b == false.
  @param msg the message to print iff a and b are not equal.
  @param eq an equality.
  @param a un variable.
  @return b a variable.
*)
let check_not_eq (msg: string) (eq: 'a -> 'b -> bool) (a: 'a) (b: 'b) : unit =
  if eq a b then Alcotest.fail msg

let pp_kfmla fmt f =
  Format.fprintf fmt "%s" (string_of_kfmla f)

let formula_testable = Alcotest.testable pp_kfmla equal_kfmla

(*************************************************************************)
(*                              equal_kfmla                              *)
(*************************************************************************)

let test_equal_kfmla_0 () =
  let f1 = Know(1, And(AP 0, Not(AP 1))) in
  let f2 = Know(1, And(AP 0, Not(AP 1))) in
  Alcotest.(check formula_testable) "f1 == f2" f1 f2

let test_equal_kfmla_1 () =
  let f1 = AP 0 in
  let f2 = Not (AP 1) in
  check_not_eq "f1 <> f2" equal_kfmla f1 f2


let tests_equal_kfmla = [
  test_equal_kfmla_0;
  test_equal_kfmla_1;
]

(*************************************************************************)
(*                                pp_kfmla                               *)
(*************************************************************************)
let test_string_of_kfmla_0 () =
  let expected = "Know(1, And(AP(0), Not(AP(1))))" in
  let obtained = string_of_kfmla (Know(1, And(AP 0, Not(AP 1)))) in
  Alcotest.(check string) "" expected obtained

let tests_string_of_kfmla = [
  test_string_of_kfmla_0;
]

(*************************************************************************)
(*                               el_form_of                              *)
(*************************************************************************)

let test_el_form_of_0 () =
  let expected = Not(And(Not(AP(0)), Not(Not(AP(0))))) in
  let obtained = el_form_of True in
  Alcotest.(check formula_testable) "" expected obtained

let test_el_form_of_1 () =
  let expected = Not(And(Not(AP 0), Not(And(AP 0, Not(AP 0))))) in
  let obtained = el_form_of (Or(AP 0, False)) in
  Alcotest.(check formula_testable) "" expected obtained

let test_el_form_of_2 () =
  let expected = Know(1, And(AP 0, Not(AP 1))) in
  let obtained = el_form_of (Know(1, And(AP 0, Not(AP 1)))) in
  Alcotest.(check formula_testable) "" expected obtained

let test_el_form_of_3 () =
  let expected = And(Not(And(Not(Not(AP(0))), Not(AP(1)))), Not(And(Not(Not(AP(1))), Not(AP(0))))) in
  let obtained = el_form_of (Eq(AP 0, AP 1)) in
  Alcotest.(check formula_testable) "" expected obtained

let tests_el_form_of = [
  test_el_form_of_0;
  test_el_form_of_1;
  test_el_form_of_2;
  test_el_form_of_3;
]

(*************************************************************************)
(*                              modal_depth                              *)
(*************************************************************************)

let test_modal_depth_0 () =
  let expected = 0 in
  let obtained = modal_depth (AP 0) in
  Alcotest.(check int) "" expected obtained

let test_modal_depth_1 () =
  let expected = 1 in
  let obtained = modal_depth (Know(1, And(AP 0, Not(AP 1)))) in
  Alcotest.(check int) "" expected obtained

let tests_modal_depth = [
  test_modal_depth_0;
  test_modal_depth_1;
]

(*************************************************************************)
(*                                 size                                  *)
(*************************************************************************)

let test_size_0 () =
  let expected = 0 in
  let obtained = modal_depth (AP 0) in
  Alcotest.(check int) "" expected obtained

let test_size_1 () =
  let expected = 5 in
  let obtained = size (Know(1, And(AP 0, Not(AP 1)))) in
  Alcotest.(check int) "" expected obtained

let tests_size = [
  test_size_0;
  test_size_1;
]

(*************************************************************************)

let tests = [
  "equal_kflma",     tests_equal_kfmla;
  "string_of_kfmla", tests_string_of_kfmla;
  "el_form_of",      tests_el_form_of;
  "modal_depth",     tests_modal_depth;
  "size",            tests_size;
]

let format_tests (tss: (string * (return -> return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> test_case "" `Quick t) (snd ts))
  ) tss

let () =
  run "Epistemic logic" (format_tests tests) 