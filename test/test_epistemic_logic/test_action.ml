open Epistemic_logic

(* Add formula equality *)

let pp_fmla fmt f =
  Format.fprintf fmt "%s" (string_of_fmla f)

let testable_fmla = Alcotest.testable (pp_fmla) (=)

(* Event model for the two generals problem *)

(* Create send_ab action *)

let e1 = {
  pre  = Bin (AP "m_a", And, AP "d");
  post = [ "m_a", False ; "m_b", True ]
}
let e2 = {
  pre  = True;
  post = [ "m_a", False ; "m_b", False ]
}

let r1a = [ e1, e1 ; e1, e2 ; e2, e2 ]
let r1b = [ e1, e1 ; e2, e2 ]

let em1 = {
  events    = [ e1 ; e2 ];
  relations = [ "a", r1a ; "b", r1b ]
}
let send_ab = (em1, e1)

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
let r2b = [ e3, e3 ; e3, e4 ; e4, e4 ]

let em2 = {
  events    = [ e3 ; e4 ];
  relations = [ "a", r2a ; "b", r2b ]
}
let send_ba = (em2, e3)

(* Other events *)

let e5 = { pre  = AP "\xff"; post = [] }

let b = send_ab == send_ba

let () = print_string (string_of_bool b)

(*****************************************************************************)
(*                                    post                                   *)
(*****************************************************************************)

(* After event [e1], the atomic proposition [m_a] is set to [false]. *)
let test_post_0 () =
  let obtained = post e1 "m_a" in
  let expected = False in
  Alcotest.(check testable_fmla) "" expected obtained

(* After event [e1], the atomic proposition [m_a] isn't changed : [post(e)(p) = p]. *)
let test_post_1 () =
  let obtained = post e1 "d" in
  let expected = AP "d" in
  Alcotest.(check testable_fmla) "" expected obtained

let tests_post = "post", [
  test_post_0;
  test_post_1;
]

(*****************************************************************************)
(*                                size_of_event                              *)
(*****************************************************************************)

let test_size_of_event_0 () =
  let obtained = size_of_event e1 in
  let expected = 5 in (* precondition = size 3, postconditions = 2 * size 1 *)
  Alcotest.(check int) "" expected obtained

let tests_size_of_event = "size_of_event", [
  test_size_of_event_0;
]

(*****************************************************************************)
(*                                aps_of_events                              *)
(*****************************************************************************)

let test_aps_of_events_0 () =
  let obtained = aps_of_events [] in
  let expected = [] in
  Alcotest.(check (list string)) "" expected obtained

let test_aps_of_events_1 () =
  let obtained = aps_of_events [e1 ; e2] in
  let expected = ["d" ; "m_a" ; "m_b"] in
  Alcotest.(check (list string)) "" expected obtained

let tests_aps_of_events = "aps_of_events", [
  test_aps_of_events_0;
  test_aps_of_events_1;
]

(*****************************************************************************)
(*                                pp_of_event                                *)
(*****************************************************************************)

let test_pp_of_event_0 () =
  let expected = "\
  +----------------+\n\
  | pre: (m_a ∧ d) |\n\
  +----------------+\n\
  | post:          |\n\
  |   m_a := ⊥     |\n\
  |   m_b := ⊤     |\n\
  +----------------+\n"
  in
  let obtained = pp_of_event e1 in
  Alcotest.(check string) "" expected obtained

let test_pp_of_event_1 () =
  let expected = "\
  +------------+\n\
  | pre: ⊤     |\n\
  +------------+\n\
  | post:      |\n\
  |   m_a := ⊥ |\n\
  |   m_b := ⊥ |\n\
  +------------+\n"
  in
  let obtained = pp_of_event e2 in
  Alcotest.(check string) "" expected obtained

let test_pp_of_event_2 () =
  let expected = "\
  +--------+\n\
  | pre: \255 |\n\
  +--------+\n\
  | post:  |\n\
  +--------+\n" in
  let obtained = pp_of_event e5 in
  Alcotest.(check string) "" expected obtained

    
let tests_pp_of_event = "pp_of_event", [
  test_pp_of_event_0;
  test_pp_of_event_1;
  test_pp_of_event_2;
]

(*****************************************************************************)
(*                             size_of_event_model                           *)
(*****************************************************************************)

let test_size_of_event_model_0 () =
  let obtained = size_of_event_model em1 in
  let expected = 15 in (* 2 + (2+3) + (5+3) *)
  Alcotest.(check int) "" expected obtained

let tests_size_of_event_model = "size_of_event_model", [
  test_size_of_event_model_0;
]

(*****************************************************************************)

let tests = [
  tests_post;
  tests_size_of_event;
  tests_aps_of_events;
  tests_pp_of_event;
  tests_size_of_event_model;
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Epistemic logic - actions" (format_tests tests) 