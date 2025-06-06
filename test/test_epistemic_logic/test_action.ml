open Epistemic_logic

(*****************************************************************************)
(*                                    Set up                                 *)
(*****************************************************************************)

let pp_fmla fmt f =
  Format.fprintf fmt "%s" (string_of_fmla f)

let testable_fmla = Alcotest.testable (pp_fmla) (=)

let ctx1 = {
  aps = [| "m_a" ; "m_b" ; "d" |];
  ags = [| "a" ; "b" |]
}

let e_bot = {
  eid = 0;
  pre  = True;
  post = [ 0, False ; 0, False ]
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
  pre  = Bin (AP 0, And, AP 1);
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


(* Other events *)

let e5 = { eid = 5; pre = AP 0; post = [] }

let ctx2 = {
  aps = [| "\xff" |];
  ags = [| |]
}

(*****************************************************************************)
(*                                    post                                   *)
(*****************************************************************************)

(* After event [e1], the atomic proposition [m_a] is set to [false]. *)
let test_post_0 () =
  let obtained = post e1 0 in
  let expected = False in
  Alcotest.(check testable_fmla) "" expected obtained

(* After event [e1], the atomic proposition [m_a] isn't changed : [post(e)(p) = p]. *)
let test_post_1 () =
  let obtained = post e1 2 in
  let expected = AP 2 in
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
(*                              max_ap_of_events                             *)
(*****************************************************************************)

let test_max_ap_of_events_0 () =
  let obtained = max_ap_of_events [e1] in
  let expected = 2 in
  Alcotest.(check int) "" expected obtained

let test_max_ap_of_events_1 () =
  let obtained = max_ap_of_events [e2] in
  let expected = 1 in
  Alcotest.(check int) "" expected obtained

let tests_max_ap_of_events = "max_ap_of_events", [
  test_max_ap_of_events_0;
  test_max_ap_of_events_1;
]

(*****************************************************************************)
(*                                pp_of_event                                *)
(*****************************************************************************)

let test_pp_of_event_0 () =
  let expected = "\
  +----------------+\n\
  | Event e_1      |\n\
  +----------------+\n\
  | pre: (m_a ∧ d) |\n\
  +----------------+\n\
  | post:          |\n\
  |   m_a := ⊥     |\n\
  |   m_b := ⊤     |\n\
  +----------------+\n"
  in
  let obtained = pp_of_event ctx1 e1 in
  Alcotest.(check string) "" expected obtained

let test_pp_of_event_1 () =
  let expected = "\
    +------------+\n\
    | Event e_0  |\n\
    +------------+\n\
    | pre: ⊤     |\n\
    +------------+\n\
    | post:      |\n\
    |   m_a := ⊥ |\n\
    |   m_a := ⊥ |\n\
    +------------+\n"
  in
  let obtained = pp_of_event ctx1 e_bot in
  Alcotest.(check string) "" expected obtained

let test_pp_of_event_2 () =
  let expected = "\
  +-----------+\n\
  | Event e_5 |\n\
  +-----------+\n\
  | pre: \255    |\n\
  +-----------+\n\
  | post:     |\n\
  +-----------+\n" in
  let obtained = pp_of_event ctx2 e5 in
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
  let expected = 16 in (* 2 + (2+4) + (5+3) *)
  Alcotest.(check int) "" expected obtained

let test_size_of_event_model_1 () =
  let obtained = size_of_event_model em2 in
  let expected = 16 in (* 2 + (2+4) + (5+3) *)
  Alcotest.(check int) "" expected obtained

let tests_size_of_event_model = "size_of_event_model", [
  test_size_of_event_model_0;
  test_size_of_event_model_1;
]

(*****************************************************************************)
(*                                size_of_action                             *)
(*****************************************************************************)

let test_size_of_action_0 () =
  let obtained = size_of_action send_ab in
  let expected = 16 in (* 2 + (2+4) + (5+3) *)
  Alcotest.(check int) "" expected obtained

let test_size_of_action_1 () =
  let obtained = size_of_action send_ba in
  let expected = 16 in (* 2 + (2+4) + (5+3) *)
  Alcotest.(check int) "" expected obtained

let tests_size_of_action = "size_of_action", [
  test_size_of_action_0;
  test_size_of_action_1;
]

(*****************************************************************************)

let tests = [
  tests_post;
  tests_size_of_event;
  tests_max_ap_of_events;
  tests_pp_of_event;
  tests_size_of_event_model;
  tests_size_of_action;
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Epistemic logic - actions" (format_tests tests) 