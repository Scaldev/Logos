open Epistemic_logic

(*************************************************************************)
(*                        max_ap_in_event_model                          *)
(*************************************************************************)

(* Event model for the two generals problem *)

let aps = [| "m_a"; "m_b"; "d" |]
let ags = [| "a"; "b" |]

(* Create send_ab action *)

let e1 = {
  pre  = Bin (AP 1, And, AP 2);
  post = [| Some False; Some True; None |]
}

let e2 = {
  pre  = True;
  post = [| Some False; Some False; None |]
}

let r1a = (e1, e1) :: (e1, e2) :: (e2, e2) :: []
let r1b = (e1, e1) :: (e2, e2) :: []

let em1 = {
  events = e1 :: e2 :: [];
  rels   = [| r1a; r1b |]
}

let send_ab = (em1, 0)

(* Create send_ba action *)

let e3 = {
  pre  = Bin (AP 0, And, AP 2);
  post = [| Some True; Some False; None |]
}

let e4 = {
  pre  = True;
  post = [| Some False; Some False; None |]
}

let r2a = (e3, e3) :: (e3, e3) :: []
let r2b = (e3, e3) :: (e3, e4) :: (e4, e4) :: []

let em2 = {
  events = e3 :: e3 :: [];
  rels   = [| r2a; r2b |]
}

let send_ba = (em2, 0)

let b = send_ab == send_ba

let () = print_string (string_of_bool b)

(*************************************************************************)
(*                             string_of_fmla                            *)
(*************************************************************************)

let test_pp_of_event_0 () =
  let expected = "\
    +------------+\n\
    | pre: (m_b \226\136\167 d)|\n\
    +------------+\n\
    | post:   |\n\
    | m_a:=\226\138\165|\n\
    | m_b:=\226\138\164|\n\
    +------------+\n"
  in
  let obtained = pp_of_event aps ags e1 in
  Alcotest.(check string) "" expected obtained

let tests_pp_of_event = "pp_of_event", [
  test_pp_of_event_0;
]

(*************************************************************************)

let tests = [
  tests_pp_of_event;
]

let format_tests (tss: (string * (Alcotest.return -> Alcotest.return) list) list) =
  List.map (fun ts ->
    (fst ts, List.map (fun t -> Alcotest.test_case "" `Quick t) (snd ts))
  ) tss

let () = Alcotest.run "Epistemic logic (semantics)" (format_tests tests) 