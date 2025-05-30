open Epistemic_logic

(* Event model for the two generals problem *)

let aps = [| "m_a"; "m_b"; "d" |]
let ags = [| "a"; "b" |]

(* Create send_ab action *)

let e1 = {
  pre  = Bin (AP 0, And, AP 2);
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

let () = print_string (string_of_bool b) ; print_newline ()

let s = pp_of_event aps ags e1

let exec () = print_string s