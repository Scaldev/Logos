open Epistemic_logic

let test_modal_depth_1 () =
  let expected = 0 in
  let obtained = modal_depth (AP 0) in
  Alcotest.(check int) "same depth" expected obtained


let () =
  let open Alcotest in
  run "Epistemic logic" [
    "Modal depth", [
      test_case "Modal depth 1" `Quick test_modal_depth_1;
    ]
  ]