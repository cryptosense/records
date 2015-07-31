open OUnit2

let tautology = "tautology" >:: fun ctxt ->
  assert_equal ~ctxt 3 3

let suite =
  "Records" >::: [
    tautology
  ]

let _ = run_test_tt_main suite
