open OUnit2

type r
let rt : r Record.layout = Record.declare "r"
let x = Record.field rt "x" Type.int
let () = Record.seal rt

let set_get ctxt =
  let r = Record.make rt in
  Record.set r x 2;
  assert_equal 2 (Record.get r x)

let get_undef ctxt =
  let r = Record.make rt in
  let e = Record.UndefinedField "x" in
  assert_raises e @@ fun () ->
    Record.get r x

let extend_after_seal ctxt =
  let e = Record.ModifyingSealedStruct "r" in
  assert_raises e @@ fun () ->
    Record.field rt "y" Type.int

let seal_twice ctxt =
  let e = Record.ModifyingSealedStruct "r" in
  assert_raises e @@ fun () ->
    Record.seal rt

let suite =
  "Records" >:::
    [ "Set & get" >:: set_get
    ; "Get undefined field" >:: get_undef
    ; "Extend a sealed layout" >:: extend_after_seal
    ; "Seal a sealed layout" >:: seal_twice
    ]

let _ = run_test_tt_main suite
