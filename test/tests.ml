open OUnit2

type r
let rt : r Record.layout = Record.Unsafe.declare "r"
let x = Record.Unsafe.field rt "x" Record.Type.int
let () = Record.Unsafe.seal rt

module Safe_layouts =
struct
  module Rt = (val Record.Safe.declare "r")
  let x = Rt.field "x" Record.Type.int
  let () = Rt.seal ()

  module Rres = (val Record.Safe.declare "rr")
  let value_r1 = Rres.field "r1" (Record.Type.result Record.Type.int Record.Type.string)
  let value_r2 = Rres.field "r2" (Record.Type.result Record.Type.int Record.Type.string)
  let () = Rres.seal ()
end

let set_get ctxt =
  let r = Record.Unsafe.make rt in
  Record.set r x 2;
  assert_equal 2 (Record.get r x)

let safe_set_get ctxt =
  let open Safe_layouts in
  let r = Rt.make () in
  Record.set r x 2;
  assert_equal 2 (Record.get r x)

let get_undef ctxt =
  let r = Record.Unsafe.make rt in
  let e = Record.UndefinedField "x" in
  assert_raises e (fun () ->
    Record.get r x
  )

let safe_get_undef ctxt =
  let open Safe_layouts in
  let r = Rt.make () in
  let e = Record.UndefinedField "x" in
  assert_raises e (fun () ->
    Record.get r x
  )

let extend_after_seal ctxt =
  let e = Record.ModifyingSealedStruct "r" in
  assert_raises e (fun () ->
    Record.Unsafe.field rt "y" Record.Type.int
  )

let safe_extend_after_seal ctxt =
  let open Safe_layouts in
  let e = Record.ModifyingSealedStruct "r" in
  assert_raises e (fun () ->
    Record.Unsafe.field Rt.layout "y" Record.Type.int
  )

let seal_twice ctxt =
  let e = Record.ModifyingSealedStruct "r" in
  assert_raises e (fun () ->
    Record.Unsafe.seal rt
  )

let safe_seal_twice ctxt =
  let open Safe_layouts in
  let e = Record.ModifyingSealedStruct "r" in
  assert_raises e (fun () ->
    Record.Unsafe.seal Rt.layout
  )

let make_unsealed ctxt (type r2) =
  let rt2 : r2 Record.layout = Record.Unsafe.declare "r2" in
  let _x2 = Record.Unsafe.field rt2 "x2" Record.Type.int in
  let e = Record.AllocatingUnsealedStruct "r2" in
  assert_raises e (fun () ->
    Record.Unsafe.make rt2
  )

let safe_make_unsealed ctxt (type r2) =
  let module Rt2 = (val Record.Safe.declare "r2") in
  let _x2 = Rt2.field "x2" Record.Type.int in
  let e = Record.AllocatingUnsealedStruct "r2" in
  assert_raises e (fun () ->
    Rt2.make ()
  )

let layout_name ctxt =
  assert_equal "r" (Record.Unsafe.layout_name rt)

let safe_layout_name ctxt =
  assert_equal "r" Safe_layouts.Rt.layout_name

let layout_id ctxt =
  let id1 = Record.Unsafe.layout_id rt in
  let id2 = Record.Unsafe.layout_id rt in
  let id3 = Record.Polid.fresh () in
  assert_bool "layout_id is pure" (Record.Polid.equal id1 id2 = Record.Polid.Equal);
  assert_equal ~msg:"layout_id is pure (int)" (Record.Polid.to_int id1) (Record.Polid.to_int id2);
  assert_bool "fresh returns a different id" (Record.Polid.equal id1 id3 = Record.Polid.Different)

let safe_layout_id ctxt =
  let open Safe_layouts in
  let id1 = Rt.layout_id in
  let id2 = Record.Polid.fresh () in
  assert_bool "fresh returns a different id" (Record.Polid.equal id1 id2 = Record.Polid.Different)

let field_name ctxt =
  assert_equal "x" (Record.Field.name x)

let safe_field_name ctxt =
  assert_equal "x" (Record.Field.name Safe_layouts.x)

let field_type ctxt =
  assert_equal "int" (Record.Type.name (Record.Field.ftype x))

let safe_field_type ctxt =
  assert_equal "int" (Record.Type.name (Record.Field.ftype Safe_layouts.x))

let record_layout ctxt =
  let r = Record.Unsafe.make rt in
  let l = Record.get_layout r in
  assert_bool "layout is the same"
    (Record.Polid.is_equal
      (Record.Unsafe.layout_id l)
      (Record.Unsafe.layout_id rt)
    )

let safe_record_layout ctxt =
  let open Safe_layouts in
  let r = Rt.make () in
  let l = Record.get_layout r in
  assert_bool "layout is the same"
    (Record.Polid.is_equal
      (Record.Unsafe.layout_id l)
      Rt.layout_id
    )

let force = function
  | Ok x -> x
  | Error _ -> assert false

let of_json ctxt =
  let j = `Assoc [("x", `Int 2)] in
  let r = force @@ Record.of_yojson rt j in
  assert_equal 2 (Record.get r x)

let safe_of_json ctxt =
  let j = `Assoc [("x", `Int 2)] in
  let r = force @@ Record.of_yojson Safe_layouts.Rt.layout j in
  assert_equal 2 (Record.get r Safe_layouts.x)

let to_json ctxt =
  let r = Record.Unsafe.make rt in
  Record.set r x 2;
  let expected = `Assoc [("x", `Int 2)] in
  let printer = Yojson.Safe.pretty_to_string in
  assert_equal ~printer expected (Record.to_yojson r)

let safe_to_json ctxt =
  let open Safe_layouts in
  let r = Rt.make () in
  Record.set r x 2;
  let expected = `Assoc [("x", `Int 2)] in
  let printer = Yojson.Safe.pretty_to_string in
  assert_equal ~printer expected (Record.to_yojson r)

let to_json_null ctxt =
  let r = Record.Unsafe.make rt in
  let expected = `Assoc [("x", `Null)] in
  let printer = Yojson.Safe.pretty_to_string in
  assert_equal ~printer expected (Record.to_yojson r)

let safe_to_json_null ctxt =
  let open Safe_layouts in
  let r = Rt.make () in
  let expected = `Assoc [("x", `Null)] in
  let printer = Yojson.Safe.pretty_to_string in
  assert_equal ~printer expected (Record.to_yojson r)

let safe_json_result ctxt =
  let open Safe_layouts in
  let r = Rres.make () in
  Record.set r value_r1 (Ok 35);
  Record.set r value_r2 (Error "no");
  let json =
    `Assoc
      [ "r1", `Assoc ["Ok", `Int 35]
      ; "r2", `Assoc ["Error", `String "no"]
      ]
  in
  let printer = Yojson.Safe.pretty_to_string in
  assert_equal ~printer json (Record.to_yojson r);
  let recovered_1 = force @@ Record.of_yojson Rres.layout json in
  assert_equal (Ok 35) (Record.get recovered_1 value_r1);
  let recovered_2 = force @@ Record.of_yojson Rres.layout json in
  assert_equal (Error "no") (Record.get recovered_2 value_r2)

let declare0 ctxt =
  let l = Record.Util.declare0 ~name:"r" in
  assert_equal "r" (Record.Unsafe.layout_name l)

let declare1 ctxt =
  let (l, f) = Record.Util.declare1 ~name:"r" ~f1_name:"x" ~f1_type:Record.Type.int in
  assert_equal "r" (Record.Unsafe.layout_name l);
  assert_equal "x" (Record.Field.name f)

let declare2 ctxt =
  let (l, f1, f2) =
    Record.Util.declare2 ~name:"r"
      ~f1_name:"f1" ~f1_type:Record.Type.int
      ~f2_name:"f2" ~f2_type:Record.Type.int
  in
  assert_equal "r" (Record.Unsafe.layout_name l);
  assert_equal "f1" (Record.Field.name f1);
  assert_equal "f2" (Record.Field.name f2)

let declare3 ctxt =
  let (l, f1, f2, f3) =
    Record.Util.declare3 ~name:"r"
      ~f1_name:"f1" ~f1_type:Record.Type.int
      ~f2_name:"f2" ~f2_type:Record.Type.int
      ~f3_name:"f3" ~f3_type:Record.Type.int
  in
  assert_equal "r" (Record.Unsafe.layout_name l);
  assert_equal "f1" (Record.Field.name f1);
  assert_equal "f2" (Record.Field.name f2);
  assert_equal "f3" (Record.Field.name f3)

let declare4 ctxt =
  let (l, f1, f2, f3, f4) =
    Record.Util.declare4 ~name:"r"
      ~f1_name:"f1" ~f1_type:Record.Type.int
      ~f2_name:"f2" ~f2_type:Record.Type.int
      ~f3_name:"f3" ~f3_type:Record.Type.int
      ~f4_name:"f4" ~f4_type:Record.Type.int
  in
  assert_equal "r" (Record.Unsafe.layout_name l);
  assert_equal "f1" (Record.Field.name f1);
  assert_equal "f2" (Record.Field.name f2);
  assert_equal "f3" (Record.Field.name f3);
  assert_equal "f4" (Record.Field.name f4)

let layout_type ctxt =
  let rt_typ = Record.Util.layout_type rt in
  let (la, fa1, fa2) =
    Record.Util.declare2 ~name:"pair"
      ~f1_name:"f1" ~f1_type:rt_typ
      ~f2_name:"f2" ~f2_type:rt_typ
  in
  let r = Record.Unsafe.make rt in
  Record.set r x 3;
  let rp = Record.Unsafe.make la in
  Record.set rp fa1 r;
  Record.set rp fa2 r;
  let printer = Yojson.Safe.pretty_to_string in
  let expected =
    `Assoc
      [ ("f1", `Assoc [("x", `Int 3)])
      ; ("f2", `Assoc [("x", `Int 3)])
      ]
  in
  assert_equal ~ctxt ~printer expected (Record.to_yojson rp)

let safe_layout_type ctxt =
  let open Safe_layouts in
  let rt_typ = Record.Util.layout_type Rt.layout in
  let module La = (val Record.Safe.declare "pair") in
  let fa1 = La.field "f1" rt_typ in
  let fa2 = La.field "f2" rt_typ in
  let () = La.seal () in
  let r = Rt.make () in
  Record.set r x 3;
  let rp = La.make () in
  Record.set rp fa1 r;
  Record.set rp fa2 r;
  let printer = Yojson.Safe.pretty_to_string in
  let expected =
    `Assoc
      [ ("f1", `Assoc [("x", `Int 3)])
      ; ("f2", `Assoc [("x", `Int 3)])
      ]
  in
  assert_equal ~ctxt ~printer expected (Record.to_yojson rp)

let view ctxt =
  let read n =
    match Char.chr n with
    | c -> Ok c
    | exception (Invalid_argument _) -> Error "read"
  in
  let write c =
    Char.code c
  in
  let char_as_int =
    let open Record.Type in
    view
      ~name:"char_as_int"
      ~read
      ~write
      int
  in
  let j = `Int 0x41 in
  let c = 'A' in
  assert_equal ~ctxt (Ok c) (Record.Type.of_yojson char_as_int j);
  assert_equal ~ctxt j (Record.Type.to_yojson char_as_int c)

let suite =
  "Records" >:::
    [ "Set & get" >:: set_get
    ; "Get undefined field" >:: get_undef
    ; "Extend a sealed layout" >:: extend_after_seal
    ; "Seal a sealed layout" >:: seal_twice
    ; "Instanciate an unsealed layout" >:: make_unsealed
    ; "Layout name" >:: layout_name
    ; "Layout id" >:: layout_id
    ; "Field name" >:: field_name
    ; "Field type" >:: field_type
    ; "Record layout" >:: record_layout
    ; "JSON reader" >:: of_json
    ; "JSON writer" >:: to_json
    ; "JSON writer (null field)" >:: to_json_null
    ; "declare0" >:: declare0
    ; "declare1" >:: declare1
    ; "declare2" >:: declare2
    ; "declare3" >:: declare3
    ; "declare4" >:: declare4
    ; "layout_type" >:: layout_type
    ; "view" >:: view
    ; "Safe set & get" >:: safe_set_get
    ; "Safe get undefined field" >:: safe_get_undef
    ; "Safe extend a sealed layout" >:: safe_extend_after_seal
    ; "Safe seal a sealed layout" >:: safe_seal_twice
    ; "Safe instanciate an unsealed layout" >:: safe_make_unsealed
    ; "Safe layout name" >:: safe_layout_name
    ; "Safe layout id" >:: safe_layout_id
    ; "Safe field name" >:: safe_field_name
    ; "Safe field type" >:: safe_field_type
    ; "Safe record layout" >:: safe_record_layout
    ; "Safe JSON reader" >:: safe_of_json
    ; "Safe JSON writer" >:: safe_to_json
    ; "Safe JSON writer (null field)" >:: safe_to_json_null
    ; "Safe JSON (result)" >:: safe_json_result
    ; "Safe layout_type" >:: safe_layout_type
    ]

let _ = run_test_tt_main suite
