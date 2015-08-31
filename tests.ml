open OUnit2

type r
let rt : r Record.layout = Record.declare "r"
let x = Record.field rt "x" Type.int
let () = Record.seal rt

type p
let rpt : p Record.layout = Record.declare "rp"
let value_p = Record.field rpt "value_pair" (Type.product_2 "fst" Type.int "snd" Type.int)
let () = Record.seal rpt

type l
let rlt : l Record.layout = Record.declare "rl"
let value_l = Record.field rlt "value_list" (Type.list Type.int)
let () = Record.seal rlt

let set_get ctxt =
  let r = Record.make rt in
  Record.set r x 2;
  assert_equal 2 (Record.get r x)

let get_undef ctxt =
  let r = Record.make rt in
  let e = Record.UndefinedField "x" in
  assert_raises e (fun () ->
    Record.get r x
  )

let extend_after_seal ctxt =
  let e = Record.ModifyingSealedStruct "r" in
  assert_raises e (fun () ->
    Record.field rt "y" Type.int
  )

let seal_twice ctxt =
  let e = Record.ModifyingSealedStruct "r" in
  assert_raises e (fun () ->
    Record.seal rt
  )

let make_unsealed ctxt (type r2) =
  let rt2 : r2 Record.layout = Record.declare "r2" in
  let _x2 = Record.field rt2 "x2" Type.int in
  let e = Record.AllocatingUnsealedStruct "r2" in
  assert_raises e (fun () ->
    Record.make rt2
  )

let layout_name ctxt =
  assert_equal "r" (Record.layout_name rt)

let layout_id ctxt =
  let id1 = Record.layout_id rt in
  let id2 = Record.layout_id rt in
  let id3 = Polid.fresh () in
  assert_bool "layout_id is pure" (Polid.equal id1 id2 = Polid.Equal);
  assert_equal ~msg:"layout_id is pure (int)" (Polid.to_int id1) (Polid.to_int id2);
  assert_bool "fresh returns a different id" (Polid.equal id1 id3 = Polid.Different)

let field_name ctxt =
  assert_equal "x" (Record.field_name x)

let field_type ctxt =
  assert_equal "int" (Record.field_type x).Type.name

let record_layout ctxt =
  let r = Record.make rt in
  let l = Record.get_layout r in
  assert_bool "layout is the same"
    (Polid.is_equal
      (Record.layout_id l)
      (Record.layout_id rt)
    )

let of_json ctxt =
  let j = `Assoc [("x", `Int 2)] in
  let r = Record.of_json rt j in
  assert_equal 2 (Record.get r x)

let to_json ctxt =
  let r = Record.make rt in
  Record.set r x 2;
  let expected = `Assoc [("x", `Int 2)] in
  let printer = Yojson.Basic.pretty_to_string in
  assert_equal ~printer expected (Record.to_json r)

let to_json_null ctxt =
  let r = Record.make rt in
  let expected = `Assoc [("x", `Null)] in
  let printer = Yojson.Basic.pretty_to_string in
  assert_equal ~printer expected (Record.to_json r)

let json_product ctxt  =
  let r = Record.make rpt in
  Record.set r value_p (3, 14);
  let json =
    `Assoc
      [ ("value_pair"
        , `Assoc
             [ ("fst", `Int 3)
             ; ("snd", `Int 14)
             ]
        )
      ]
  in
  let printer = Yojson.Basic.pretty_to_string in
  assert_equal ~printer json (Record.to_json r);
  let recovered = Record.of_json rpt json in
  assert_equal (3, 14) (Record.get recovered value_p)

let json_list ctxt =
  let r = Record.make rlt in
  Record.set r value_l [3; 14; 15];
  let json =
    `Assoc
      [ ("value_list"
        , `List
             [ `Int 3
             ; `Int 14
             ; `Int 15
             ]
        )
      ]
  in
  let printer = Yojson.Basic.pretty_to_string in
  assert_equal ~printer json (Record.to_json r);
  let recovered = Record.of_json rlt json in
  assert_equal [3; 14; 15] (Record.get recovered value_l)

let declare0 ctxt =
  let l = Record.declare0 ~name:"r" in
  assert_equal "r" (Record.layout_name l)

let declare1 ctxt =
  let (l, f) = Record.declare1 ~name:"r" ~f1_name:"x" ~f1_type:Type.int in
  assert_equal "r" (Record.layout_name l);
  assert_equal "x" (Record.field_name f)

let declare2 ctxt =
  let (l, f1, f2) =
    Record.declare2 ~name:"r"
      ~f1_name:"f1" ~f1_type:Type.int
      ~f2_name:"f2" ~f2_type:Type.int
  in
  assert_equal "r" (Record.layout_name l);
  assert_equal "f1" (Record.field_name f1);
  assert_equal "f2" (Record.field_name f2)

let declare3 ctxt =
  let (l, f1, f2, f3) =
    Record.declare3 ~name:"r"
      ~f1_name:"f1" ~f1_type:Type.int
      ~f2_name:"f2" ~f2_type:Type.int
      ~f3_name:"f3" ~f3_type:Type.int
  in
  assert_equal "r" (Record.layout_name l);
  assert_equal "f1" (Record.field_name f1);
  assert_equal "f2" (Record.field_name f2);
  assert_equal "f3" (Record.field_name f3)

let declare4 ctxt =
  let (l, f1, f2, f3, f4) =
    Record.declare4 ~name:"r"
      ~f1_name:"f1" ~f1_type:Type.int
      ~f2_name:"f2" ~f2_type:Type.int
      ~f3_name:"f3" ~f3_type:Type.int
      ~f4_name:"f4" ~f4_type:Type.int
  in
  assert_equal "r" (Record.layout_name l);
  assert_equal "f1" (Record.field_name f1);
  assert_equal "f2" (Record.field_name f2);
  assert_equal "f3" (Record.field_name f3);
  assert_equal "f4" (Record.field_name f4)

let layout_type ctxt =
  let rt_typ = Record.layout_type rt in
  let (la, fa1, fa2) =
    Record.declare2 ~name:"pair"
      ~f1_name:"f1" ~f1_type:rt_typ
      ~f2_name:"f2" ~f2_type:rt_typ
  in
  let r = Record.make rt in
  Record.set r x 3;
  let rp = Record.make la in
  Record.set rp fa1 r;
  Record.set rp fa2 r;
  let printer = Yojson.Basic.pretty_to_string in
  let expected =
    `Assoc
      [ ("f1", `Assoc [("x", `Int 3)])
      ; ("f2", `Assoc [("x", `Int 3)])
      ]
  in
  assert_equal ~ctxt ~printer expected (Record.to_json rp)

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
    ; "JSON (product)" >:: json_product
    ; "JSON (list)" >:: json_list
    ; "declare0" >:: declare0
    ; "declare1" >:: declare1
    ; "declare2" >:: declare2
    ; "declare3" >:: declare3
    ; "declare4" >:: declare4
    ; "layout_type" >:: layout_type
    ]

let _ = run_test_tt_main suite
