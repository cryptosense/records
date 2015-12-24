type 's layout =
  {
    name: string;
    uid: 's Polid.t;
    mutable fields: 's boxed_field list;
    mutable sealed: bool
  }

and ('a, 's) field =
  {
    polid: 'a Polid.t;
    fname: string;
    ftype: 'a Type.t;
    foffset: int;
  }

and _ boxed_field = BoxedField: ('a,'s) field -> 's boxed_field

let equal (type a) (type b) (a: a layout) (b: b layout): (a, b) Polid.equal =
  Polid.equal a.uid b.uid

let declare (type s) name : s layout =
  {
    name;
    uid = Polid.fresh ();
    fields = [];
    sealed = false;
  }

let layout_name layout = layout.name
let layout_id t = t.uid

exception ModifyingSealedStruct of string

let seal (type s) (layout: s layout) : unit =
  if layout.sealed
  then raise (ModifyingSealedStruct layout.name);

  layout.fields <- List.rev layout.fields;
  layout.sealed <- true;
  ()

let field (type s) (type a) (layout: s layout) label (ty : a Type.t):
  (a,s) field =
  if layout.sealed
  then raise (ModifyingSealedStruct layout.name);

  let foffset = List.length layout.fields in
  let field =
      {
        polid = Polid.fresh ();
        fname = label;
        ftype = ty;
        foffset;
      }
  in
  layout.fields <- BoxedField field :: layout.fields;
  field

type 'a t =
  {
    layout: 'a layout;
    content: 'a;
  }

exception AllocatingUnsealedStruct of string

(* The [dummy] is a place holder for t fields. We use it
   instead of boxing each value in an option. If the user accesses a
   field that contains this dummy value, we raise an exception (the
   field was not initialized.). This avoid an extra layer of boxing
   w.r.t. to the solution in which each field is an option. *)
let dummy = Obj.repr (ref ())

let make (type s) : s layout -> s t =
  fun (layout: s layout) ->
    if not layout.sealed
    then raise (AllocatingUnsealedStruct layout.name);

    let size = List.length layout.fields in
    let obj = Obj.new_block 0 size in
    for i = 0 to size - 1 do
      Obj.set_field obj i dummy;
    done;
    {layout;
     content = Obj.obj obj}

exception UndefinedField of string

let get_layout t = t.layout

let get record field =
  let f = Obj.field (Obj.repr record.content) field.foffset in
  if f == dummy
  then raise (UndefinedField (field.fname));
  Obj.obj f

let set record field value =
  Obj.set_field (Obj.repr record.content) field.foffset
    (Obj.repr value)

let field_name field =
  field.fname

let field_type field =
  field.ftype

(* There are three ways to handle fields that are not set: raise an
   error, map them to `Null, or skip the field. If some fields might
   not be set, we should use the 2nd or the 3rd. Maybe this kind of
   behavior could be set at field creation time? *)
let to_json (type s) (s : s t) : Yojson.Basic.json =
  let fields =
    List.map
      (fun (BoxedField f) ->
         let value =
           try
             (field_type f).Type.to_json (get s f)
           with UndefinedField _ ->
             `Null
         in
         field_name f, value)
      s.layout.fields
  in
  `Assoc fields

(* todo: the error handling here is plain wrong. Should do something
   special in the `Null case.  *)
let of_json (type s) (s: s layout) (json: Yojson.Basic.json) : s t =
  let s = make s in
  List.iter
    (fun (BoxedField f) ->
      let open Type in
      let typ = field_type f in
      let m = Yojson.Basic.Util.member (field_name f) json in
      let r = typ.of_json m in
      set s f r
    )
    s.layout.fields;
  s

let layout_type layout =
  let name = layout_name layout in
  let to_json = to_json in
  let of_json = of_json layout in
  Type.make
    ~name
    ~to_json
    ~of_json
    ()

let format (type s) fmt (s: s t) : unit =
  Format.fprintf fmt "%s" (Yojson.Basic.to_string (to_json s))

let declare0 ~name =
  let layout = declare name in
  seal layout;
  layout

let declare1 ~name ~f1_name ~f1_type =
  let layout = declare name in
  let f1 = field layout f1_name f1_type in
  seal layout;
  (layout, f1)

let declare2 ~name ~f1_name ~f1_type ~f2_name ~f2_type =
  let layout = declare name in
  let f1 = field layout f1_name f1_type in
  let f2 = field layout f2_name f2_type in
  seal layout;
  (layout, f1, f2)

let declare3 ~name ~f1_name ~f1_type ~f2_name ~f2_type
                          ~f3_name ~f3_type =
  let layout = declare name in
  let f1 = field layout f1_name f1_type in
  let f2 = field layout f2_name f2_type in
  let f3 = field layout f3_name f3_type in
  seal layout;
  (layout, f1, f2, f3)

let declare4 ~name ~f1_name ~f1_type ~f2_name ~f2_type
                          ~f3_name ~f3_type ~f4_name ~f4_type =
  let layout = declare name in
  let f1 = field layout f1_name f1_type in
  let f2 = field layout f2_name f2_type in
  let f3 = field layout f3_name f3_type in
  let f4 = field layout f4_name f4_type in
  seal layout;
  (layout, f1, f2, f3, f4)

module Safe =
struct
  module type LAYOUT =
  sig
    type s
    val layout : s layout
    val field : string -> 'a Type.t -> ('a, s) field
    val seal : unit -> unit
    val layout_name : string
    val layout_id : s Polid.t
    val make : unit -> s t
  end

  module Declare(X: sig val name : string end) : LAYOUT =
  struct
    type s
    let layout = declare X.name
    let field n t = field layout n t
    let seal () = seal layout
    let layout_name = layout.name
    let layout_id = layout.uid
    let make () = make layout
  end

  let declare : string -> (module LAYOUT) =
    fun name -> (module (Declare (struct let name = name end)))
end
