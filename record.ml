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

let format (type s) fmt (s: s t) : unit =
  Format.fprintf fmt "%s" (Yojson.Basic.to_string (to_json s))
