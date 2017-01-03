open Result

module Json_safe = struct
  let (>>=) x f =
    match x with
    | Error _ as e -> e
    | Ok y -> f y

  let (>>|) x f =
    x >>= fun y -> Result.Ok (f y)

  let rec mapM f = function
    | [] -> Ok []
    | x::xs ->
      f x >>= fun y ->
      mapM f xs >>= fun ys ->
      Ok (y::ys)

  let rec assoc_option key = function
    | [] -> None
    | (k, v)::_ when k = key -> Some v
    | _::l -> assoc_option key l

  let member key = function
    | `Assoc kvs ->
      begin
        match assoc_option key kvs with
        | Some j -> Ok j
        | None -> Error (Printf.sprintf "Key not found: %s" key)
      end
    | _ -> Error "Not a JSON object"

end

module Type = struct
  type 'a t =
    { name: string;
      to_yojson: ('a -> Yojson.Safe.json);
      of_yojson: (Yojson.Safe.json -> ('a, string) result);
    }

  let name t = t.name
  let of_yojson t = t.of_yojson
  let to_yojson t = t.to_yojson

  let make ~name ~to_yojson ~of_yojson () =
    {
      name;
      to_yojson;
      of_yojson;
    }

  let make_string ~name ~to_string ~of_string () =
    let to_yojson x = `String (to_string x) in
    let of_yojson = function
      | `String s -> of_string s
      | _ -> Error (Printf.sprintf "(while parsing %s) not a string" name)
    in
    make ~name ~to_yojson ~of_yojson ()

  exception UnserializedException of string

  let exn =
    let to_string x = Printexc.to_string x in
    let of_string x = Ok (UnserializedException x)
    in
    make_string ~name:"exn" ~to_string ~of_string ()

  let product_2 na ta nb tb =
    let name = ta.name ^ "_" ^ tb.name in
    let to_yojson (a, b) =
      `Assoc [ na, ta.to_yojson a; nb, tb.to_yojson b]
    in
    let of_yojson json =
      let open Json_safe in
      member na json >>= ta.of_yojson >>= fun a ->
      member nb json >>= tb.of_yojson >>= fun b ->
      Ok (a, b)
    in
    make
      ~name
      ~to_yojson
      ~of_yojson
      ()

  let result ta tb =
    let open Json_safe in
    let to_yojson = function
      | Result.Ok x -> `Assoc ["Ok", ta.to_yojson x]
      | Result.Error x -> `Assoc ["Error", tb.to_yojson x]
    in
    let of_yojson =
      function
      | `Assoc ["Ok", x] ->
        ta.of_yojson x >>| fun y ->
        Result.Ok y
      | `Assoc ["Error", x] ->
        tb.of_yojson x >>| fun y ->
        Result.Error y
      | _ -> Result.Error "result_of_json"
    in
    make
      ~name: (Printf.sprintf "(%s,%s) Result.t" ta.name tb.name )
      ~to_yojson
      ~of_yojson
      ()

  let unit =
    make
      ~name: "unit"
      ~to_yojson: (fun () -> `Null)
      ~of_yojson: (fun _ -> Ok ())
      ()

  let list typ =
    let to_yojson list = `List (List.map typ.to_yojson list) in
    let of_yojson = function
      | `List xs -> Json_safe.mapM typ.of_yojson xs
      | _ -> Error "Not a JSON list"
    in
    make
      ~name: (typ.name ^ "_list")
      ~to_yojson
      ~of_yojson
      ()

  let string =
    let of_yojson = function
      | `String s -> Ok s
      | _ -> Error "Not a JSON string"
    in
    make
      ~name:"string"
      ~to_yojson: (fun s -> `String s)
      ~of_yojson
      ()

  let int =
    let of_yojson = function
      | `Int s -> Ok s
      | _ -> Error "Not a JSON int"
    in
    make
      ~name:"int"
      ~to_yojson: (fun s -> `Int s)
      ~of_yojson
      ()

  let int32 =
    let of_yojson = function
      | `Int x -> Ok (Int32.of_int x)
      | `Intlit x -> Ok (Int32.of_string x)
      | _ -> Error "int32.of_yojson"
    in
    let to_yojson n = `Intlit (Int32.to_string n) in
    make
      ~name:"int32"
      ~to_yojson
      ~of_yojson
      ()

  let int64 =
    let of_yojson = function
      | `Int x -> Ok (Int64.of_int x)
      | `Intlit x -> Ok (Int64.of_string x)
      | _ -> Error "int32.of_yojson"
    in
    let to_yojson n = `Intlit (Int64.to_string n) in
    make
      ~name:"int64"
      ~to_yojson
      ~of_yojson
      ()

  let view ~name ~read ~write typ =
    let to_yojson b =
      typ.to_yojson (write b)
    in
    let of_yojson bj =
      let open Json_safe in
      (typ.of_yojson bj) >>= read
    in
    make ~name ~to_yojson ~of_yojson ()
end

module Polid = struct
  type 'a t = int

  let fresh =
    let counter = ref (-1) in
    fun () ->
      incr counter;
      !counter

  type (_, _) equal =
    | Equal: ('a, 'a) equal
    | Different: ('a, 'b) equal

  let equal (type a) (type b) (a: a t) (b: b t): (a, b) equal =
    if a = b then
      (Obj.magic (Equal: (a, a) equal): (a, b) equal)
    else
      Different

  let to_int x =
    x

  let is_equal (type a) (type b) (a: a t) (b: b t): bool =
    a = b
end

module Field = struct
  type ('a, 's) t =
    {
      polid: 'a Polid.t;
      fname: string;
      ftype: 'a Type.t;
      foffset: int;
    }

  let name field =
    field.fname

  let ftype field =
    field.ftype
end

type 's layout =
  {
    name: string;
    uid: 's Polid.t;
    mutable fields: 's boxed_field list;
    mutable sealed: bool
  }

and _ boxed_field = BoxedField: ('a,'s) Field.t -> 's boxed_field

type 'a t =
  {
    layout: 'a layout;
    content: 'a content;
  }
and 'a content

let equal (type a) (type b) (a: a layout) (b: b layout): (a, b) Polid.equal =
  Polid.equal a.uid b.uid

exception ModifyingSealedStruct of string

exception AllocatingUnsealedStruct of string

(* The [dummy] is a place holder for t fields. We use it
   instead of boxing each value in an option. If the user accesses a
   field that contains this dummy value, we raise an exception (the
   field was not initialized.). This avoid an extra layer of boxing
   w.r.t. to the solution in which each field is an option. *)
let dummy = Obj.repr (ref ())

let field_safe (type s) (type a) (layout: s layout) label (ty : a Type.t):
  (a,s) Field.t =
  if layout.sealed
  then raise (ModifyingSealedStruct layout.name);

  let foffset = List.length layout.fields in
  let field =
      let open Field in
      {
        polid = Polid.fresh ();
        fname = label;
        ftype = ty;
        foffset;
      }
  in
  layout.fields <- BoxedField field :: layout.fields;
  field

module Unsafe = struct
  let declare (type s) name : s layout =
    {
      name;
      uid = Polid.fresh ();
      fields = [];
      sealed = false;
    }

  let seal (type s) (layout: s layout) : unit =
    if layout.sealed
    then raise (ModifyingSealedStruct layout.name);

    layout.fields <- List.rev layout.fields;
    layout.sealed <- true;
    ()

  let field (type s) (type a) (layout: s layout) label (ty : a Type.t):
    (a,s) Field.t =
    field_safe layout label ty

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

  let layout_name layout = layout.name

  let layout_id t = t.uid
end

exception UndefinedField of string

let get_layout t = t.layout

let get record field =
  let open Field in
  let f = Obj.field (Obj.repr record.content) field.foffset in
  if f == dummy
  then raise (UndefinedField (field.fname));
  Obj.obj f

let set record field value =
  let open Field in
  Obj.set_field (Obj.repr record.content) field.foffset
    (Obj.repr value)

(* There are three ways to handle fields that are not set: raise an
   error, map them to `Null, or skip the field. If some fields might
   not be set, we should use the 2nd or the 3rd. Maybe this kind of
   behavior could be set at field creation time? *)
let to_yojson (type s) (s : s t) : Yojson.Safe.json =
  let fields =
    List.map
      (fun (BoxedField f) ->
         let value =
           try
             (Field.ftype f).Type.to_yojson (get s f)
           with UndefinedField _ ->
             `Null
         in
         Field.name f, value)
      s.layout.fields
  in
  `Assoc fields

(* todo: the error handling here is plain wrong. Should do something
   special in the `Null case.  *)
let of_yojson (type s) (s: s layout) (json: Yojson.Safe.json) : (s t, string) result =
  let open Json_safe in
  let field_value (BoxedField f) =
    let open Type in
    let key = Field.name f in
    let typ = Field.ftype f in
    member key json >>= fun m ->
    typ.of_yojson m >>| fun r s ->
    set s f r
  in
  Json_safe.mapM field_value s.fields >>| fun kvs ->
  let s = Unsafe.make s in
  List.iter (fun f -> f s) kvs;
  s

module Util = struct
  let layout_type layout =
    let name = Unsafe.layout_name layout in
    let of_yojson = of_yojson layout in
    Type.make
      ~name
      ~to_yojson
      ~of_yojson
      ()

  let declare0 ~name =
    let layout = Unsafe.declare name in
    Unsafe.seal layout;
    layout

  let declare1 ~name ~f1_name ~f1_type =
    let layout = Unsafe.declare name in
    let f1 = field_safe layout f1_name f1_type in
    Unsafe.seal layout;
    (layout, f1)

  let declare2 ~name ~f1_name ~f1_type ~f2_name ~f2_type =
    let layout = Unsafe.declare name in
    let f1 = field_safe layout f1_name f1_type in
    let f2 = field_safe layout f2_name f2_type in
    Unsafe.seal layout;
    (layout, f1, f2)

  let declare3 ~name ~f1_name ~f1_type ~f2_name ~f2_type
                            ~f3_name ~f3_type =
    let layout = Unsafe.declare name in
    let f1 = field_safe layout f1_name f1_type in
    let f2 = field_safe layout f2_name f2_type in
    let f3 = field_safe layout f3_name f3_type in
    Unsafe.seal layout;
    (layout, f1, f2, f3)

  let declare4 ~name ~f1_name ~f1_type ~f2_name ~f2_type
                            ~f3_name ~f3_type ~f4_name ~f4_type =
    let layout = Unsafe.declare name in
    let f1 = field_safe layout f1_name f1_type in
    let f2 = field_safe layout f2_name f2_type in
    let f3 = field_safe layout f3_name f3_type in
    let f4 = field_safe layout f4_name f4_type in
    Unsafe.seal layout;
    (layout, f1, f2, f3, f4)
end

module Safe =
struct
  module type LAYOUT =
  sig
    type s
    val layout : s layout
    val field : string -> 'a Type.t -> ('a, s) Field.t
    val seal : unit -> unit
    val layout_name : string
    val layout_id : s Polid.t
    val make : unit -> s t
  end

  module Declare(X: sig val name : string end) : LAYOUT =
  struct
    type s
    let layout = Unsafe.declare X.name
    let field n t = field_safe layout n t
    let seal () = Unsafe.seal layout
    let layout_name = layout.name
    let layout_id = layout.uid
    let make () = Unsafe.make layout
  end

  let declare : string -> (module LAYOUT) =
    fun name -> (module (Declare (struct let name = name end)))
end
