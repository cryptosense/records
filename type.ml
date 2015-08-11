type 'a t =
  {
    name: string;
    to_json: 'a -> Yojson.Basic.json;
    of_json: Yojson.Basic.json -> 'a
  }

let make ~name ~to_json ~of_json () =
  {
    name;
    to_json;
    of_json;
  }

let make_string ~name ~to_string ~of_string () =
  let to_json x = `String (to_string x) in
  let of_json x = of_string (Yojson.Basic.Util.to_string x) in
  {
    name;
    to_json;
    of_json;
  }


exception UnserializedException of string

let exn =
  let to_string x = Printexc.to_string x in
  let of_string x =  (UnserializedException x)
  in
  make_string ~name:"exn" ~to_string ~of_string ()

let product_2: string -> 'a t -> string -> 'b t -> ('a * 'b) t =
  fun na ta nb tb ->
    let to_json (a, b) =
      `Assoc [ na, ta.to_json a; nb, tb.to_json b]
    in
    let of_json json =
      let a = ta.of_json (Yojson.Basic.Util.member na json) in
      let b = tb.of_json (Yojson.Basic.Util.member nb json) in
      a, b
    in
    make
      ~name: (ta.name ^ "_" ^ tb.name)
      ~to_json
      ~of_json
      ()

let unit =
  make
    ~name: "unit"
    ~to_json: (fun () -> `Null)
    ~of_json: (fun _ -> ())
    ()

let list typ =
  let to_json list = `List (List.map typ.to_json list) in
  let of_json json = List.map typ.of_json (Yojson.Basic.Util.to_list json) in
  make
    ~name: (typ.name ^ "_list")
    ~to_json
    ~of_json
    ()

let string =
  make
    ~name:"string"
    ~to_json: (fun s -> `String s)
    ~of_json: (Yojson.Basic.Util.to_string)
    ()

let int =
  make
    ~name:"int"
    ~to_json: (fun s -> `Int s)
    ~of_json: (Yojson.Basic.Util.to_int)
    ()
