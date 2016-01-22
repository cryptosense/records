(**
    How to convert a type to and from JSON.
 *)
type 'a t =
  {
    name: string;
    to_json: 'a -> Yojson.Basic.json;
    of_json: Yojson.Basic.json -> 'a
  }
  [@@deprecated "Please use Record.Type.t instead"]

(** Declare a new type. *)
val make:
  name: string ->
  to_json: ('a -> Yojson.Basic.json) ->
  of_json: (Yojson.Basic.json -> 'a) ->
  unit -> 'a t
  [@@deprecated "Please use Record.Type.make instead"]

(** Declare a new type that marshal/unmarshal to strings. *)
val make_string:
  name: string ->
  to_string: ('a -> string) ->
  of_string: (string -> 'a) ->
  unit -> 'a t
  [@@deprecated "Please use Record.Type.make_string instead"]

(** How to represent exceptions. *)
val exn: exn t
  [@@deprecated "Please use Record.Type.exn instead"]

(** Raised by [exn.of_json] *)
exception UnserializedException of string
  [@@deprecated "Please use Record.Type.UnserializedException instead"]

(** How to represent [unit]. *)
val unit: unit t
  [@@deprecated "Please use Record.Type.unit instead"]

(** How to represent [string]. *)
val string: string t
  [@@deprecated "Please use Record.Type.string instead"]

(** How to represent [int]. *)
val int: int  t
  [@@deprecated "Please use Record.Type.int instead"]

(** Build a representation of a list. *)
val list: 'a t -> 'a list t
  [@@deprecated "Please use Record.Type.list instead"]

(** Build a representation of a couple.
    The labels identify the elements, not their types.
 *)
val product_2: string -> 'a t -> string -> 'b t -> ('a * 'b) t
  [@@deprecated "Please use Record.Type.product_2 instead"]

(** Build a ['b] type which has the same JSON encoding as the ['a] type from
    conversion functions [read] and [write]. *)
val view : name:string -> read:('a -> 'b) -> write:('b -> 'a) -> 'a t -> 'b t
  [@@deprecated "Please use Record.Type.view instead"]
