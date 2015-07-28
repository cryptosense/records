(**
    How to convert a type to and from JSON.
 *)
type 'a t =
  {
    name: string;
    to_json: 'a -> Yojson.Basic.json;
    of_json: Yojson.Basic.json -> 'a
  }

(** Declare a new type. *)
val make:
  name: string ->
  to_json: ('a -> Yojson.Basic.json) ->
  of_json: (Yojson.Basic.json -> 'a) ->
  unit -> 'a t

(** Declare a new type that marshal/unmarshal to strings. *)
val make_string:
  name: string ->
  to_string: ('a -> string) ->
  of_string: (string -> 'a) ->
  unit -> 'a t

(** How to represent exceptions. *)
val exn: exn t

(** Raised by [exn.of_json] *)
exception UnserializedException of string

(** How to represent [unit]. *)
val unit: unit t

(** How to represent [string]. *)
val string: string t

(** How to represent [int]. *)
val int: int  t

(** Build a representation of a list. *)
val list: 'a t -> 'a list t

(** Build a representation of a couple.
    The labels identify the elements, not their types.
 *)
val product_2: string -> 'a t -> string -> 'b t -> ('a * 'b) t
