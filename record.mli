(** {2} Layouts *)

(** The representation of record types. ['s] is usually a phantom type. *)
type 's layout

(** A field of type ['a] within a ['s layout]. *)
type ('a,'s) field

(** Create a new layout with the given name. *)
val declare : string -> 's layout

(** Add a field to a layout. This modifies the layout and returns the field. *)
val field: 's layout -> string -> 'a Type.t -> ('a,'s) field

(** Get the name of the field (as passed to [field]). *)
val field_name : ('a, 's) field -> string

(** Get the type of the field (as passed to [field]). *)
val field_type : ('a, 's) field -> 'a Type.t

(** Make the layout unmodifiable. It is necessary before constructing values. *)
val seal : 's layout -> unit

(** Raised by [field] or [seal] if layout has already been sealed. *)
exception ModifyingSealedStruct of string

(** Get the name that was given to a layout. *)
val layout_name : 's layout -> string

(** Get the unique identifier given to a layout. *)
val layout_id: 's layout -> 's Polid.t

(** {2} Records *)

(** The representation of record values. *)
type 's t =
  {
    layout: 's layout;
    content: 's;
  }

(** Allocate a record of a given layout, with all fields initially unset. *)
val make: 's layout -> 's t

(** Get the layout of a record. *)
val get_layout : 'a t -> 'a layout

(** Raised by [make] when the corresponding layout has not been sealed. *)
exception AllocatingUnsealedStruct of string

(** Get the value of a field. *)
val get: 's t -> ('a,'s) field -> 'a

(** Set the value of a field. *)
val set: 's t -> ('a,'s) field -> 'a -> unit

(** Raised by [get] if the field was not set. *)
exception UndefinedField of string

(** {2} Miscellaneous *)

(** Convert a record to JSON. *)
val to_json: 'a t -> Yojson.Basic.json

(** Convert a JSON value into a given schema. *)
val of_json: 'a layout -> Yojson.Basic.json -> 'a t

(** Equality predicate. *)
val equal: 'a layout -> 'b layout -> ('a, 'b) Polid.equal

(** Print the JSON representation of a record to a formatter. *)
val format: Format.formatter -> 'a t -> unit
