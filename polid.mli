(** Polymorphic identifiers. *)

(** The type of identifiers associated to type ['a]. *)
type 'a t

(** Make a new, fresh identifier.
    This is the only way to obtain a value of type [t]. *)
val fresh: unit -> 'a t

(** Type constraint which is conditioned on identifier equality. *)
type (_, _) equal =
  | Equal: ('a, 'a) equal
  | Different: ('a, 'b) equal

(** Equality predicate. *)
val equal: 'a t -> 'b t -> ('a, 'b) equal

(** Convert an identifier to an integer.
    The integer is guaranteed to be unique for each call to {!fresh}. *)
val to_int: 'a t -> int

(** [equal] projected to a plain [bool]. *)
val is_equal: 'a t -> 'b t -> bool
