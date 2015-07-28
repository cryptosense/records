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
