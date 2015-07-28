Dynamic records
===============

This library enables you to define and manipulate dynamic records in OCaml.

## Example

Let us define a "point" record with three integer fields: x, y and z.

First, declare a new record layout. It uses a phantom type so that `point` has
type `point layout`.

```ocaml
type point
let point : point Record.layout = Record.declare "point"
```

Second, define the fields. They have the type `(int, point) field`.

```ocaml
let x = Record.field point "x" Type.int
let y = Record.field point "y" Type.int
let z = Record.field point "z" Type.int
```

Third, "seal" this record structure. This prevents it from being further modified.
Structures must be sealed before they can be used.

```ocaml
let () = Record.seal point
```

At this point, you have a working record structure. The next step is to create actual
records.  They have the type `point Record.t` and are created using `Record.make`.
Initially their fields have no value.

```ocaml
let _ =
  let p = Record.make point in
  Record.set p x 3;
  Record.set p y 4;
  Record.set p z 5;
  Record.format Format.std_formatter p
```

The last line outputs:

```json
{"x":3,"y":4,"z":5}
```

## Licensing

This library is available under the 2-clause BSD license.
See `COPYING` for more information.
