type point
let point : point Record.layout = Record.declare "point"

let x = Record.field point "x" Type.int
let y = Record.field point "y" Type.int
let z = Record.field point "z" Type.int

let () = Record.seal point

let _ =
  let p = Record.make point in
  Record.set p x 3;
  Record.set p y 4;
  Record.set p z 5;
  Record.format Format.std_formatter p
