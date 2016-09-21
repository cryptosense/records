type point
let point : point Record.layout = Record.Unsafe.declare "point"

let x = Record.Unsafe.field point "x" Record.Type.int
let y = Record.Unsafe.field point "y" Record.Type.int
let z = Record.Unsafe.field point "z" Record.Type.int

let () = Record.Unsafe.seal point

let _ =
  let p = Record.Unsafe.make point in
  Record.set p x 3;
  Record.set p y 4;
  Record.set p z 5;
  Yojson.Safe.to_channel stdout @@ Record.to_yojson p
