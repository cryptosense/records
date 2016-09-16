#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "records" @@ fun c ->
  Ok [
    Pkg.mllib "src/records.mllib";
    Pkg.test "test/tests";
    Pkg.test ~run:false "test/example";
  ]
