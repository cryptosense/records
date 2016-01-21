open Ast_helper
open Asttypes
open Location
open Longident
open Parsetree

(** Return an expression representing the "typ" value for a type *)
let typ_exp ctyp =
  match ctyp.ptyp_desc with
  | Ptyp_constr (lid, []) ->
    let typ_name = Ppx_deriving.mangle_lid (`Suffix "typ") lid.txt in
    Exp.ident (mknoloc typ_name)
  | _ -> assert false

(** Given a list of expressions, return an expression representing the list *)
let make_list exprs =
  let cons h t =
    [%expr [%e h] :: [%e t]]
  in
  let nil =
    [%expr []]
  in
  List.fold_right cons exprs nil

let string_lit s =
  Exp.constant (Const_string (s, None))

(** Return a pair:
    the first one is used in [to_json], the second one in [of_json]
*)
let make_json_cases ld =
  let field = ld.pld_name.txt in
  let typ = typ_exp ld.pld_type in
  let lid = mknoloc (Lident field) in
  let str = string_lit field in
  let to_json = [%expr [%e typ].to_json] in
  let of_json = [%expr [%e typ].of_json] in
  let to_json_case =
    let value =
      Exp.field [%expr x] (mknoloc (Lident field))
    in
    [%expr
      ([%e str], [%e to_json] [%e value])
    ]
  in
  let of_json_case =
    (lid, [%expr [%e of_json] (Yojson.Basic.Util.member [%e str] j)])
  in
  (to_json_case, of_json_case)

let make_binding decl =
  let name = Ppx_deriving.mangle_type_decl (`Suffix "typ") decl in
  let pat = Pat.var (mknoloc name) in
  let tk = decl.ptype_kind in
  let expr =
    match tk with
    | Ptype_record lds ->
      let cases = List.map make_json_cases lds in
      let (to_json_cases, of_json_cases) = List.split cases in
      [%expr
        let open Type in
        let to_json x =
          `Assoc [%e make_list to_json_cases]
        in
        let of_json j =
          [%e Exp.record of_json_cases None ]
        in
        let name = [%e string_lit name] in
        Type.make ~name ~to_json ~of_json ()
      ]
    | _ -> assert false
  in
  Vb.mk pat expr

let type_decl_str ~options ~path decls =
  let vbs =
    List.map make_binding decls
  in
  let item =
    Str.value Recursive vbs
  in
  [item]
  
let plugin =
  Ppx_deriving.create
    "typ"
    ~type_decl_str
    ()

let _ = Ppx_deriving.register plugin
