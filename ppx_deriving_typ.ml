(** Return an expression representing the "typ" value for a type *)
let typ_exp ctyp =
  let open Ast_helper in
  let open Location in
  let open Parsetree in
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
  let open Ast_helper in
  let open Asttypes in
  Exp.constant (Const_string (s, None))

let make_binding decl =
  let open Ast_helper in
  let open Asttypes in
  let open Location in
  let open Longident in
  let open Parsetree in
  let name = Ppx_deriving.mangle_type_decl (`Suffix "typ") decl in
  let pat = Pat.var (mknoloc name) in
  let tk = decl.ptype_kind in
  let expr =
    match tk with
    | Ptype_record lds ->
      let make_to_json_case ld =
        let to_json = [%expr [%e typ_exp ld.pld_type].to_json] in
        let field = ld.pld_name.txt in
        let value =
          Exp.field [%expr x] (mknoloc (Lident field))
        in
        [%expr
          ([%e string_lit field], [%e to_json] [%e value])
        ]
      in
      let make_of_json_case ld =
        let field = ld.pld_name.txt in
        let lid = mknoloc (Lident field) in
        let of_json = [%expr [%e typ_exp ld.pld_type].of_json] in
        (lid, [%expr [%e of_json] (Yojson.Basic.Util.member [%e string_lit field] j)])
      in
      [%expr
        let open Type in
        let to_json x =
          `Assoc [%e make_list (List.map make_to_json_case lds)]
        in
        let of_json j =
          [%e Exp.record (List.map make_of_json_case lds) None ]
        in
        let name = [%e string_lit name] in
        Type.make ~name ~to_json ~of_json ()
      ]

    | _ -> assert false
  in
  Vb.mk pat expr

let type_decl_str ~options ~path decls =
  let open Ast_helper in
  let open Asttypes in
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
