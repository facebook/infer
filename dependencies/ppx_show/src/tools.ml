open Ppxlib

let rec rev_map_append f list accu =
  match list with
  | [] -> accu
  | hd :: tl -> rev_map_append f tl (List.rev_append (f hd) accu)

let flatten_map f list =
  List.rev (rev_map_append f list [])

let map_loc (f : 'a -> 'b) ({ loc; txt } : 'a loc) : 'b loc =
  { loc; txt = f txt }

type affix =
  | Prefix of string
  | Suffix of string
  | PrefixSuffix of string * string

let mangle ?(fixpoint = "t") affix name =
  if name = fixpoint then
    match affix with
    | Prefix x | Suffix x -> x
    | PrefixSuffix (x, y) -> x ^ "_" ^ y
  else
    match affix with
    | Prefix x -> x ^ "_" ^ name
    | Suffix x -> name ^ "_" ^ x
    | PrefixSuffix (x, y) -> x ^ "_" ^ name ^ "_" ^ y

let mangle_type_decl ?fixpoint affix (td : type_declaration) : string loc =
  map_loc (mangle ?fixpoint affix) td.ptype_name

let mangle_lid ?fixpoint affix (lid : Longident.t) : Longident.t =
  match lid with
  | Lident s -> Lident (mangle ?fixpoint affix s)
  | Ldot (p, s) -> Ldot (p, mangle ?fixpoint affix s)
  | Lapply _ -> invalid_arg "mangle_lid"

let seq ?(loc = !Ast_helper.default_loc) list : expression =
  match List.rev list with
  | [] -> [%expr ()]
  | hd :: tl ->
      List.fold_left begin fun acc item : expression ->
        [%expr [%e item]; [%e acc]]
      end hd tl

let separate separator l =
  match l with
  | [] | [_] -> l
  | hd :: tl ->
      let revl =
        List.fold_left begin fun acc x ->
          x :: separator :: acc
        end [] tl in
      hd :: List.rev revl

let poly_var x =
  "poly_" ^ x

let var_of_type (ty : core_type) =
  match ty.ptyp_desc with
  | Ptyp_var x -> x
  | _ -> invalid_arg "var_of_type"

let poly_fun_of_type_decl (td : type_declaration) (e : expression)
    : expression =
  let loc = !Ast_helper.default_loc in
  List.fold_left begin fun acc (ty, _) : expression ->
    let var = var_of_type ty in
    [%expr fun [%p Ast_helper.Pat.var { loc; txt = poly_var var }] -> [%e acc]]
  end e (List.rev td.ptype_params)

let poly_arrow_of_type_decl (mkvar : core_type -> core_type)
    (td : type_declaration) (ty : core_type)
    : core_type =
  let loc = !Ast_helper.default_loc in
  List.fold_left begin fun acc ((ty : core_type), _) : core_type ->
    [%type: [%t mkvar ty] -> [%t acc]]
  end ty (List.rev td.ptype_params)

let core_type_of_type_decl (td : type_declaration) : core_type =
  Ast_helper.Typ.constr
    (td.ptype_name |> map_loc (fun x : Longident.t -> Lident x))
    (List.map fst td.ptype_params)

let expand_path ~path ident =
  String.concat "." (path @ [ident])

let path_of_type_decl ~path (td : type_declaration) =
  match td.ptype_manifest with
  | Some { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, _); _ } ->
    begin match lid with
    | Lident _ -> []
    | Ldot (lid, _) -> Ocaml_common.Longident.flatten lid
    | Lapply _ -> assert false
    end
  | _ -> path

let pat_var_of_string s =
  let loc = !Ast_helper.default_loc in
  Ast_helper.Pat.var { loc; txt = s }

let ident_of_string s =
  let loc = !Ast_helper.default_loc in
  Ast_helper.Exp.ident { loc; txt = Lident s }

let ident_of_str ({ loc; txt } : string Location.loc) =
  Ast_helper.Exp.ident { loc; txt = Lident txt }

let poly_apply_of_type_decl (td : type_declaration) (e : expression) =
  match td.ptype_params with
  | [] -> e
  | _ ->
      Ast_helper.Exp.apply e begin td.ptype_params |> List.map begin
        fun (ty, _) : (arg_label * expression) ->
          Nolabel, ident_of_string (poly_var (var_of_type ty))
      end end
