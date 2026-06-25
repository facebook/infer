open Ppxlib

let attr_nobuiltin : (core_type, unit -> unit) Ppxlib.Attribute.t =
  Ppxlib.Attribute.declare "deriving.show.nobuiltin" Core_type
    (Ppxlib.Ast_pattern.(pstr nil))
    Fun.id

let attr_opaque : (core_type, unit -> unit) Ppxlib.Attribute.t =
  Ppxlib.Attribute.declare "deriving.show.opaque" Core_type
    (Ppxlib.Ast_pattern.(pstr nil))
    Fun.id

let attr_printer : (core_type, expression) Ppxlib.Attribute.t =
  Ppxlib.Attribute.declare "deriving.show.printer" Core_type
    (Ppxlib.Ast_pattern.(single_expr_payload __))
    Fun.id

let attr_polyprinter : (core_type, expression) Ppxlib.Attribute.t =
  Ppxlib.Attribute.declare "deriving.show.polyprinter" Core_type
    (Ppxlib.Ast_pattern.(single_expr_payload __))
    Fun.id

let pp_open_box i : expression =
  let loc = !Ast_helper.default_loc in
  [%expr Ppx_show_runtime.Format.pp_open_box fmt
     [%e (Ast_helper.Exp.constant (Ast_helper.Const.int i))]]

let pp_close_box () : expression =
  let loc = !Ast_helper.default_loc in
  [%expr Ppx_show_runtime.Format.pp_close_box fmt ()]

let pp_print_space () : expression =
  let loc = !Ast_helper.default_loc in
  [%expr Ppx_show_runtime.Format.pp_print_space fmt ()]

let pp_print_string_expression e : expression =
  let loc = !Ast_helper.default_loc in
  [%expr Ppx_show_runtime.Format.pp_print_string fmt
     [%e e]]

let pp_print_string s =
  pp_print_string_expression
    (Ast_helper.Exp.constant (Ast_helper.Const.string s))

let pp_list_of_record ~path (fields : (string * expression list) list)
    : expression list =
  List.flatten [
    [pp_open_box 2; pp_print_string "{ "];
    List.flatten begin
      Tools.separate [pp_print_string ";"; pp_print_space ()]
      begin fields |> List.map begin fun (name, value) ->
        let name = Tools.expand_path ~path name in
        pp_open_box 0 :: pp_print_string (name ^ " =") :: pp_print_space ()
          :: value @ [pp_close_box ()]
      end end
    end;
    [pp_print_space (); pp_print_string "}"; pp_close_box ()]]

let pp_list_of_tuple (values : expression list list) : expression list =
  List.flatten [
    [pp_open_box 1; pp_print_string "("];
    List.flatten begin
      Tools.separate [pp_print_string ","; pp_print_space ()]
      begin values |> List.map begin fun value ->
        pp_open_box 0 :: value @ [pp_close_box ()]
      end end
    end;
    [pp_print_string ")"; pp_close_box ()]]

let binders_of_printers printers =
  printers |> List.mapi begin fun i printer ->
    let binder = "x" ^ string_of_int i in
    Tools.pat_var_of_string binder, printer (Tools.ident_of_string binder)
  end |> List.split

type constructor_arguments =
  | No_argument
  | Singleton of (expression -> expression list)
  | Tuple of (expression -> expression list) list

type kind =
  | Construct
  | Variant

let pp_cases_of_cases ?(path = []) kind cases =
  cases |> List.map begin fun (constr, arguments) ->
    let pat, constr =
      match kind with
      | Construct ->
          let loc = !Ast_helper.default_loc in
          Ast_helper.Pat.construct { loc; txt = Lident constr },
          Tools.expand_path ~path constr
      | Variant ->
          Ast_helper.Pat.variant constr, "`" ^ constr in
    let arguments, printers =
      match arguments with
      | No_argument -> None, [pp_print_string constr]
      | Singleton printer ->
          let binder = "x" in
          Some (Tools.pat_var_of_string binder),
          begin
            pp_open_box 1 ::
            pp_print_string (constr ^ " (") ::
            printer (Tools.ident_of_string binder) @
            [pp_print_string ")"; pp_close_box ()]
          end
      | Tuple printers ->
          let binders, printers = binders_of_printers printers in
          Some (Ast_helper.Pat.tuple binders),
          begin
            pp_open_box 0 ::
            pp_print_string constr ::
            pp_print_space () ::
            pp_list_of_tuple printers @
            [pp_close_box ()]
          end in
    Ast_helper.Exp.case (pat arguments) (Tools.seq printers)
  end

let rec pp_list_of_type (ty : core_type) (value : expression)
    : expression list =
  let loc = ty.ptyp_loc in
  match Ppxlib.Attribute.get attr_printer ty with
  | Some printer ->
      [Ast_helper.Exp.apply printer [Nolabel, [%expr fmt]; Nolabel, value]]
  | None ->
    if Ppxlib.Attribute.get attr_opaque ty = None then
      match ty with
      | { ptyp_desc = Ptyp_any; _ } ->
          [pp_print_string "_"]
      | { ptyp_desc = Ptyp_arrow _; _ } ->
          [pp_print_string "<fun>"]
      | { ptyp_desc = Ptyp_tuple types; _ } ->
          let binders, printers =
            binders_of_printers (types |> List.map pp_list_of_type) in
          [Ast_helper.Exp.let_ Nonrecursive [Ast_helper.Vb.mk (Ast_helper.Pat.tuple binders) value]
             (Tools.seq (pp_list_of_tuple printers))]
      | { ptyp_desc = Ptyp_variant (fields, _, _); _ } ->
          let cases =
            fields |> List.map begin fun (field : row_field) ->
              match field.prf_desc with
              | Rtag (label, true, _) ->
                  label.txt, No_argument
              | Rtag (label, false, ty :: _) ->
                  label.txt, Singleton (pp_list_of_type ty)
              | _ ->
                  failwith "Not implemented open tag"
            end in
          [Ast_helper.Exp.match_ value (pp_cases_of_cases Variant cases)]
      | { ptyp_desc = Ptyp_var x; _ } ->
          [Ast_helper.Exp.apply
            (Ast_helper.Exp.ident { loc; txt = Lident (Tools.poly_var x)})
            [Nolabel, [%expr fmt]; Nolabel, value]]
      | { ptyp_desc = Ptyp_constr (constr, arguments); _ } ->
          begin match
            if Ppxlib.Attribute.get attr_nobuiltin ty = None then
              pp_list_of_builtin_type ty value
            else
              []
          with
          | [] ->
              let printer =
                match Ppxlib.Attribute.get attr_polyprinter ty with
                | None ->
                    Ast_helper.Exp.ident (constr |>
                      Tools.map_loc (Tools.mangle_lid (Prefix "pp")))
                | Some printer -> printer in
              [Ast_helper.Exp.apply printer
                 begin
                   begin arguments |> List.map begin
                     fun ty : (arg_label * expression) ->
                       Nolabel, [%expr fun fmt x ->
                         [%e Tools.seq (pp_list_of_type ty [%expr x])]]
                   end end @
                   [Nolabel, [%expr fmt]; Nolabel, value]
                 end]
          | list -> list
          end
      | _ ->
        Location.raise_errorf "ppx_show: Not implemented %a"
          (Pprintast.core_type) ty
    else
      [pp_print_string "<opaque>"]

and pp_list_of_builtin_type (ty : core_type) (value : expression)
    : expression list =
  let loc = ty.ptyp_loc in
  match ty with
  | [%type: unit] -> [pp_print_string "()"]
  | [%type: int] ->
      [[%expr Ppx_show_runtime.Format.pp_print_int fmt [%e value]]]
  | [%type: int32] ->
      [pp_print_string_expression
        [%expr Ppx_show_runtime.Int32.to_string [%e value]];
       pp_print_string "l"]
  | [%type: int64] ->
      [pp_print_string_expression
        [%expr Ppx_show_runtime.Int64.to_string [%e value]];
       pp_print_string "L"]
  | [%type: nativeint] ->
      [pp_print_string_expression
        [%expr Ppx_show_runtime.Nativeint.to_string [%e value]];
       pp_print_string "n"]
  | [%type: float] ->
      [[%expr Ppx_show_runtime.Format.pp_print_float fmt [%e value]]]
  | [%type: bool] ->
      [[%expr Ppx_show_runtime.Format.pp_print_bool fmt [%e value]]]
  | [%type: char] ->
      [[%expr Ppx_show_runtime.Format.pp_print_char fmt [%e value]]]
  | [%type: string] ->
      [pp_print_string "\"";
       pp_print_string_expression
         [%expr Ppx_show_runtime.String.escaped [%e value]];
       pp_print_string "\""]
  | [%type: bytes] ->
      [pp_print_string "\"";
       pp_print_string_expression
         [%expr Ppx_show_runtime.String.escaped
            (Ppx_show_runtime.Bytes.to_string [%e value])];
       pp_print_string "\""]
  | [%type: [%t? ty] ref] ->
      pp_open_box 1 :: pp_print_string "ref (" ::
      pp_list_of_type ty [%expr ! [%e value]] @
      [pp_print_string ")"; pp_close_box ()]
  | [%type: [%t? ty] Lazy.t] ->
      [pp_open_box 1; pp_print_string "lazy (";
       [%expr
        if Ppx_show_runtime.Lazy.is_val [%e value] then
          [%e Tools.seq (pp_list_of_type ty
            [%expr Ppx_show_runtime.Lazy.force [%e value]])]
        else
          Ppx_show_runtime.Format.pp_print_string fmt "<not evaluated>"];
      pp_print_string ")"; pp_close_box ()]
  | [%type: [%t? sub] option] ->
      [Ast_helper.Exp.match_
         (Ast_helper.Exp.constraint_ value [%type: _ option]) begin
        pp_cases_of_cases Construct [
          "None", No_argument;
          "Some", Singleton (fun x -> pp_list_of_type sub x)]
      end]
  | [%type: ([%t? ok], [%t? error]) result] ->
      [Ast_helper.Exp.match_
         (Ast_helper.Exp.constraint_ value [%type: (_, _) result]) begin
        pp_cases_of_cases Construct [
          "Ok", Singleton (fun x -> pp_list_of_type ok x);
          "Error", Singleton (fun x -> pp_list_of_type error x)]
      end]
  | [%type: [%t? ty] list] ->
      [[%expr Ppx_show_runtime.pp_list (fun fmt x ->
        [%e Tools.seq (pp_list_of_type ty [%expr x])]) fmt [%e value]]]
  | _ -> []

let pp_list_of_label_declaration_list ?(path = [])
    (labels : label_declaration list)
    (value : expression) : expression list =
  let fields = labels |> List.map begin fun (label : label_declaration) ->
    label.pld_name.txt,
    pp_list_of_type label.pld_type (Ast_helper.Exp.field value
      (label.pld_name |> Tools.map_loc (fun name : Longident.t ->
        Lident name)))
  end in
  pp_list_of_record ~path fields

let pp_of_variant ~with_path (constrs : constructor_declaration list)
    (value : expression) : expression =
  let cases =
    constrs |> List.map begin fun (constr : constructor_declaration) ->
      constr.pcd_name.txt,
      match constr.pcd_args with
      | Pcstr_tuple [] -> No_argument
      | Pcstr_tuple [ty] -> Singleton (pp_list_of_type ty)
      | Pcstr_tuple list ->
         Tuple (list |> List.map pp_list_of_type)
      | Pcstr_record labels ->
         Singleton (pp_list_of_label_declaration_list labels)
    end in
  let path =
    match with_path with
    | None -> []
    | Some path -> path in
  Ast_helper.Exp.match_ value (pp_cases_of_cases ~path Construct cases)

let pp_of_record ~with_path (labels : label_declaration list)
    (value : expression) : expression =
  let path =
    match with_path with
    | None -> []
    | Some path -> path in
  Tools.seq (pp_list_of_label_declaration_list ~path labels value)

let pp = "pp"

let show = "show"

let fmt_ty (ty : core_type) : core_type =
  let loc = ty.ptyp_loc in
  [%type: Ppx_show_runtime.Format.formatter -> [%t ty] -> unit]

let type_of_type_decl (td : type_declaration) : core_type =
  let loc = td.ptype_loc in
  Ast_helper.with_default_loc loc begin fun () ->
    let ty = Tools.core_type_of_type_decl td in
    Tools.poly_arrow_of_type_decl fmt_ty td (fmt_ty ty)
  end

let pp_of_type_decl ~with_path (td : type_declaration) : value_binding =
  let with_path =
    match with_path with
    | None -> None
    | Some path -> Some (Tools.path_of_type_decl ~path td) in
  let loc = td.ptype_loc in
  Ast_helper.with_default_loc loc begin fun () ->
    let name = Tools.mangle_type_decl (Prefix pp) td in
    let printer : expression =
      match td.ptype_kind with
      | Ptype_abstract ->
          begin match td.ptype_manifest with
          | None ->
              Location.raise_errorf ~loc
                "show cannot be derived for fully abstract types"
          | Some ty ->
              Tools.seq (pp_list_of_type ty [%expr x])
          end
      | Ptype_variant constrs ->
          pp_of_variant ~with_path constrs [%expr x]
      | Ptype_record labels ->
          pp_of_record ~with_path labels [%expr x]
      | Ptype_open ->
          Location.raise_errorf ~loc "show cannot be derived for open types" in
    let printer : expression =
      [%expr fun fmt x ->
        [%e printer]] in
    let printer = Tools.poly_fun_of_type_decl td printer in
    let constraint_ =
      Ast_helper.Typ.poly (td.ptype_params |> List.map begin
        fun (ty, _) : string Location.loc ->
          { loc = ty.ptyp_loc; txt = Tools.var_of_type ty }
      end)
        (type_of_type_decl td) in
    Ast_helper.Vb.mk
      ~attrs:[Ast_helper.Attr.mk
        { loc; txt = "ocaml.warning" } (PStr [%str "-39"])]
      (Ast_helper.Pat.constraint_ (Ast_helper.Pat.var name) constraint_) printer
  end

let show_of_type_decl (td : type_declaration) : value_binding =
  let loc = td.ptype_loc in
  Ast_helper.with_default_loc loc begin fun () ->
    let name = Tools.mangle_type_decl (Prefix show) td in
    let printer_name = Tools.mangle_type_decl (Prefix pp) td in
    let printer : expression =
      Tools.poly_apply_of_type_decl td (Tools.ident_of_str printer_name) in
    let printer : expression =
      [%expr fun x ->
        Ppx_show_runtime.Format.asprintf "@[%a@]" [%e printer] x] in
    let printer = Tools.poly_fun_of_type_decl td printer in
    Ast_helper.Vb.mk (Ast_helper.Pat.var name) printer
  end

let pp_type_of_type_decl (td : type_declaration) : value_description =
  let loc = td.ptype_loc in
  Ast_helper.with_default_loc loc begin fun () ->
    let name = Tools.mangle_type_decl (Prefix pp) td in
    Ast_helper.Val.mk name (type_of_type_decl td)
  end

let show_type_of_type_decl (td : type_declaration) : value_description =
  let loc = td.ptype_loc in
  Ast_helper.with_default_loc loc begin fun () ->
    let name = Tools.mangle_type_decl (Prefix show) td in
    let ty = Tools.core_type_of_type_decl td in
    let ty =
      Tools.poly_arrow_of_type_decl fmt_ty td
        (Ast_helper.Typ.arrow Nolabel ty [%type: string]) in
    Ast_helper.Val.mk name ty
  end

let make_str ~ctxt (rec_flag, tds) (with_path : expression option)
    : structure =
  let with_path =
    match with_path with
    | Some [%expr false] -> None
    | _ ->
        let code_path = Ppxlib.Expansion_context.Deriver.code_path ctxt in
        let main_module_name = Ppxlib.Code_path.main_module_name code_path in
        let submodule_path = Ppxlib.Code_path.submodule_path code_path in
        Some (main_module_name :: submodule_path) in
  let vbs = tds |> List.map (pp_of_type_decl ~with_path) in
  let loc = Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt in
  [Ast_helper.Str.value ~loc rec_flag vbs;
   Ast_helper.Str.value ~loc Nonrecursive (tds |> List.map show_of_type_decl)]

let str_type_decl =
  Ppxlib.Deriving.Generator.V2.make
    Ppxlib.Deriving.Args.(empty +>
      arg "with_path" __)
    make_str

let make_sig ~loc ~path:_ (_rec_flag, tds) : signature =
  let vds = tds |> List.map pp_type_of_type_decl in
  let shows = tds |> List.map show_type_of_type_decl in
  (vds |> List.map (fun vd -> Ast_helper.Sig.value ~loc vd)) @
  (shows |> List.map (fun vd -> Ast_helper.Sig.value ~loc vd))

let sig_type_decl = Ppxlib.Deriving.Generator.make_noarg make_sig

let extension ~loc ~path:_ ty : expression =
  let binder = "x" in
  [%expr fun fmt x ->
    [%e Tools.seq (pp_list_of_type ty (Tools.ident_of_string binder))]]

let deriver =
  Ppxlib.Deriving.add "show" ~str_type_decl ~sig_type_decl ~extension
