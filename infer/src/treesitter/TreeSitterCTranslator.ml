(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type cst_node = TreeSitterFFI.cst_node =
  { tag: string
  ; field: string option
  ; srow: int
  ; scol: int
  ; erow: int
  ; ecol: int
  ; text: string
  ; children: cst_node list }

(** Parse tree-sitter XML output into CST nodes. Uses a pull-based approach with xmlm: consume
    signals sequentially. *)
module XmlParser = struct
  let parse_xml (xml_string : string) : cst_node list =
    let input = Xmlm.make_input (`String (0, xml_string)) in
    let get_attr attrs name =
      List.find_map attrs ~f:(fun ((_, n), v) -> if String.equal n name then Some v else None)
    in
    let rec parse_element tag attrs : cst_node =
      let field = get_attr attrs "field" in
      let srow = get_attr attrs "srow" |> Option.value_map ~default:0 ~f:Int.of_string in
      let scol = get_attr attrs "scol" |> Option.value_map ~default:0 ~f:Int.of_string in
      let erow = get_attr attrs "erow" |> Option.value_map ~default:0 ~f:Int.of_string in
      let ecol = get_attr attrs "ecol" |> Option.value_map ~default:0 ~f:Int.of_string in
      let children = ref [] in
      let text_parts = ref [] in
      let continue = ref true in
      while !continue do
        match Xmlm.input input with
        | `El_end ->
            continue := false
        | `Data s ->
            text_parts := s :: !text_parts
        | `El_start ((_ns, child_tag), child_attrs) ->
            let child = parse_element child_tag child_attrs in
            children := child :: !children
        | `Dtd _ ->
            ()
      done ;
      let text = String.concat (List.rev !text_parts) |> String.strip in
      {tag; field; srow; scol; erow; ecol; text; children= List.rev !children}
    in
    let rec find_translation_units (node : cst_node) : cst_node list =
      if String.equal node.tag "translation_unit" then [node]
      else List.concat_map node.children ~f:find_translation_units
    in
    let nodes = ref [] in
    let continue = ref true in
    while !continue do
      if Xmlm.eoi input then continue := false
      else
        match Xmlm.input input with
        | `Dtd _ | `Data _ | `El_end ->
            ()
        | `El_start ((_ns, tag), attrs) ->
            let node = parse_element tag attrs in
            nodes := node :: !nodes
    done ;
    List.rev !nodes |> List.concat_map ~f:find_translation_units
end

(* --- CST helpers --- *)

let find_field children field_name =
  List.find children ~f:(fun (n : cst_node) -> Option.exists n.field ~f:(String.equal field_name))


let find_by_tag children tag =
  List.filter children ~f:(fun (n : cst_node) -> String.equal n.tag tag)


let rec get_text (node : cst_node) : string =
  if not (String.is_empty node.text) then node.text
  else List.map node.children ~f:get_text |> String.concat |> String.strip


let loc (node : cst_node) : Textual.Location.t =
  Textual.Location.known ~line:(node.srow + 1) ~col:(node.scol + 1)


let loc_end (node : cst_node) : Textual.Location.t =
  Textual.Location.known ~line:(node.erow + 1) ~col:(node.ecol + 1)


let get_binary_op (node : cst_node) : string =
  let op = String.strip node.text in
  if String.is_empty op then
    let full = get_text node in
    let left = find_field node.children "left" |> Option.value_map ~default:"" ~f:get_text in
    let right = find_field node.children "right" |> Option.value_map ~default:"" ~f:get_text in
    if (not (String.is_empty left)) && not (String.is_empty right) then
      let idx_l = String.substr_index full ~pattern:left |> Option.value ~default:0 in
      let after_left = idx_l + String.length left in
      let idx_r =
        String.substr_index full ~pattern:right ~pos:after_left
        |> Option.value ~default:(String.length full)
      in
      String.sub full ~pos:after_left ~len:(idx_r - after_left) |> String.strip
    else "+"
  else op


let get_inner_expr (node : cst_node) =
  if String.equal node.tag "parenthesized_expression" then
    let exprs =
      List.filter node.children ~f:(fun c ->
          (not (String.is_empty c.tag))
          &&
          let text = get_text c in
          (not (String.equal text "(")) && not (String.equal text ")") )
    in
    match exprs with [inner] -> inner | _ -> node
  else node


(* --- Type mapping --- *)

let type_to_sil (node : cst_node) : Textual.Typ.t =
  match node.tag with
  | "primitive_type" -> (
      let text = get_text node in
      match text with
      | "int" | "char" | "short" | "long" | "unsigned" | "signed" ->
          Int
      | "float" | "double" ->
          Float
      | "void" ->
          Void
      | _ ->
          Int )
  | "struct_specifier" -> (
    match find_by_tag node.children "type_identifier" with
    | name_node :: _ ->
        Struct (Textual.TypeName.of_string (get_text name_node))
    | [] ->
        Int )
  | "sized_type_specifier" ->
      (* unsigned int, long long, unsigned long, etc. — all map to Int *)
      let text = get_text node in
      if String.is_substring text ~substring:"float" || String.is_substring text ~substring:"double"
      then Float
      else Int
  | "enum_specifier" ->
      Int
  | "type_identifier" ->
      (* typedef names — treat as opaque struct *)
      Struct (Textual.TypeName.of_string (get_text node))
  | _ -> (
      let text = get_text node |> String.strip in
      match text with
      | "int" | "char" | "short" | "long" | "unsigned" | "signed" ->
          Int
      | "float" | "double" ->
          Float
      | "void" ->
          Void
      | _ ->
          Int )


let type_to_sil_or_default node = Option.value_map node ~default:Textual.Typ.Int ~f:type_to_sil

(** Extract struct type name from a type node, if it's a struct type *)
let struct_type_name_of_type_node (node : cst_node) : string option =
  if String.equal node.tag "struct_specifier" then
    match find_by_tag node.children "type_identifier" with
    | name_node :: _ ->
        Some (get_text name_node)
    | [] ->
        None
  else None


(* --- Binop/Unop mapping --- *)

let binop_of_string op =
  match op with
  | "+" ->
      Some (Binop.PlusA (Some IInt))
  | "-" ->
      Some (Binop.MinusA (Some IInt))
  | "*" ->
      Some (Binop.Mult (Some IInt))
  | "/" ->
      Some Binop.DivI
  | "%" ->
      Some Binop.Mod
  | ">" ->
      Some Binop.Gt
  | "<" ->
      Some Binop.Lt
  | ">=" ->
      Some Binop.Ge
  | "<=" ->
      Some Binop.Le
  | "==" ->
      Some Binop.Eq
  | "!=" ->
      Some Binop.Ne
  | "&&" ->
      Some Binop.LAnd
  | "||" ->
      Some Binop.LOr
  | "&" ->
      Some Binop.BAnd
  | "|" ->
      Some Binop.BOr
  | "^" ->
      Some Binop.BXor
  | "<<" ->
      Some Binop.Shiftlt
  | ">>" ->
      Some Binop.Shiftrt
  | _ ->
      None


let comparison_binop_of_string op =
  match op with
  | ">" ->
      Some Binop.Gt
  | "<" ->
      Some Binop.Lt
  | ">=" ->
      Some Binop.Ge
  | "<=" ->
      Some Binop.Le
  | "==" ->
      Some Binop.Eq
  | "!=" ->
      Some Binop.Ne
  | _ ->
      None


let mk_binop_call binop args = Textual.Exp.call_non_virtual (Textual.ProcDecl.of_binop binop) args

let mk_unop_call unop args = Textual.Exp.call_non_virtual (Textual.ProcDecl.of_unop unop) args

(* --- Translation context --- *)

type block_acc =
  { label: Textual.NodeName.t
  ; mutable instrs: Textual.Instr.t list  (** reverse order *)
  ; mutable last: Textual.Terminator.t option
  ; mutable last_loc: Textual.Location.t }

type func_ctx =
  { mutable blocks: block_acc list  (** reverse order *)
  ; mutable block_counter: int
  ; mutable temp_counter: int
  ; mutable locals: (Textual.VarName.t * Textual.Typ.annotated) list
  ; mutable loop_break: Textual.NodeName.t option
  ; mutable loop_continue: Textual.NodeName.t option
  ; var_struct_types: (string, string) Stdlib.Hashtbl.t
        (** Maps variable names to their struct type name for field access resolution *)
  ; var_types: (string, Textual.Typ.t) Stdlib.Hashtbl.t
        (** Maps variable names to their declared type for accurate loads *) }

let mk_func_ctx () =
  { blocks= []
  ; block_counter= 0
  ; temp_counter= 0
  ; locals= []
  ; loop_break= None
  ; loop_continue= None
  ; var_struct_types= Stdlib.Hashtbl.create 16
  ; var_types= Stdlib.Hashtbl.create 16 }


let new_block_label ctx =
  let label = Textual.NodeName.of_string (F.sprintf "bb%d" ctx.block_counter) in
  ctx.block_counter <- ctx.block_counter + 1 ;
  label


let new_block ctx =
  let label = new_block_label ctx in
  let b = {label; instrs= []; last= None; last_loc= Textual.Location.Unknown} in
  ctx.blocks <- b :: ctx.blocks ;
  b


let new_temp ctx =
  let id = Textual.Ident.of_int ctx.temp_counter in
  ctx.temp_counter <- ctx.temp_counter + 1 ;
  id


let add_instr block instr = block.instrs <- instr :: block.instrs

let set_term block term loc =
  block.last <- Some term ;
  block.last_loc <- loc


(** Global registry of struct field names for initializer list handling *)
let struct_fields_registry : (string, string list) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 32

let mk_jump labels =
  Textual.Terminator.Jump
    (List.map labels ~f:(fun label -> {Textual.Terminator.label; ssa_args= []}))


let add_local ctx name typ =
  ctx.locals <- (name, Textual.Typ.mk_without_attributes typ) :: ctx.locals ;
  Stdlib.Hashtbl.replace ctx.var_types (Textual.VarName.to_string name) typ


(** Look up the declared type of a variable *)
let lookup_var_type ctx name = Stdlib.Hashtbl.find_opt ctx.var_types name

(* --- Expression translation --- *)

(** Translate an expression, returning a Textual.Exp.t *)
let rec translate_expr ctx (block : block_acc) (node : cst_node) : Textual.Exp.t =
  match node.tag with
  | "identifier" ->
      let name = get_text node in
      let id = new_temp ctx in
      let l = loc node in
      let typ = Option.value (lookup_var_type ctx name) ~default:Textual.Typ.Int in
      add_instr block
        (Textual.Instr.Load {id; exp= Lvar (Textual.VarName.of_string name); typ= Some typ; loc= l}) ;
      Var id
  | "number_literal" -> (
      let text = get_text node in
      match Int.of_string_opt text with
      | Some n ->
          Const (Int (Z.of_int n))
      | None -> (
        match Float.of_string_opt text with Some f -> Const (Float f) | None -> Const (Int Z.zero) )
      )
  | "null" ->
      Const Null
  | "string_literal" ->
      let text = get_text node in
      (* Strip surrounding quotes *)
      let stripped =
        if String.length text >= 2 && Char.equal text.[0] '"' then
          String.sub text ~pos:1 ~len:(String.length text - 2)
        else text
      in
      Const (Str stripped)
  | "parenthesized_expression" ->
      let inner = get_inner_expr node in
      translate_expr ctx block inner
  | "binary_expression" ->
      translate_binary ctx block node
  | "unary_expression" ->
      translate_unary ctx block node
  | "update_expression" ->
      translate_update ctx block node
  | "assignment_expression" ->
      translate_assignment ctx block node
  | "call_expression" ->
      translate_call ctx block node
  | "pointer_expression" ->
      translate_pointer_expr ctx block node
  | "field_expression" ->
      translate_field_expr ctx block node
  | "subscript_expression" ->
      translate_subscript ctx block node
  | "cast_expression" -> (
      let value = find_field node.children "value" in
      match value with Some v -> translate_expr ctx block v | None -> Const (Int Z.zero) )
  | "sizeof_expression" ->
      Const (Int Z.one)
  | "conditional_expression" ->
      translate_ternary ctx block node
  | "comma_expression" ->
      translate_comma ctx block node
  | _ ->
      Const (Int Z.zero)


and translate_binary ctx block (node : cst_node) =
  let left = find_field node.children "left" in
  let right = find_field node.children "right" in
  let op = get_binary_op node in
  match op with
  | "&&" ->
      (* Short-circuit: a && b  =>  a ? b : 0 *)
      let l =
        Option.value_map left ~default:(Textual.Exp.Const (Int Z.zero))
          ~f:(translate_expr ctx block)
      in
      let r =
        Option.value_map right ~default:(Textual.Exp.Const (Int Z.zero))
          ~f:(translate_expr ctx block)
      in
      let cond = Textual.BoolExp.Exp (mk_binop_call Ne [l; Const (Int Z.zero)]) in
      let id = new_temp ctx in
      add_instr block
        (Textual.Instr.Let
           {id= Some id; exp= If {cond; then_= r; else_= Const (Int Z.zero)}; loc= loc node} ) ;
      Textual.Exp.Var id
  | "||" ->
      (* Short-circuit: a || b  =>  a ? a : b *)
      let l =
        Option.value_map left ~default:(Textual.Exp.Const (Int Z.zero))
          ~f:(translate_expr ctx block)
      in
      let r =
        Option.value_map right ~default:(Textual.Exp.Const (Int Z.zero))
          ~f:(translate_expr ctx block)
      in
      let cond = Textual.BoolExp.Exp (mk_binop_call Ne [l; Const (Int Z.zero)]) in
      let id = new_temp ctx in
      add_instr block
        (Textual.Instr.Let {id= Some id; exp= If {cond; then_= l; else_= r}; loc= loc node}) ;
      Textual.Exp.Var id
  | _ -> (
      let l =
        Option.value_map left ~default:(Textual.Exp.Const (Int Z.zero))
          ~f:(translate_expr ctx block)
      in
      let r =
        Option.value_map right ~default:(Textual.Exp.Const (Int Z.zero))
          ~f:(translate_expr ctx block)
      in
      match binop_of_string op with
      | Some binop ->
          mk_binop_call binop [l; r]
      | None ->
          mk_binop_call (PlusA (Some IInt)) [l; r] )


and translate_unary ctx block (node : cst_node) : Textual.Exp.t =
  let arg = find_field node.children "argument" in
  let op = String.strip node.text in
  let a =
    Option.value_map arg ~default:(Textual.Exp.Const (Int Z.zero)) ~f:(translate_expr ctx block)
  in
  match op with
  | "-" ->
      mk_unop_call Neg [a]
  | "!" ->
      mk_unop_call LNot [a]
  | "~" ->
      mk_unop_call BNot [a]
  | "&" -> (
    match arg with
    | Some arg_node when String.equal arg_node.tag "identifier" ->
        Lvar (Textual.VarName.of_string (get_text arg_node))
    | _ ->
        a )
  | _ ->
      a


and translate_update ctx block (node : cst_node) : Textual.Exp.t =
  let arg = find_field node.children "argument" in
  match arg with
  | None ->
      Textual.Exp.Const (Int Z.zero)
  | Some arg_node ->
      let name = get_text arg_node in
      let var = Textual.VarName.of_string name in
      let l = loc node in
      let id = new_temp ctx in
      add_instr block (Textual.Instr.Load {id; exp= Lvar var; typ= Some Int; loc= l}) ;
      let op_text = String.strip node.text in
      let one = Textual.Exp.Const (Int Z.one) in
      let new_val =
        if String.is_substring op_text ~substring:"++" then
          mk_binop_call (PlusA (Some IInt)) [Var id; one]
        else mk_binop_call (MinusA (Some IInt)) [Var id; one]
      in
      let store_id = new_temp ctx in
      add_instr block (Textual.Instr.Let {id= Some store_id; exp= new_val; loc= l}) ;
      add_instr block
        (Textual.Instr.Store {exp1= Lvar var; typ= Some Int; exp2= Var store_id; loc= l}) ;
      Var id


and translate_assignment ctx block (node : cst_node) =
  let left = find_field node.children "left" in
  let right = find_field node.children "right" in
  let l = loc node in
  let rhs =
    Option.value_map right ~default:(Textual.Exp.Const (Int Z.zero)) ~f:(translate_expr ctx block)
  in
  let op = String.strip node.text in
  let apply_compound_assign op_str old_val rhs =
    match op_str with
    | "+=" ->
        mk_binop_call (PlusA (Some IInt)) [old_val; rhs]
    | "-=" ->
        mk_binop_call (MinusA (Some IInt)) [old_val; rhs]
    | "*=" ->
        mk_binop_call (Mult (Some IInt)) [old_val; rhs]
    | "/=" ->
        mk_binop_call Binop.DivI [old_val; rhs]
    | "%=" ->
        mk_binop_call Binop.Mod [old_val; rhs]
    | _ ->
        rhs
  in
  match left with
  | Some left_node when String.equal left_node.tag "pointer_expression" -> (
      let arg = find_field left_node.children "argument" in
      match arg with
      | Some arg_node ->
          let ptr_name = get_text arg_node in
          let ptr_id = new_temp ctx in
          add_instr block
            (Textual.Instr.Load
               { id= ptr_id
               ; exp= Lvar (Textual.VarName.of_string ptr_name)
               ; typ= Some (Ptr (Int, []))
               ; loc= l } ) ;
          add_instr block (Textual.Instr.Store {exp1= Var ptr_id; typ= Some Int; exp2= rhs; loc= l}) ;
          rhs
      | None ->
          rhs )
  | Some left_node when String.equal left_node.tag "subscript_expression" ->
      let arr = find_field left_node.children "argument" in
      let idx = find_field left_node.children "index" in
      let arr_exp =
        Option.value_map arr ~default:(Textual.Exp.Const (Int Z.zero)) ~f:(translate_expr ctx block)
      in
      let idx_exp =
        Option.value_map idx ~default:(Textual.Exp.Const (Int Z.zero)) ~f:(translate_expr ctx block)
      in
      let ptr_id = new_temp ctx in
      add_instr block
        (Textual.Instr.Let
           {id= Some ptr_id; exp= mk_binop_call (PlusA (Some IInt)) [arr_exp; idx_exp]; loc= l} ) ;
      add_instr block (Textual.Instr.Store {exp1= Var ptr_id; typ= Some Int; exp2= rhs; loc= l}) ;
      rhs
  | Some left_node when String.equal left_node.tag "field_expression" -> (
      (* p->field = value or obj.field = value *)
      let arg = find_field left_node.children "argument" in
      let field = find_field left_node.children "field" in
      match (arg, field) with
      | Some arg_node, Some field_node -> (
          let is_arrow = String.is_substring left_node.text ~substring:">" in
          let struct_type = resolve_struct_type ctx arg_node in
          match struct_type with
          | Some enclosing_class ->
              let fname = get_text field_node in
              let field_name : Textual.qualified_fieldname =
                {enclosing_class; name= Textual.FieldName.of_string fname}
              in
              let obj_exp =
                if is_arrow then
                  if String.equal arg_node.tag "identifier" then (
                    let struct_typ = Textual.Typ.Struct enclosing_class in
                    let ptr_id = new_temp ctx in
                    add_instr block
                      (Textual.Instr.Load
                         { id= ptr_id
                         ; exp= Lvar (Textual.VarName.of_string (get_text arg_node))
                         ; typ= Some (Ptr (struct_typ, []))
                         ; loc= l } ) ;
                    Textual.Exp.Var ptr_id )
                  else translate_expr ctx block arg_node
                else translate_expr ctx block arg_node
              in
              add_instr block
                (Textual.Instr.Store
                   {exp1= Field {exp= obj_exp; field= field_name}; typ= Some Int; exp2= rhs; loc= l}
                ) ;
              rhs
          | None ->
              (* Unknown struct: fall back to pointer store *)
              let ptr = translate_expr ctx block arg_node in
              add_instr block (Textual.Instr.Store {exp1= ptr; typ= Some Int; exp2= rhs; loc= l}) ;
              rhs )
      | _ ->
          rhs )
  | Some left_node when String.equal left_node.tag "identifier" ->
      let name = get_text left_node in
      let var = Textual.VarName.of_string name in
      let val_ =
        if not (String.equal op "=") then (
          let old_id = new_temp ctx in
          add_instr block (Textual.Instr.Load {id= old_id; exp= Lvar var; typ= Some Int; loc= l}) ;
          let compound = apply_compound_assign op (Textual.Exp.Var old_id) rhs in
          let compound_id = new_temp ctx in
          add_instr block (Textual.Instr.Let {id= Some compound_id; exp= compound; loc= l}) ;
          Textual.Exp.Var compound_id )
        else rhs
      in
      add_instr block (Textual.Instr.Store {exp1= Lvar var; typ= Some Int; exp2= val_; loc= l}) ;
      val_
  | _ ->
      rhs


and translate_call ctx block (node : cst_node) =
  let func = find_field node.children "function" in
  let args_node = find_field node.children "arguments" in
  let func_name = Option.value_map func ~default:"unknown" ~f:get_text in
  let args =
    match args_node with
    | Some an ->
        List.filter_map an.children ~f:(fun c ->
            if String.is_empty c.tag then None
            else
              let text = get_text c in
              if String.equal text "(" || String.equal text ")" || String.equal text "," then None
              else Some (translate_expr ctx block c) )
    | None ->
        []
  in
  let proc_name =
    Textual.QualifiedProcName.make_qualified_proc_name Textual.QualifiedProcName.TopLevel
      (Textual.ProcName.of_string func_name)
  in
  let call_exp = Textual.Exp.call_non_virtual proc_name args in
  let id = new_temp ctx in
  add_instr block (Textual.Instr.Let {id= Some id; exp= call_exp; loc= loc node}) ;
  Textual.Exp.Var id


and translate_pointer_expr ctx block (node : cst_node) : Textual.Exp.t =
  let op = String.strip node.text in
  let arg = find_field node.children "argument" in
  if String.equal op "*" then
    match arg with
    | Some arg_node ->
        let ptr = translate_expr ctx block arg_node in
        let id = new_temp ctx in
        add_instr block (Textual.Instr.Load {id; exp= ptr; typ= Some Int; loc= loc node}) ;
        Var id
    | None ->
        Const (Int Z.zero)
  else
    match arg with
    | Some arg_node when String.equal arg_node.tag "identifier" ->
        Lvar (Textual.VarName.of_string (get_text arg_node))
    | Some arg_node ->
        translate_expr ctx block arg_node
    | None ->
        Const (Int Z.zero)


and resolve_struct_type ctx (arg_node : cst_node) : Textual.TypeName.t option =
  if String.equal arg_node.tag "identifier" then
    Stdlib.Hashtbl.find_opt ctx.var_struct_types (get_text arg_node)
    |> Option.map ~f:Textual.TypeName.of_string
  else None


and translate_field_expr ctx block (node : cst_node) : Textual.Exp.t =
  let arg = find_field node.children "argument" in
  let field = find_field node.children "field" in
  match (arg, field) with
  | Some arg_node, Some _field_node -> (
      let is_arrow = String.is_substring node.text ~substring:">" in
      let struct_type = resolve_struct_type ctx arg_node in
      match struct_type with
      | Some enclosing_class ->
          let fname = get_text _field_node in
          let field_name : Textual.qualified_fieldname =
            {enclosing_class; name= Textual.FieldName.of_string fname}
          in
          if is_arrow then (
            let struct_typ = Textual.Typ.Struct enclosing_class in
            let ptr_exp =
              if String.equal arg_node.tag "identifier" then (
                let ptr_id = new_temp ctx in
                add_instr block
                  (Textual.Instr.Load
                     { id= ptr_id
                     ; exp= Lvar (Textual.VarName.of_string (get_text arg_node))
                     ; typ= Some (Ptr (struct_typ, []))
                     ; loc= loc node } ) ;
                Textual.Exp.Var ptr_id )
              else translate_expr ctx block arg_node
            in
            let id = new_temp ctx in
            add_instr block
              (Textual.Instr.Load
                 {id; exp= Field {exp= ptr_exp; field= field_name}; typ= Some Int; loc= loc node} ) ;
            Var id )
          else
            let obj = translate_expr ctx block arg_node in
            let id = new_temp ctx in
            add_instr block
              (Textual.Instr.Load
                 {id; exp= Field {exp= obj; field= field_name}; typ= Some Int; loc= loc node} ) ;
            Var id
      | None ->
          (* Unknown struct type: fall back to loading the pointer and dereferencing *)
          let ptr = translate_expr ctx block arg_node in
          let id = new_temp ctx in
          add_instr block (Textual.Instr.Load {id; exp= ptr; typ= Some Int; loc= loc node}) ;
          Var id )
  | _ ->
      Const (Int Z.zero)


and translate_subscript ctx block (node : cst_node) =
  let arr = find_field node.children "argument" in
  let idx = find_field node.children "index" in
  let arr_exp =
    Option.value_map arr ~default:(Textual.Exp.Const (Int Z.zero)) ~f:(translate_expr ctx block)
  in
  let idx_exp =
    Option.value_map idx ~default:(Textual.Exp.Const (Int Z.zero)) ~f:(translate_expr ctx block)
  in
  let ptr_id = new_temp ctx in
  add_instr block
    (Textual.Instr.Let
       {id= Some ptr_id; exp= mk_binop_call (PlusA (Some IInt)) [arr_exp; idx_exp]; loc= loc node}
    ) ;
  let val_id = new_temp ctx in
  add_instr block (Textual.Instr.Load {id= val_id; exp= Var ptr_id; typ= Some Int; loc= loc node}) ;
  Textual.Exp.Var val_id


and translate_ternary ctx block (node : cst_node) : Textual.Exp.t =
  let cond_node = find_field node.children "condition" in
  let cons_node = find_field node.children "consequence" in
  let alt_node = find_field node.children "alternative" in
  let cond_exp =
    Option.value_map cond_node ~default:(Textual.Exp.Const (Int Z.zero))
      ~f:(translate_expr ctx block)
  in
  let cond = Textual.BoolExp.Exp (mk_binop_call Ne [cond_exp; Const (Int Z.zero)]) in
  let then_ =
    Option.value_map cons_node ~default:(Textual.Exp.Const (Int Z.zero))
      ~f:(translate_expr ctx block)
  in
  let else_ =
    Option.value_map alt_node ~default:(Textual.Exp.Const (Int Z.zero))
      ~f:(translate_expr ctx block)
  in
  (* Materialize into a Let so TextualTransform can lower the If expression *)
  let id = new_temp ctx in
  add_instr block (Textual.Instr.Let {id= Some id; exp= If {cond; then_; else_}; loc= loc node}) ;
  Var id


and translate_comma ctx block (node : cst_node) : Textual.Exp.t =
  let left = find_field node.children "left" in
  let right = find_field node.children "right" in
  (* Evaluate left for side effects, return right *)
  Option.iter left ~f:(fun l -> ignore (translate_expr ctx block l)) ;
  Option.value_map right ~default:(Textual.Exp.Const (Int Z.zero)) ~f:(translate_expr ctx block)


(** Translate an expression for side effects only *)
let translate_expr_for_effect ctx block (node : cst_node) = ignore (translate_expr ctx block node)

(** Translate a condition, returning a Textual.BoolExp.t *)
let rec translate_condition ctx block (node : cst_node) : Textual.BoolExp.t =
  match node.tag with
  | "binary_expression" -> (
      let op = get_binary_op node in
      match op with
      | "&&" ->
          let left = find_field node.children "left" in
          let right = find_field node.children "right" in
          let l =
            match left with
            | Some n ->
                translate_condition ctx block n
            | None ->
                Textual.BoolExp.Exp (Textual.Exp.Const (Int Z.zero))
          in
          let r =
            match right with
            | Some n ->
                translate_condition ctx block n
            | None ->
                Textual.BoolExp.Exp (Textual.Exp.Const (Int Z.zero))
          in
          And (l, r)
      | "||" ->
          let left = find_field node.children "left" in
          let right = find_field node.children "right" in
          let l =
            match left with
            | Some n ->
                translate_condition ctx block n
            | None ->
                Textual.BoolExp.Exp (Textual.Exp.Const (Int Z.zero))
          in
          let r =
            match right with
            | Some n ->
                translate_condition ctx block n
            | None ->
                Textual.BoolExp.Exp (Textual.Exp.Const (Int Z.zero))
          in
          Or (l, r)
      | _ -> (
        match comparison_binop_of_string op with
        | Some binop ->
            let left = find_field node.children "left" in
            let right = find_field node.children "right" in
            let l =
              Option.value_map left ~default:(Textual.Exp.Const (Int Z.zero))
                ~f:(translate_expr ctx block)
            in
            let r =
              Option.value_map right ~default:(Textual.Exp.Const (Int Z.zero))
                ~f:(translate_expr ctx block)
            in
            Exp (mk_binop_call binop [l; r])
        | None ->
            let exp = translate_expr ctx block node in
            Exp (mk_binop_call Ne [exp; Const (Int Z.zero)]) ) )
  | "unary_expression" when String.equal (String.strip node.text) "!" ->
      let arg = find_field node.children "argument" in
      let inner =
        match arg with
        | Some a ->
            translate_condition ctx block a
        | None ->
            Textual.BoolExp.Exp (Textual.Exp.Const (Int Z.zero))
      in
      Not inner
  | _ ->
      let exp = translate_expr ctx block node in
      Exp (mk_binop_call Ne [exp; Const (Int Z.zero)])


let is_statement (node : cst_node) =
  match node.tag with
  | "declaration"
  | "expression_statement"
  | "return_statement"
  | "if_statement"
  | "for_statement"
  | "while_statement"
  | "do_statement"
  | "compound_statement"
  | "switch_statement"
  | "break_statement"
  | "continue_statement" ->
      true
  | _ ->
      false


(* --- Statement translation --- *)

let rec translate_compound ctx (compound : cst_node) current_block exit_label =
  let stmts = List.filter compound.children ~f:is_statement in
  match stmts with
  | [] ->
      set_term current_block (mk_jump [exit_label]) (loc compound)
  | _ ->
      let rec go block = function
        | [] ->
            set_term block (mk_jump [exit_label]) (loc compound)
        | [stmt] ->
            translate_stmt ctx stmt block exit_label
        | stmt :: rest ->
            let next = new_block ctx in
            translate_stmt ctx stmt block next.label ;
            go next rest
      in
      go current_block stmts


and translate_stmt ctx (stmt : cst_node) (current_block : block_acc) next_label =
  match stmt.tag with
  | "declaration" ->
      translate_declaration ctx stmt current_block next_label
  | "expression_statement" ->
      let exprs =
        List.filter stmt.children ~f:(fun c ->
            (not (String.is_empty c.tag))
            &&
            let text = get_text c in
            not (String.equal text ";") )
      in
      List.iter exprs ~f:(translate_expr_for_effect ctx current_block) ;
      set_term current_block (mk_jump [next_label]) (loc stmt)
  | "return_statement" ->
      translate_return ctx stmt current_block
  | "if_statement" ->
      translate_if ctx stmt current_block next_label
  | "for_statement" ->
      translate_for ctx stmt current_block next_label
  | "while_statement" ->
      translate_while ctx stmt current_block next_label
  | "do_statement" ->
      translate_do_while ctx stmt current_block next_label
  | "switch_statement" ->
      translate_switch ctx stmt current_block next_label
  | "break_statement" -> (
    match ctx.loop_break with
    | Some break_label ->
        set_term current_block (mk_jump [break_label]) (loc stmt)
    | None ->
        set_term current_block (mk_jump [next_label]) (loc stmt) )
  | "continue_statement" -> (
    match ctx.loop_continue with
    | Some cont_label ->
        set_term current_block (mk_jump [cont_label]) (loc stmt)
    | None ->
        set_term current_block (mk_jump [next_label]) (loc stmt) )
  | "compound_statement" ->
      translate_compound ctx stmt current_block next_label
  | _ ->
      set_term current_block (mk_jump [next_label]) (loc stmt)


and translate_declaration ctx (decl : cst_node) current_block next_label =
  let type_node = find_field decl.children "type" in
  let sil_type = type_to_sil_or_default type_node in
  let struct_name = Option.bind type_node ~f:struct_type_name_of_type_node in
  let extract_var_info (name_decl : cst_node) =
    let rec unwrap node depth =
      if String.equal node.tag "pointer_declarator" then
        match find_field node.children "declarator" with
        | Some inner ->
            unwrap inner (depth + 1)
        | None ->
            (depth, "unknown")
      else (depth, get_text node)
    in
    let depth, name = unwrap name_decl 0 in
    (depth > 0, name)
  in
  let register_var_type var_name is_ptr =
    (* Record struct type for pointer-to-struct or direct struct variables *)
    Option.iter struct_name ~f:(fun sn ->
        if is_ptr then Stdlib.Hashtbl.replace ctx.var_struct_types var_name sn )
  in
  List.iter decl.children ~f:(fun child ->
      if String.equal child.tag "init_declarator" then (
        let name_decl = find_field child.children "declarator" in
        let is_ptr, var_name =
          match name_decl with Some n -> extract_var_info n | None -> (false, "unknown")
        in
        let actual_type : Textual.Typ.t = if is_ptr then Ptr (sil_type, []) else sil_type in
        let var = Textual.VarName.of_string var_name in
        add_local ctx var actual_type ;
        register_var_type var_name is_ptr ;
        let value = find_field child.children "value" in
        Option.iter value ~f:(fun v ->
            if String.equal v.tag "initializer_list" then
              (* Struct initializer: {val1, val2, ...} → store to each field *)
              let sn =
                Option.bind struct_name ~f:(Stdlib.Hashtbl.find_opt struct_fields_registry)
              in
              match sn with
              | Some field_names ->
                  let init_vals =
                    List.filter v.children ~f:(fun c ->
                        (not (String.is_empty c.tag))
                        &&
                        let t = get_text c in
                        (not (String.equal t "{"))
                        && (not (String.equal t "}"))
                        && not (String.equal t ",") )
                  in
                  let type_name = Textual.TypeName.of_string (Option.value_exn struct_name) in
                  List.iteri init_vals ~f:(fun i val_node ->
                      match List.nth field_names i with
                      | None ->
                          ()
                      | Some fname ->
                          let exp = translate_expr ctx current_block val_node in
                          let field : Textual.qualified_fieldname =
                            {enclosing_class= type_name; name= Textual.FieldName.of_string fname}
                          in
                          let l = loc val_node in
                          add_instr current_block
                            (Textual.Instr.Store
                               {exp1= Field {exp= Lvar var; field}; typ= Some Int; exp2= exp; loc= l}
                            ) )
              | None ->
                  ()
            else
              let exp = translate_expr ctx current_block v in
              let l = loc child in
              add_instr current_block
                (Textual.Instr.Store {exp1= Lvar var; typ= Some actual_type; exp2= exp; loc= l}) ) )
      else if String.equal child.tag "identifier" then (
        let var_name = get_text child in
        add_local ctx (Textual.VarName.of_string var_name) sil_type ;
        register_var_type var_name false )
      else if String.equal child.tag "pointer_declarator" then (
        let is_ptr, var_name = extract_var_info child in
        let actual_type : Textual.Typ.t = if is_ptr then Ptr (sil_type, []) else sil_type in
        add_local ctx (Textual.VarName.of_string var_name) actual_type ;
        register_var_type var_name is_ptr ) ) ;
  set_term current_block (mk_jump [next_label]) (loc decl)


and translate_return ctx (stmt : cst_node) current_block =
  let ret_expr =
    List.find stmt.children ~f:(fun c ->
        (not (String.is_empty c.tag))
        &&
        let text = get_text c in
        (not (String.equal text "return")) && not (String.equal text ";") )
  in
  let l = loc stmt in
  match ret_expr with
  | Some expr ->
      let exp = translate_expr ctx current_block expr in
      (* Materialize into a temp to ensure If expressions in the result
         get lowered by TextualTransform before appearing in Ret *)
      let id = new_temp ctx in
      add_instr current_block (Textual.Instr.Let {id= Some id; exp; loc= l}) ;
      set_term current_block (Ret (Var id)) l
  | None ->
      set_term current_block (Ret (Const Null)) l


and translate_if ctx (stmt : cst_node) current_block join_label =
  let cond_node = find_field stmt.children "condition" in
  let then_node = find_field stmt.children "consequence" in
  let else_clause = find_field stmt.children "alternative" in
  let bexp =
    match cond_node with
    | Some cn ->
        let inner = get_inner_expr cn in
        translate_condition ctx current_block inner
    | None ->
        Textual.BoolExp.Exp (Textual.Exp.Const (Int Z.one))
  in
  let then_body = new_block ctx in
  let else_body = new_block ctx in
  let then_term = mk_jump [then_body.label] in
  let else_term = mk_jump [else_body.label] in
  let l = Option.value_map cond_node ~default:Textual.Location.Unknown ~f:loc in
  set_term current_block (If {bexp; then_= then_term; else_= else_term}) l ;
  ( match then_node with
  | Some tn ->
      translate_stmt ctx tn then_body join_label
  | None ->
      set_term then_body (mk_jump [join_label]) l ) ;
  match else_clause with
  | Some ec -> (
      let else_stmt = List.find ec.children ~f:is_statement in
      match else_stmt with
      | Some es ->
          translate_stmt ctx es else_body join_label
      | None ->
          set_term else_body (mk_jump [join_label]) l )
  | None ->
      set_term else_body (mk_jump [join_label]) l


and translate_for ctx (stmt : cst_node) current_block exit_label =
  let init = find_field stmt.children "initializer" in
  let cond = find_field stmt.children "condition" in
  let update = find_field stmt.children "update" in
  let body = find_field stmt.children "body" in
  let cond_block = new_block ctx in
  ( match init with
  | Some init_node when String.equal init_node.tag "declaration" ->
      translate_declaration ctx init_node current_block cond_block.label
  | Some init_node ->
      translate_expr_for_effect ctx current_block init_node ;
      set_term current_block (mk_jump [cond_block.label]) (loc init_node)
  | None ->
      set_term current_block (mk_jump [cond_block.label]) (loc stmt) ) ;
  let body_block = new_block ctx in
  let update_block = new_block ctx in
  ( match cond with
  | Some cn ->
      let bexp = translate_condition ctx cond_block cn in
      let then_term = mk_jump [body_block.label] in
      let else_term = mk_jump [exit_label] in
      set_term cond_block (If {bexp; then_= then_term; else_= else_term}) (loc cn)
  | None ->
      set_term cond_block (mk_jump [body_block.label]) (loc stmt) ) ;
  (match update with Some un -> translate_expr_for_effect ctx update_block un | None -> ()) ;
  set_term update_block (mk_jump [cond_block.label]) (loc stmt) ;
  let saved_break = ctx.loop_break in
  let saved_continue = ctx.loop_continue in
  ctx.loop_break <- Some exit_label ;
  ctx.loop_continue <- Some update_block.label ;
  ( match body with
  | Some bn ->
      translate_stmt ctx bn body_block update_block.label
  | None ->
      set_term body_block (mk_jump [update_block.label]) (loc stmt) ) ;
  ctx.loop_break <- saved_break ;
  ctx.loop_continue <- saved_continue


and translate_while ctx (stmt : cst_node) current_block exit_label =
  let cond_node = find_field stmt.children "condition" in
  let body_node = find_field stmt.children "body" in
  let cond_block = new_block ctx in
  set_term current_block (mk_jump [cond_block.label]) (loc stmt) ;
  let body_block = new_block ctx in
  ( match cond_node with
  | Some cn ->
      let inner = get_inner_expr cn in
      let bexp = translate_condition ctx cond_block inner in
      set_term cond_block
        (If {bexp; then_= mk_jump [body_block.label]; else_= mk_jump [exit_label]})
        (loc cn)
  | None ->
      set_term cond_block (mk_jump [body_block.label]) (loc stmt) ) ;
  let saved_break = ctx.loop_break in
  let saved_continue = ctx.loop_continue in
  ctx.loop_break <- Some exit_label ;
  ctx.loop_continue <- Some cond_block.label ;
  ( match body_node with
  | Some bn ->
      translate_stmt ctx bn body_block cond_block.label
  | None ->
      set_term body_block (mk_jump [cond_block.label]) (loc stmt) ) ;
  ctx.loop_break <- saved_break ;
  ctx.loop_continue <- saved_continue


and translate_do_while ctx (stmt : cst_node) current_block exit_label =
  let body_node = find_field stmt.children "body" in
  let cond_node = find_field stmt.children "condition" in
  let body_block = new_block ctx in
  set_term current_block (mk_jump [body_block.label]) (loc stmt) ;
  let cond_block = new_block ctx in
  let saved_break = ctx.loop_break in
  let saved_continue = ctx.loop_continue in
  ctx.loop_break <- Some exit_label ;
  ctx.loop_continue <- Some cond_block.label ;
  ( match body_node with
  | Some bn ->
      translate_stmt ctx bn body_block cond_block.label
  | None ->
      set_term body_block (mk_jump [cond_block.label]) (loc stmt) ) ;
  ctx.loop_break <- saved_break ;
  ctx.loop_continue <- saved_continue ;
  match cond_node with
  | Some cn ->
      let inner = get_inner_expr cn in
      let bexp = translate_condition ctx cond_block inner in
      set_term cond_block
        (If {bexp; then_= mk_jump [body_block.label]; else_= mk_jump [exit_label]})
        (loc cn)
  | None ->
      set_term cond_block (mk_jump [body_block.label]) (loc stmt)


and translate_switch ctx (stmt : cst_node) current_block exit_label =
  let cond_node = find_field stmt.children "condition" in
  let body_node = find_field stmt.children "body" in
  let cond_exp =
    match cond_node with
    | Some cn ->
        let inner = get_inner_expr cn in
        translate_expr ctx current_block inner
    | None ->
        Textual.Exp.Const (Int Z.zero)
  in
  let saved_break = ctx.loop_break in
  ctx.loop_break <- Some exit_label ;
  let cases = ref [] in
  let default_block = ref None in
  ( match body_node with
  | Some bn ->
      List.iter bn.children ~f:(fun child ->
          if String.equal child.tag "case_statement" then (
            let value_node = find_field child.children "value" in
            let case_val : Textual.Exp.t =
              match value_node with
              | Some v -> (
                match Int.of_string_opt (get_text v) with
                | Some n ->
                    Const (Int (Z.of_int n))
                | None ->
                    Const (Int Z.zero) )
              | None ->
                  Const (Int Z.zero)
            in
            let case_block = new_block ctx in
            cases := (case_val, case_block) :: !cases ;
            let case_stmts = List.filter child.children ~f:is_statement in
            translate_case_stmts ctx case_stmts case_block exit_label )
          else if String.equal child.tag "default_statement" then (
            let def_block = new_block ctx in
            default_block := Some def_block ;
            let case_stmts = List.filter child.children ~f:is_statement in
            translate_case_stmts ctx case_stmts def_block exit_label ) )
  | None ->
      () ) ;
  ctx.loop_break <- saved_break ;
  let cases = List.rev !cases in
  let fallthrough = match !default_block with Some db -> db.label | None -> exit_label in
  let l = Option.value_map cond_node ~default:Textual.Location.Unknown ~f:loc in
  let rec build_dispatch block = function
    | [] ->
        set_term block (mk_jump [fallthrough]) l
    | (case_val, case_block) :: rest ->
        let bexp = Textual.BoolExp.Exp (mk_binop_call Eq [cond_exp; case_val]) in
        let next = new_block ctx in
        set_term block (If {bexp; then_= mk_jump [case_block.label]; else_= mk_jump [next.label]}) l ;
        build_dispatch next rest
  in
  build_dispatch current_block cases


and translate_case_stmts ctx stmts block exit_label =
  match stmts with
  | [] ->
      if Option.is_none block.last then
        set_term block (mk_jump [exit_label]) Textual.Location.Unknown
  | [s] ->
      translate_stmt ctx s block exit_label
  | s :: rest ->
      let next = new_block ctx in
      translate_stmt ctx s block next.label ;
      translate_case_stmts ctx rest next exit_label


(* --- Function-level translation --- *)

let finalize_blocks ctx : Textual.Node.t list =
  let blocks = List.rev ctx.blocks in
  List.map blocks ~f:(fun b ->
      let instrs = List.rev b.instrs in
      let last = Option.value b.last ~default:(mk_jump []) in
      { Textual.Node.label= b.label
      ; ssa_parameters= []
      ; exn_succs= []
      ; last
      ; instrs
      ; last_loc= b.last_loc
      ; label_loc= Textual.Location.Unknown } )


(** Extract parameter info from a declarator, handling nested pointers (e.g. int** pp). Returns
    (name, type, struct_type_name option). *)
let extract_param_info (ptype_node : cst_node option) (pdecl_node : cst_node option) =
  let base_type = type_to_sil_or_default ptype_node in
  let struct_name = Option.bind ptype_node ~f:struct_type_name_of_type_node in
  let rec unwrap_ptr_decl (node : cst_node) ptr_depth =
    if String.equal node.tag "pointer_declarator" then
      let inner = find_field node.children "declarator" in
      match inner with Some n -> unwrap_ptr_decl n (ptr_depth + 1) | None -> ("unknown", ptr_depth)
    else (get_text node, ptr_depth)
  in
  match pdecl_node with
  | Some pd when String.equal pd.tag "pointer_declarator" ->
      let name, depth = unwrap_ptr_decl pd 0 in
      let typ =
        let rec wrap_ptr t n = if n <= 0 then t else wrap_ptr (Textual.Typ.Ptr (t, [])) (n - 1) in
        wrap_ptr base_type depth
      in
      (name, typ, struct_name)
  | Some pd ->
      (get_text pd, base_type, None)
  | None ->
      ("unknown", base_type, None)


let get_func_sig (func_node : cst_node) =
  let type_node = find_field func_node.children "type" in
  let ret_type = type_to_sil_or_default type_node in
  let decl = find_field func_node.children "declarator" in
  let is_ptr_ret, decl =
    match decl with
    | Some d when String.equal d.tag "pointer_declarator" ->
        (true, find_field d.children "declarator")
    | _ ->
        (false, decl)
  in
  let ret_type : Textual.Typ.t = if is_ptr_ret then Ptr (ret_type, []) else ret_type in
  let name, params =
    match decl with
    | Some d when String.equal d.tag "function_declarator" ->
        let name_node = find_field d.children "declarator" in
        let name = Option.value_map name_node ~default:"unknown" ~f:get_text in
        let params_node = find_field d.children "parameters" in
        let params =
          match params_node with
          | Some pn ->
              List.filter_map pn.children ~f:(fun c ->
                  if String.equal c.tag "parameter_declaration" then
                    let ptype = find_field c.children "type" in
                    let pdecl = find_field c.children "declarator" in
                    let pname, ptyp, _struct_name = extract_param_info ptype pdecl in
                    Some (pname, ptyp)
                  else None )
          | None ->
              []
        in
        (name, params)
    | _ ->
        ("unknown", [])
  in
  (name, params, ret_type)


let mk_procdecl name params ret_type : Textual.ProcDecl.t =
  let qualified_name =
    Textual.QualifiedProcName.make_qualified_proc_name Textual.QualifiedProcName.TopLevel
      (Textual.ProcName.of_string name)
  in
  let formals_types =
    Some (List.map params ~f:(fun (_, t) -> Textual.Typ.mk_without_attributes t))
  in
  let result_type = Textual.Typ.mk_without_attributes ret_type in
  {qualified_name; formals_types; result_type; attributes= []}


let register_param_struct_types ctx (func_node : cst_node) =
  let decl = find_field func_node.children "declarator" in
  let decl =
    match decl with
    | Some d when String.equal d.tag "pointer_declarator" ->
        find_field d.children "declarator"
    | _ ->
        decl
  in
  match decl with
  | Some d when String.equal d.tag "function_declarator" -> (
      let params_node = find_field d.children "parameters" in
      match params_node with
      | Some pn ->
          List.iter pn.children ~f:(fun c ->
              if String.equal c.tag "parameter_declaration" then (
                let ptype = find_field c.children "type" in
                let pdecl = find_field c.children "declarator" in
                let pname, ptyp, struct_name = extract_param_info ptype pdecl in
                Stdlib.Hashtbl.replace ctx.var_types pname ptyp ;
                Option.iter struct_name ~f:(fun sn ->
                    Stdlib.Hashtbl.replace ctx.var_struct_types pname sn ) ) )
      | None ->
          () )
  | _ ->
      ()


let translate_function (func_node : cst_node) : Textual.Module.decl option =
  let name, params, ret_type = get_func_sig func_node in
  let body = find_field func_node.children "body" in
  match body with
  | None ->
      None
  | Some body_node ->
      let ctx = mk_func_ctx () in
      register_param_struct_types ctx func_node ;
      let entry = new_block ctx in
      let exit_block = new_block ctx in
      set_term exit_block (mk_jump []) (loc_end func_node) ;
      translate_compound ctx body_node entry exit_block.label ;
      let procdecl = mk_procdecl name params ret_type in
      let nodes = finalize_blocks ctx in
      let start = (List.hd_exn (List.rev ctx.blocks)).label in
      let params_vars = List.map params ~f:(fun (n, _) -> Textual.VarName.of_string n) in
      let locals = List.rev ctx.locals in
      let exit_loc = loc_end func_node in
      Some
        (Textual.Module.Proc
           {procdecl; fresh_ident= None; nodes; start; params= params_vars; locals; exit_loc} )


let translate_func_decl (decl_node : cst_node) : Textual.Module.decl option =
  let decl = find_field decl_node.children "declarator" in
  let has_func_decl =
    match decl with
    | Some d ->
        String.equal d.tag "function_declarator"
        || String.equal d.tag "pointer_declarator"
           && Option.exists (find_field d.children "declarator") ~f:(fun inner ->
                  String.equal inner.tag "function_declarator" )
    | None ->
        false
  in
  if has_func_decl then
    let name, params, ret_type = get_func_sig decl_node in
    Some (Textual.Module.Procdecl (mk_procdecl name params ret_type))
  else None


let translate_struct (node : cst_node) : Textual.Module.decl option =
  let name_nodes = find_by_tag node.children "type_identifier" in
  let body_nodes = find_by_tag node.children "field_declaration_list" in
  match (name_nodes, body_nodes) with
  | name_node :: _, body_node :: _ ->
      let struct_name = get_text name_node in
      let type_name = Textual.TypeName.of_string struct_name in
      let fields =
        List.filter_map body_node.children ~f:(fun c ->
            if String.equal c.tag "field_declaration" then
              let ft = find_field c.children "type" in
              let fd = find_field c.children "declarator" in
              match (ft, fd) with
              | Some t, Some d ->
                  let base_typ = type_to_sil t in
                  let is_ptr, fname =
                    if String.equal d.tag "pointer_declarator" then
                      let inner = find_field d.children "declarator" in
                      (true, Option.value_map inner ~default:"" ~f:get_text)
                    else if String.equal d.tag "field_identifier" || String.equal d.tag "identifier"
                    then (false, get_text d)
                    else
                      (* Skip array declarators, bitfields, etc. *)
                      (false, "")
                  in
                  if String.is_empty fname then None
                  else
                    let field_typ : Textual.Typ.t =
                      if is_ptr then Ptr (base_typ, []) else base_typ
                    in
                    let field_name = Textual.FieldName.of_string fname in
                    Some
                      { Textual.FieldDecl.qualified_name=
                          {enclosing_class= type_name; name= field_name}
                      ; typ= field_typ
                      ; attributes= [] }
              | _ ->
                  None
            else None )
      in
      Some (Textual.Module.Struct {name= type_name; supers= []; fields; attributes= []})
  | _ ->
      None


(** Collect all function names that are called in a translation unit *)
let rec collect_called_functions (node : cst_node) (acc : (string, unit) Stdlib.Hashtbl.t) =
  if String.equal node.tag "call_expression" then (
    let func = find_field node.children "function" in
    Option.iter func ~f:(fun f ->
        if String.equal f.tag "identifier" then Stdlib.Hashtbl.replace acc (get_text f) () ) ;
    List.iter node.children ~f:(fun c -> collect_called_functions c acc) )
  else List.iter node.children ~f:(fun c -> collect_called_functions c acc)


(* --- Top-level entry point --- *)

let register_struct_fields (node : cst_node) =
  let name_nodes = find_by_tag node.children "type_identifier" in
  let body_nodes = find_by_tag node.children "field_declaration_list" in
  match (name_nodes, body_nodes) with
  | name_node :: _, body_node :: _ ->
      let struct_name = get_text name_node in
      let field_names =
        List.filter_map body_node.children ~f:(fun c ->
            if String.equal c.tag "field_declaration" then
              let fd = find_field c.children "declarator" in
              match fd with
              | Some d when String.equal d.tag "field_identifier" || String.equal d.tag "identifier"
                ->
                  Some (get_text d)
              | Some d when String.equal d.tag "pointer_declarator" ->
                  let inner = find_field d.children "declarator" in
                  Option.map inner ~f:get_text
              | _ ->
                  None
            else None )
      in
      Stdlib.Hashtbl.replace struct_fields_registry struct_name field_names
  | _ ->
      ()


let translate_unit (tu : cst_node) ~file_name : Textual.Module.t =
  let attrs = [Textual.Attr.mk_source_language C] in
  let decls = ref [] in
  Stdlib.Hashtbl.clear struct_fields_registry ;
  (* Collect structs and register their field names *)
  let rec collect_structs (node : cst_node) =
    if String.equal node.tag "struct_specifier" then (
      register_struct_fields node ;
      Option.iter (translate_struct node) ~f:(fun d -> decls := d :: !decls) )
    else List.iter node.children ~f:collect_structs
  in
  collect_structs tu ;
  (* Translate functions, declarations, and globals.
     Each child is translated independently so a failure in one system header
     declaration doesn't prevent user code from being captured. *)
  List.iter tu.children ~f:(fun child ->
      try
        if String.equal child.tag "function_definition" then
          Option.iter (translate_function child) ~f:(fun d -> decls := d :: !decls)
        else if String.equal child.tag "declaration" then
          match translate_func_decl child with
          | Some d ->
              decls := d :: !decls
          | None ->
              (* Not a function declaration — check if it's a global variable *)
              let type_node = find_field child.children "type" in
              let sil_type = type_to_sil_or_default type_node in
              List.iter child.children ~f:(fun c ->
                  if String.equal c.tag "identifier" then
                    let name = Textual.VarName.of_string (get_text c) in
                    decls :=
                      Textual.Module.Global {name; typ= sil_type; attributes= []; init_exp= None}
                      :: !decls
                  else if String.equal c.tag "init_declarator" then
                    let name_decl = find_field c.children "declarator" in
                    let var_name = Option.value_map name_decl ~default:"unknown" ~f:get_text in
                    let name = Textual.VarName.of_string var_name in
                    decls :=
                      Textual.Module.Global {name; typ= sil_type; attributes= []; init_exp= None}
                      :: !decls )
      with _exn -> () ) ;
  (* Auto-declare any called functions that weren't defined or declared.
     Collect already-emitted decl names + scan full tree for definitions. *)
  let called = Stdlib.Hashtbl.create 64 in
  collect_called_functions tu called ;
  let already_declared = Stdlib.Hashtbl.create 64 in
  List.iter !decls ~f:(fun decl ->
      match decl with
      | Textual.Module.Proc pd ->
          let name = Textual.ProcName.to_string pd.procdecl.qualified_name.name in
          Stdlib.Hashtbl.replace already_declared name ()
      | Textual.Module.Procdecl pd ->
          let name = Textual.ProcName.to_string pd.qualified_name.name in
          Stdlib.Hashtbl.replace already_declared name ()
      | _ ->
          () ) ;
  Stdlib.Hashtbl.iter
    (fun name () ->
      if not (Stdlib.Hashtbl.mem already_declared name) then
        let qualified_name =
          Textual.QualifiedProcName.make_qualified_proc_name Textual.QualifiedProcName.TopLevel
            (Textual.ProcName.of_string name)
        in
        decls :=
          Textual.Module.Procdecl
            { qualified_name
            ; formals_types= None
            ; result_type= Textual.Typ.mk_without_attributes (Ptr (Void, []))
            ; attributes= [] }
          :: !decls )
    called ;
  let sourcefile = Textual.SourceFile.create file_name in
  {Textual.Module.attrs; decls= List.rev !decls; sourcefile}


let translate_xml (xml_string : string) ~file_name : Textual.Module.t =
  let tus = XmlParser.parse_xml xml_string in
  match tus with
  | [tu] ->
      translate_unit tu ~file_name
  | _ ->
      (* Multiple TUs: merge decls *)
      let all_decls = List.concat_map tus ~f:(fun tu -> (translate_unit tu ~file_name).decls) in
      let attrs = [Textual.Attr.mk_source_language C] in
      let sourcefile = Textual.SourceFile.create file_name in
      {Textual.Module.attrs; decls= all_decls; sourcefile}


let translate_cst (root : cst_node) ~file_name : Textual.Module.t = translate_unit root ~file_name
