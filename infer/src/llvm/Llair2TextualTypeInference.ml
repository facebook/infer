(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Llair
module TypeName = Llair2TextualTypeName
module Field = Llair2TextualField
module ModuleState = Llair2TextualState.ModuleState

let extract_protocol_typ module_state (typ : Llair.Typ.t) =
  let ModuleState.{lang; mangled_map; struct_map; _} = module_state in
  let get_name (t : Llair.Typ.t) =
    match t with
    | Struct {name; _}
    | Opaque {name}
    | Pointer {elt= Struct {name; _}; _}
    | Pointer {elt= Opaque {name}; _} ->
        Some name
    | _ ->
        None
  in
  match get_name typ with
  | Some name
    when String.is_suffix name ~suffix:"P"
         || String.is_suffix name ~suffix:"_p"
         || String.is_suffix name ~suffix:"_pSg" ->
      let textual_name =
        TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map name
      in
      Some (Textual.Typ.mk_ptr (Struct textual_name))
  | _ ->
      None


let string_of_ptr (exp : Llair.Exp.t) =
  let rec aux e =
    match e with
    | Llair.Exp.Reg {id; _} ->
        Some ("reg_" ^ Int.to_string id)
    | Llair.Exp.Global {name; _} | Llair.Exp.FuncName {name; _} ->
        Some ("global_" ^ name)
    | Llair.Exp.Ap1 (GetElementPtr (Static offset), _, base) | Llair.Exp.Ap1 (Select offset, _, base)
      ->
        Option.map (aux base) ~f:(fun b -> b ^ "_idx_" ^ Int.to_string offset)
    | Llair.Exp.Ap1 (GetElementPtr (DynamicWvd name), _, base) ->
        Option.map (aux base) ~f:(fun b -> b ^ "_wvd_" ^ name)
    | Llair.Exp.Ap1 ((Convert _ | Signed _ | Unsigned _), _, base) ->
        aux base
    | _ ->
        None
  in
  aux exp


(* Unified recursive scanner for both WVD and Protocol types *)
let rec infer_from_exp wvd_types protocol_types module_state (exp : Llair.Exp.t) =
  let recurse = infer_from_exp wvd_types protocol_types module_state in
  match exp with
  | Ap1 (GetElementPtr (DynamicWvd wvd_name), _, Reg {id; _}) -> (
    match Field.extract_class_and_field_from_wvd wvd_name with
    | Some mangled_class, _ ->
        let ModuleState.{lang; mangled_map; struct_map; _} = module_state in
        let class_name =
          TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
            mangled_class
        in
        Hashtbl.set wvd_types ~key:id ~data:(Textual.Typ.mk_ptr (Struct class_name))
    | _ ->
        () )
  | Ap1 ((Select _ | GetElementPtr _), typ, Reg {id; _}) ->
      Option.iter (extract_protocol_typ module_state typ) ~f:(fun textual_typ ->
          Hashtbl.set protocol_types ~key:id ~data:textual_typ )
  | Ap1 (_, _, e) ->
      recurse e
  | Ap2 (_, _, e1, e2) ->
      recurse e1 ;
      recurse e2
  | Ap3 (_, _, e1, e2, e3) ->
      recurse e1 ;
      recurse e2 ;
      recurse e3
  | ApN (_, _, exps) ->
      NS.IArray.iter ~f:recurse exps
  | _ ->
      ()


let infer_from_inst wvd_types protocol_types load_map module_state (inst : Llair.inst) =
  let infer = infer_from_exp wvd_types protocol_types module_state in
  match inst with
  | Load {reg; ptr} ->
      Option.iter (string_of_ptr ptr) ~f:(fun ptr_key ->
          Hashtbl.set load_map ~key:(Reg.id reg) ~data:ptr_key ) ;
      infer ptr
  | Store {ptr; exp} | AtomicRMW {ptr; exp} ->
      infer ptr ;
      infer exp
  | AtomicCmpXchg {ptr; cmp; exp} ->
      infer ptr ;
      infer cmp ;
      infer exp
  | Builtin {args} ->
      NS.IArray.iter ~f:infer args
  | Move {reg_exps} | MovePhi {reg_exps} ->
      NS.IArray.iter reg_exps ~f:(fun (reg, exp) ->
          Option.iter (string_of_ptr exp) ~f:(fun ptr_key ->
              Hashtbl.set load_map ~key:(Reg.id reg) ~data:ptr_key ) ;
          infer exp )
  | Alloc _ | Free _ | Nondet _ ->
      ()


let infer_from_term wvd_types protocol_types module_state (term : Llair.term) =
  let infer = infer_from_exp wvd_types protocol_types module_state in
  match term with
  | Switch {key} ->
      infer key
  | Call {actuals; callee} -> (
      NS.IArray.iter ~f:infer actuals ;
      match callee with Indirect {ptr} -> infer ptr | _ -> () )
  | Return {exp= Some exp} ->
      infer exp
  | Throw {exc} ->
      infer exc
  | _ ->
      ()


(*
 * Performs a type-inference and alias-resolution pre-pass over the LLVM IR.
 *
 * Why is this necessary?
 * LLVM aggressively erases types, reducing Swift existentials and ObjC layouts to
 * generic `ptr_elt` pointers or opaque `{i64, i64}` tuples. Because the frontend
 * translates sequentially, this type erasure causes two major issues:
 * 1. Textual Typechecking: Dynamic `Wvd` accesses fail if the base pointer is untyped.
 * 2. Pulse Memory Graphs: If a payload is written as a generic tuple but later read as
 * a strongly-typed Protocol, Pulse treats them as distinct memory locations, dropping
 * dynamic types.
 *
 * Algorithm:
 * 1. Walk the CFG to find strongly-typed Uses (e.g., `DynamicWvd` arguments) and
 * Protocol types hidden inside `Select`/`GetElementPtr` instructions.
 * 2. Build a `load_map` tracking the structural memory path of each load (e.g., "reg_0_idx_1").
 * 3. Alias Resolution: Flow Protocol types across registers that share the same memory path,
 * ensuring read and write operations are perfectly unified under the same Textual type.
 * 4. Pass the combined map to `ProcState`. During translation, the frontend proactively
 * injects these types to satisfy the typechecker and bridge Pulse's memory aliases.
 *)
let infer_func module_state (func : Llair.func) =
  let wvd_types = Hashtbl.create (module Int) in
  let protocol_types = Hashtbl.create (module Int) in
  let load_reg_map = Hashtbl.create (module Int) in
  let visited = Hash_set.create (module String) in
  let rec visit_block (b : Llair.block) =
    if not (Hash_set.mem visited b.lbl) then (
      Hash_set.add visited b.lbl ;
      NS.IArray.iter ~f:(infer_from_inst wvd_types protocol_types load_reg_map module_state) b.cmnd ;
      infer_from_term wvd_types protocol_types module_state b.term ;
      match b.term with
      | Switch {tbl; els} ->
          NS.IArray.iter ~f:(fun (_, (jump : Llair.jump)) -> visit_block jump.dst) tbl ;
          visit_block els.dst
      | Call {return} ->
          visit_block return.dst
      | _ ->
          () )
  in
  visit_block func.entry ;
  (* --- SAFE PROTOCOL ALIAS RESOLUTION --- *)
  let rich_ptrs = Hashtbl.create (module String) in
  (* 1. Seed the alias graph with registers that already have known Protocol types *)
  Hashtbl.iteri protocol_types ~f:(fun ~key:id ~data:typ ->
      Hashtbl.set rich_ptrs ~key:("reg_" ^ Int.to_string id) ~data:typ ) ;
  (* 2. Flow Backwards: From Register to Memory Source
     If a register has a protocol type (e.g., from a Read), and we know it was
     loaded from a specific memory path, assign that type to the memory path. *)
  Hashtbl.iteri load_reg_map ~f:(fun ~key:reg_id ~data:ptr_key ->
      match Hashtbl.find protocol_types reg_id with
      | Some typ ->
          Hashtbl.set rich_ptrs ~key:ptr_key ~data:typ
      | None ->
          () ) ;
  (* 3. Flow Forwards: From Memory Source to Register
     If a memory path now has a protocol type, propagate that type forward
     to ALL registers loaded from that exact same memory path (e.g., the Write). *)
  Hashtbl.iteri load_reg_map ~f:(fun ~key:reg_id ~data:ptr_key ->
      match Hashtbl.find rich_ptrs ptr_key with
      | Some typ ->
          Hashtbl.set protocol_types ~key:reg_id ~data:typ
      | None ->
          () ) ;
  (* --- FINAL MERGE --- *)
  (* 4. Combine the protocol types into the main WVD types map.
     We use `if not` to ensure we NEVER overwrite an existing WVD type. *)
  Hashtbl.iteri protocol_types ~f:(fun ~key:id ~data:typ ->
      if not (Hashtbl.mem wvd_types id) then Hashtbl.set wvd_types ~key:id ~data:typ ) ;
  wvd_types
