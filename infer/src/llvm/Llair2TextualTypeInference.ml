(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module TypeName = Llair2TextualTypeName
module Field = Llair2TextualField
module ModuleState = Llair2TextualState.ModuleState

let rec infer_from_exp inferred_types module_state (exp : Llair.Exp.t) =
  match exp with
  | Ap1 (GetElementPtr (DynamicWvd wvd_name), _, Reg {id; _}) -> (
    match Field.extract_class_and_field_from_wvd wvd_name with
    | Some mangled_class, _ ->
        let ModuleState.{lang; mangled_map; struct_map; _} = module_state in
        let class_name =
          TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
            mangled_class
        in
        let expected_typ = Textual.Typ.mk_ptr (Struct class_name) in
        Hashtbl.set inferred_types ~key:id ~data:expected_typ
    | _ ->
        () )
  | Ap1 (_, _, e) ->
      infer_from_exp inferred_types module_state e
  | Ap2 (_, _, e1, e2) ->
      infer_from_exp inferred_types module_state e1 ;
      infer_from_exp inferred_types module_state e2
  | Ap3 (_, _, e1, e2, e3) ->
      infer_from_exp inferred_types module_state e1 ;
      infer_from_exp inferred_types module_state e2 ;
      infer_from_exp inferred_types module_state e3
  | ApN (_, _, exps) ->
      NS.IArray.iter ~f:(infer_from_exp inferred_types module_state) exps
  | _ ->
      ()


let infer_from_inst inferred_types module_state (inst : Llair.inst) =
  match inst with
  | Load {ptr} ->
      infer_from_exp inferred_types module_state ptr
  | Store {ptr; exp} | AtomicRMW {ptr; exp} ->
      infer_from_exp inferred_types module_state ptr ;
      infer_from_exp inferred_types module_state exp
  | AtomicCmpXchg {ptr; cmp; exp} ->
      infer_from_exp inferred_types module_state ptr ;
      infer_from_exp inferred_types module_state cmp ;
      infer_from_exp inferred_types module_state exp
  | Builtin {args} ->
      NS.IArray.iter ~f:(infer_from_exp inferred_types module_state) args
  | Move {reg_exps} | MovePhi {reg_exps} ->
      NS.IArray.iter ~f:(fun (_, exp) -> infer_from_exp inferred_types module_state exp) reg_exps
  | Alloc _ | Free _ | Nondet _ ->
      ()


let infer_from_term inferred_types module_state (term : Llair.term) =
  match term with
  | Switch {key} ->
      infer_from_exp inferred_types module_state key
  | Call {actuals; callee} -> (
      NS.IArray.iter ~f:(infer_from_exp inferred_types module_state) actuals ;
      match callee with Indirect {ptr} -> infer_from_exp inferred_types module_state ptr | _ -> () )
  | Return {exp= Some exp} ->
      infer_from_exp inferred_types module_state exp
  | Throw {exc} ->
      infer_from_exp inferred_types module_state exc
  | _ ->
      ()


(*
 * Performs a backward type-inference (Use-Def) pre-pass over the LLVM IR CFG.
 *
 * Why is this necessary?
 * Sledge translates instructions sequentially. When encountering a `Load` from an
 * opaque or type-erased base (common in Swift protocol existentials or ObjC layouts),
 * Sledge assigns the destination register a generic `ptr_elt` type.
 * If a subsequent instruction (e.g., a dynamic `Wvd` field access) requires a strongly
 * typed struct pointer, Textual's strict typechecker will fail.
 *
 * Algorithm:
 * 1. Walk the CFG blocks before Textual translation begins.
 * 2. Scan expressions for strongly-typed "Uses" (e.g., passing a register into a
 * `DynamicWvd` node logically proves the register MUST be a pointer to that Wvd class).
 * 3. Map the inferred expected type to the Register's ID.
 * 4. Pass this map into `ProcState`. During actual translation, when the defining
 * `Load` instruction is translated, the frontend will proactively inject this
 * inferred type, neutralizing type-checking errors without emitting `__sil_cast`s.
 *)

let infer_func module_state (func : Llair.func) =
  let inferred_types = Hashtbl.create (module Int) in
  let visited = Hash_set.create (module String) in
  let rec visit_block (b : Llair.block) =
    if not (Hash_set.mem visited b.lbl) then (
      Hash_set.add visited b.lbl ;
      NS.IArray.iter ~f:(infer_from_inst inferred_types module_state) b.cmnd ;
      infer_from_term inferred_types module_state b.term ;
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
  inferred_types
