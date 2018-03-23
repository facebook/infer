(* Copyright (c) 2018 - present Facebook, Inc. All rights reserved.

   This source code is licensed under the BSD style license found in the
   LICENSE file in the root directory of this source tree. An additional
   grant of patent rights can be found in the PATENTS file in the same
   directory. *)

(** Translate LLVM to LLAIR *)

let fmt_lltype ff t = Format.pp_print_string ff (Llvm.string_of_lltype t)

let fmt_llvalue ff t = Format.pp_print_string ff (Llvm.string_of_llvalue t)

let fmt_llblock ff t =
  Format.pp_print_string ff (Llvm.string_of_llvalue (Llvm.value_of_block t))


(* gather debug locations *)
let (scan_locs: Llvm.llmodule -> unit), (find_loc: Llvm.llvalue -> Loc.t) =
  let loc_of_global g =
    Loc.mk
      ?dir:(Llvm.get_global_debug_loc_directory g)
      ?file:(Llvm.get_global_debug_loc_filename g)
      ~line:(Llvm.get_global_debug_loc_line g)
      ?col:None
  in
  let loc_of_function f =
    Loc.mk
      ?dir:(Llvm.get_function_debug_loc_directory f)
      ?file:(Llvm.get_function_debug_loc_filename f)
      ~line:(Llvm.get_function_debug_loc_line f)
      ?col:None
  in
  let loc_of_instr i =
    Loc.mk
      ?dir:(Llvm.get_debug_loc_directory i)
      ?file:(Llvm.get_debug_loc_filename i)
      ~line:(Llvm.get_debug_loc_line i)
      ~col:(Llvm.get_debug_loc_column i)
  in
  let loc_tbl = Hashtbl.Poly.create () in
  let add ~key ~data =
    Hashtbl.update loc_tbl key ~f:(fun prev ->
        Option.iter prev ~f:(fun loc ->
            if Option.is_none
                 (List.find_a_dup ~compare:Loc.compare [loc; data; Loc.none])
            then
              warn "ignoring location %a conflicting with %a for %a" Loc.fmt
                loc Loc.fmt data fmt_llvalue key ) ;
        data )
  in
  let scan_locs m =
    let scan_instr i =
      let loc = loc_of_instr i in
      add ~key:i ~data:loc ;
      match Llvm.instr_opcode i with
      | Call -> (
        match Llvm.(value_name (operand i (num_arg_operands i))) with
        | "llvm.dbg.declare" -> (
          match Llvm.(get_mdnode_operands (operand i 0)) with
          | [|var|] when not (String.is_empty (Llvm.value_name var)) ->
              add ~key:var ~data:loc
          | _ ->
              warn "could not find variable for debug info %a at %a"
                fmt_llvalue (Llvm.operand i 0) Loc.fmt loc )
        | _ -> () )
      | _ -> ()
    in
    let scan_block b = Llvm.iter_instrs scan_instr b in
    let scan_function f =
      add ~key:f ~data:(loc_of_function f) ;
      Llvm.iter_blocks scan_block f
    in
    let scan_global g = add ~key:g ~data:(loc_of_global g) in
    Llvm.iter_globals scan_global m ;
    Llvm.iter_functions scan_function m
  in
  let find_loc v =
    Option.value (Hashtbl.find loc_tbl v) ~default:Loc.none
  in
  (scan_locs, find_loc)


let (scan_names: Llvm.llmodule -> unit), (find_name: Llvm.llvalue -> string) =
  let name_tbl = Hashtbl.Poly.create () in
  let scan_name =
    let scope_tbl = Hashtbl.Poly.create () in
    fun llv ->
      let next, void_tbl =
        let scope =
          match Llvm.classify_value llv with
          | Argument -> `Fun (Llvm.param_parent llv)
          | BasicBlock -> `Fun (Llvm.block_parent (Llvm.block_of_value llv))
          | Function | GlobalAlias | GlobalIFunc | GlobalVariable ->
              `Mod (Llvm.global_parent llv)
          | Instruction _ ->
              `Fun (Llvm.block_parent (Llvm.instr_parent llv))
          | _ -> fail "scan_name: %a" fmt_llvalue llv ()
        in
        Hashtbl.find_or_add scope_tbl scope ~default:(fun () ->
            (ref 0, Hashtbl.Poly.create ()) )
      in
      let name =
        match Llvm.classify_type (Llvm.type_of llv) with
        | Void -> (
            let fname =
              match Llvm.classify_value llv with
              | Instruction (Call | Invoke) -> (
                match
                  Llvm.value_name
                    (Llvm.operand llv (Llvm.num_operands llv - 1))
                with
                | "" -> Int.to_string (!next - 1)
                | s -> s )
              | _ -> "void"
            in
            match Hashtbl.find void_tbl fname with
            | None ->
                Hashtbl.set void_tbl ~key:fname ~data:1 ;
                Format.sprintf "%s.void" fname
            | Some count ->
                Hashtbl.set void_tbl ~key:fname ~data:(count + 1) ;
                Format.sprintf "%s.void.%i" fname count )
        | _ ->
          match Llvm.value_name llv with
          | "" ->
              (* anonymous values take the next SSA name *)
              let name = !next in
              next := name + 1 ;
              Int.to_string name
          | name ->
            match Int.of_string name with
            | _ ->
                (* escape to avoid clash with names of anonymous values *)
                Format.sprintf "\"%s\"" name
            | exception _ -> name
      in
      Hashtbl.add_exn name_tbl ~key:llv ~data:name
  in
  let scan_names m =
    let scan_global g = scan_name g in
    let scan_instr i = scan_name i in
    let scan_block b =
      scan_name (Llvm.value_of_block b) ;
      Llvm.iter_instrs scan_instr b
    in
    let scan_function f =
      scan_name f ;
      Llvm.iter_params scan_name f ;
      Llvm.iter_blocks scan_block f
    in
    Llvm.iter_globals scan_global m ;
    Llvm.iter_functions scan_function m
  in
  let find_name v = Hashtbl.find_exn name_tbl v in
  (scan_names, find_name)


let label_of_block : Llvm.llbasicblock -> string =
 fun blk -> find_name (Llvm.value_of_block blk)


let anon_struct_name : (Llvm.lltype, string) Hashtbl.t =
  Hashtbl.Poly.create ()


let struct_name : Llvm.lltype -> string =
 fun llt ->
  match Llvm.struct_name llt with
  | Some name -> name
  | None ->
      Hashtbl.find_or_add anon_struct_name llt ~default:(fun () ->
          Int.to_string (Hashtbl.length anon_struct_name) )


let memo_type : (Llvm.lltype, Typ.t) Hashtbl.t = Hashtbl.Poly.create ()

let rec xlate_type : Llvm.lltype -> Typ.t =
 fun llt ->
  let xlate_type_ llt =
    match Llvm.classify_type llt with
    | Half -> Typ.mkFloat ~bits:16 ~enc:`IEEE
    | Float -> Typ.mkFloat ~bits:32 ~enc:`IEEE
    | Double -> Typ.mkFloat ~bits:64 ~enc:`IEEE
    | X86fp80 -> Typ.mkFloat ~bits:80 ~enc:`Extended
    | Fp128 -> Typ.mkFloat ~bits:128 ~enc:`IEEE
    | Ppc_fp128 -> Typ.mkFloat ~bits:128 ~enc:`Pair
    | Integer -> Typ.mkInteger ~bits:(Llvm.integer_bitwidth llt)
    | X86_mmx -> Typ.mkInteger ~bits:64
    | Function ->
        let return = xlate_type_opt (Llvm.return_type llt) in
        let llargs = Llvm.param_types llt in
        let len = Array.length llargs in
        let args = Vector.init len ~f:(fun i -> xlate_type llargs.(i)) in
        Typ.mkFunction ~return ~args
    | Pointer ->
        let elt = xlate_type (Llvm.element_type llt) in
        Typ.mkPointer ~elt
    | Vector ->
        let elt = xlate_type (Llvm.element_type llt) in
        let len = Llvm.vector_size llt in
        Typ.mkArray ~elt ~len
    | Array ->
        let elt = xlate_type (Llvm.element_type llt) in
        let len = Llvm.array_length llt in
        Typ.mkArray ~elt ~len
    | Struct ->
        let llelts = Llvm.struct_element_types llt in
        let len = Array.length llelts in
        let packed = Llvm.is_packed llt in
        if Llvm.is_literal llt then
          let elts = Vector.init len ~f:(fun i -> xlate_type llelts.(i)) in
          Typ.mkTuple ~packed elts
        else
          let name = struct_name llt in
          if Llvm.is_opaque llt then Typ.mkOpaque ~name
          else
            let elts =
              Vector.init len ~f:(fun i -> lazy (xlate_type llelts.(i)))
            in
            Typ.mkStruct ~name ~packed elts
    | Token -> Typ.i8p
    | Void | Label | Metadata -> assert false
  in
  Hashtbl.find_or_add memo_type llt ~default:(fun () ->
      [%Trace.call fun pf -> pf "%a" fmt_lltype llt]
      ;
      xlate_type_ llt
      |>
      [%Trace.retn fun pf -> pf "%a" Typ.fmt_defn] )


and xlate_type_opt : Llvm.lltype -> Typ.t option =
 fun llt ->
  match Llvm.classify_type llt with
  | Void -> None
  | _ -> Some (xlate_type llt)


let rec is_zero : Exp.t -> bool =
 fun exp ->
  match exp with
  | Null _ -> true
  | Integer {data} -> Z.equal Z.zero data
  | App {op= Array _ | Struct _; arg} -> is_zero arg
  | App {op; arg} -> is_zero op && is_zero arg
  | _ -> false


let suffix_after_space : string -> string =
 fun str -> String.slice str (String.rindex_exn str ' ' + 1) 0


let xlate_int : Llvm.llvalue -> Exp.t =
 fun llv ->
  let typ = xlate_type (Llvm.type_of llv) in
  let data =
    match Llvm.int64_of_const llv with
    | Some n -> Z.of_int64 n
    | None -> Z.of_string (suffix_after_space (Llvm.string_of_llvalue llv))
  in
  Exp.mkInteger data typ


let xlate_float : Llvm.llvalue -> Exp.t =
 fun llv ->
  let typ = xlate_type (Llvm.type_of llv) in
  let data = suffix_after_space (Llvm.string_of_llvalue llv) in
  Exp.mkFloat data typ


let xlate_name_opt : Llvm.llvalue -> Var.t option =
 fun instr ->
  Option.map
    (xlate_type_opt (Llvm.type_of instr))
    ~f:(fun typ ->
      let name = find_name instr in
      let loc = find_loc instr in
      Var.mk name typ ~loc )


let xlate_name : Llvm.llvalue -> Var.t =
 fun instr -> Option.value_exn (xlate_name_opt instr)


let xlate_intrinsic_exp : string -> (Exp.t -> Exp.t) option =
 fun name ->
  let i32 = Typ.mkInteger ~bits:32 in
  match name with
  | "llvm.eh.typeid.for" -> Some (fun arg -> Exp.mkCast arg i32)
  | _ -> None


let memo_value : (Llvm.llvalue, Exp.t) Hashtbl.t = Hashtbl.Poly.create ()

module Llvalue = struct
  type t = Llvm.llvalue

  let hash = Hashtbl.hash

  let compare = Poly.compare

  let sexp_of_t llv = Sexp.Atom (Llvm.string_of_llvalue llv)
end

let mkStruct_rec = Staged.unstage (Exp.mkStruct_rec (module Llvalue))

let rec xlate_value : Llvm.llvalue -> Exp.t =
 fun llv ->
  let xlate_value_ llv =
    let typ = xlate_type (Llvm.type_of llv) in
    ( match Llvm.classify_value llv with
    | Instruction Call -> (
        let func = Llvm.operand llv (Llvm.num_arg_operands llv) in
        let fname = Llvm.value_name func in
        match xlate_intrinsic_exp fname with
        | Some mkIntrinsic -> mkIntrinsic (xlate_value (Llvm.operand llv 0))
        | None -> Exp.mkVar (xlate_name llv) )
    | Instruction (Invoke | Alloca | Load | PHI | LandingPad | VAArg)
     |Argument ->
        Exp.mkVar (xlate_name llv)
    | Function | GlobalVariable -> Exp.mkGlobal (xlate_global llv)
    | GlobalAlias -> xlate_value (Llvm.operand llv 0)
    | ConstantInt -> xlate_int llv
    | ConstantFP -> xlate_float llv
    | ConstantPointerNull | ConstantAggregateZero -> Exp.mkNull typ
    | ConstantVector | ConstantArray ->
        let len = Llvm.num_operands llv in
        let f i = xlate_value (Llvm.operand llv i) in
        Exp.mkArray (Vector.init len ~f) typ
    | ConstantDataVector ->
        let len = Llvm.vector_size (Llvm.type_of llv) in
        let f i = xlate_value (Llvm.const_element llv i) in
        Exp.mkArray (Vector.init len ~f) typ
    | ConstantDataArray ->
        let len = Llvm.array_length (Llvm.type_of llv) in
        let f i = xlate_value (Llvm.const_element llv i) in
        Exp.mkArray (Vector.init len ~f) typ
    | ConstantStruct ->
        let elt_thks =
          Vector.init (Llvm.num_operands llv) ~f:(fun i ->
              lazy (xlate_value (Llvm.operand llv i)) )
        in
        mkStruct_rec ~id:llv elt_thks typ
    | BlockAddress ->
        let parent = find_name (Llvm.operand llv 0) in
        let name = find_name (Llvm.operand llv 1) in
        Exp.mkLabel ~parent ~name
    | UndefValue -> Exp.mkNondet typ (Llvm.string_of_llvalue llv)
    | Instruction
        ( ( Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP
          | FPTrunc | FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast
          | Add | FAdd | Sub | FSub | Mul | FMul | UDiv | SDiv | FDiv | URem
          | SRem | FRem | Shl | LShr | AShr | And | Or | Xor | ICmp | FCmp
          | Select | GetElementPtr | ExtractElement | InsertElement
          | ExtractValue | InsertValue | ShuffleVector ) as opcode ) ->
        xlate_opcode llv opcode
    | ConstantExpr -> xlate_opcode llv (Llvm.constexpr_opcode llv)
    | GlobalIFunc -> todo "ifuncs: %a" fmt_llvalue llv ()
    | Instruction (CatchPad | CleanupPad | CatchSwitch) ->
        todo "msvc exceptions: %a" fmt_llvalue llv ()
    | Instruction
        ( Invalid | Ret | Br | Switch | IndirectBr | Invalid2 | Unreachable
        | Store | UserOp1 | UserOp2 | Fence | AtomicCmpXchg | AtomicRMW
        | Resume | CleanupRet | CatchRet )
     |NullValue | BasicBlock | InlineAsm | MDNode | MDString ->
        fail "xlate_value: %a" fmt_llvalue llv () )
    |> Exp.locate (find_loc llv)
  in
  Hashtbl.find_or_add memo_value llv ~default:(fun () ->
      [%Trace.call fun pf -> pf "%a" fmt_llvalue llv]
      ;
      xlate_value_ llv
      |>
      [%Trace.retn fun pf exp ->
        let typ = xlate_type (Llvm.type_of llv) in
        let typ' = Exp.typ exp in
        assertf (Typ.equal typ typ')
          "xlate_value translated type %a to %a of %a" Typ.fmt typ Typ.fmt
          typ' fmt_llvalue llv () ;
        pf "%a" Exp.fmt exp] )


and xlate_opcode : Llvm.llvalue -> Llvm.Opcode.t -> Exp.t =
 fun llv opcode ->
  [%Trace.call fun pf -> pf "%a" fmt_llvalue llv]
  ;
  let xlate_rand i = xlate_value (Llvm.operand llv i) in
  let cast () = Exp.mkCast (xlate_rand 0) (xlate_type (Llvm.type_of llv)) in
  let conv signed =
    Exp.mkConv (xlate_rand 0) ~signed (xlate_type (Llvm.type_of llv))
  in
  let binary mk =
    if Poly.equal (Llvm.classify_type (Llvm.type_of llv)) Vector then
      todo "vector operations: %a" fmt_llvalue llv () ;
    mk (xlate_rand 0) (xlate_rand 1)
  in
  let unordered_or mk =
    binary (fun x y -> Exp.mkOr (Exp.mkUno x y) (mk x y))
  in
  ( match opcode with
  | BitCast | AddrSpaceCast -> cast ()
  | Trunc | ZExt | FPToUI | UIToFP | FPTrunc | FPExt | PtrToInt | IntToPtr ->
      conv false
  | SExt | FPToSI | SIToFP -> conv true
  | ICmp -> (
    match Option.value_exn (Llvm.icmp_predicate llv) with
    | Eq -> binary Exp.mkEq
    | Ne -> binary Exp.mkNe
    | Ugt -> binary Exp.mkUgt
    | Uge -> binary Exp.mkUge
    | Ult -> binary Exp.mkUlt
    | Ule -> binary Exp.mkUle
    | Sgt -> binary Exp.mkGt
    | Sge -> binary Exp.mkGe
    | Slt -> binary Exp.mkLt
    | Sle -> binary Exp.mkLe )
  | FCmp -> (
    match Llvm.fcmp_predicate llv with
    | None | Some False -> binary (fun _ _ -> Exp.mkBool false)
    | Some Oeq -> binary Exp.mkEq
    | Some Ogt -> binary Exp.mkGt
    | Some Oge -> binary Exp.mkGe
    | Some Olt -> binary Exp.mkLt
    | Some Ole -> binary Exp.mkLe
    | Some One -> binary Exp.mkNe
    | Some Ord -> binary Exp.mkOrd
    | Some Uno -> binary Exp.mkUno
    | Some Ueq -> unordered_or Exp.mkEq
    | Some Ugt -> unordered_or Exp.mkGt
    | Some Uge -> unordered_or Exp.mkGe
    | Some Ult -> unordered_or Exp.mkLt
    | Some Ule -> unordered_or Exp.mkLe
    | Some Une -> unordered_or Exp.mkNe
    | Some True -> binary (fun _ _ -> Exp.mkBool true) )
  | Add | FAdd -> binary Exp.mkAdd
  | Sub | FSub -> binary Exp.mkSub
  | Mul | FMul -> binary Exp.mkMul
  | SDiv | FDiv -> binary Exp.mkDiv
  | UDiv -> binary Exp.mkUDiv
  | SRem | FRem -> binary Exp.mkRem
  | URem -> binary Exp.mkURem
  | Shl -> binary Exp.mkShl
  | LShr -> binary Exp.mkLShr
  | AShr -> binary Exp.mkAShr
  | And -> binary Exp.mkAnd
  | Or -> binary Exp.mkOr
  | Xor -> binary Exp.mkXor
  | Select ->
      Exp.mkSelect ~cnd:(xlate_rand 0) ~thn:(xlate_rand 1)
        ~els:(xlate_rand 2)
  | ExtractElement -> Exp.mkPrjIdx ~arr:(xlate_rand 0) ~idx:(xlate_rand 1)
  | InsertElement ->
      Exp.mkUpdIdx ~arr:(xlate_rand 0) ~elt:(xlate_rand 1)
        ~idx:(xlate_rand 2)
  | ExtractValue | InsertValue ->
      let agg = xlate_rand 0 in
      let indices = Llvm.indices llv in
      let num = Array.length indices in
      let rec xlate_indices i agg =
        let agg_i, mkUpd =
          match Exp.typ agg with
          | Tuple _ | Struct _ ->
              let fld = indices.(i) in
              (Exp.mkPrjFld ~agg ~fld, Exp.mkUpdFld ~agg ~fld)
          | Array _ ->
              let idx =
                let n = indices.(i) in
                let bits = if n = 0 then 1 else 1 + Int.floor_log2 n in
                Exp.mkInteger (Z.of_int n) (Typ.mkInteger ~bits)
              in
              (Exp.mkPrjIdx ~arr:agg ~idx, Exp.mkUpdIdx ~arr:agg ~idx)
          | _ -> fail "xlate_value: %a" fmt_llvalue llv ()
        in
        let upd_or_ret elt ret =
          match[@warning "p"] opcode with
          | InsertValue -> mkUpd ~elt:(Lazy.force elt)
          | ExtractValue -> ret
        in
        if i < num - 1 then
          let elt = xlate_indices (i + 1) agg_i in
          upd_or_ret (lazy elt) elt
        else
          let elt = lazy (xlate_rand 1) in
          upd_or_ret elt agg_i
      in
      xlate_indices 0 agg
  | GetElementPtr ->
      if Poly.equal (Llvm.classify_type (Llvm.type_of llv)) Vector then
        todo "vector operations: %a" fmt_llvalue llv () ;
      let len = Llvm.num_operands llv in
      if len <= 1 then cast ()
      else
        let rec xlate_indices i =
          let idx = xlate_rand i in
          if i = 1 then
            let base = xlate_rand 0 in
            if is_zero idx then base
            else
              (* translate [gep t*, iN M] as [gep [1 x t]*, iN M] *)
              let ptr =
                match Exp.typ base with
                | Pointer {elt} ->
                    Exp.mkCast base
                      (Typ.mkPointer ~elt:(Typ.mkArray ~elt ~len:1))
                | _ -> fail "xlate_opcode: %a" fmt_llvalue llv ()
              in
              Exp.mkPtrIdx ~ptr ~idx
          else
            let ptr = xlate_indices (i - 1) in
            match (Exp.typ ptr, idx) with
            | Pointer {elt= Array _}, _ -> Exp.mkPtrIdx ~ptr ~idx
            | Pointer {elt= Tuple _ | Struct _}, Integer {data} ->
                Exp.mkPtrFld ~ptr ~fld:(Z.to_int data)
            | _ -> fail "xlate_opcode: %a" fmt_llvalue llv ()
        in
        xlate_indices (len - 1)
  | ShuffleVector -> (
      (* translate shufflevector <N x t> %x, _, <N x i32> zeroinitializer to
         %x *)
      let exp = xlate_value (Llvm.operand llv 0) in
      let llmask = Llvm.operand llv 2 in
      let mask = xlate_value llmask in
      match (Exp.typ exp, Exp.typ mask) with
      | Array {len= m}, Array {len= n} when m = n && Llvm.is_null llmask ->
          exp
      | _ -> fail "xlate_opcode: %a" fmt_llvalue llv () )
  | VAArg -> todo "variadic functions: %a" fmt_llvalue llv ()
  | Invalid | Ret | Br | Switch | IndirectBr | Invoke | Invalid2
   |Unreachable | Alloca | Load | Store | PHI | Call | UserOp1 | UserOp2
   |Fence | AtomicCmpXchg | AtomicRMW | Resume | LandingPad | CleanupRet
   |CatchRet | CatchPad | CleanupPad | CatchSwitch ->
      fail "xlate_opcode: %a" fmt_llvalue llv () )
  |> Exp.locate (find_loc llv)
  |>
  [%Trace.retn fun pf exp ->
    let typ = xlate_type (Llvm.type_of llv) in
    let typ' = Exp.typ exp in
    assertf (Typ.equal typ typ')
      "xlate_opcode translated type %a to %a of %a" Typ.fmt typ Typ.fmt typ'
      fmt_llvalue llv () ;
    pf "%a" Exp.fmt exp]


and xlate_global : Llvm.llvalue -> Global.t =
 fun llg ->
  let init =
    match (Llvm.classify_value llg, Llvm.linkage llg) with
    | _, (External | External_weak) -> None
    | GlobalVariable, _ -> Some (xlate_value (Llvm.global_initializer llg))
    | _ -> None
  in
  let g = xlate_name llg in
  Global.mk (Var.name g) (Var.typ g) ~loc:(Var.loc g) ?init


let xlate_global : Llvm.llvalue -> Global.t =
 fun llg ->
  [%Trace.call fun pf -> pf "%a" fmt_llvalue llg]
  ;
  xlate_global llg
  |>
  [%Trace.retn fun pf -> pf "%a" Global.fmt]


type pop_thunk = Loc.t -> Llair.inst list

let pop_stack_frame_of_function
    : Llvm.llvalue -> Llvm.llbasicblock -> pop_thunk =
 fun func entry_blk ->
  let append_stack_vars blk vars =
    Llvm.fold_right_instrs
      (fun instr vars ->
        match Llvm.instr_opcode instr with
        | Alloca -> xlate_name instr :: vars
        | _ -> vars )
      blk vars
  in
  let entry_vars = append_stack_vars entry_blk [] in
  Llvm.iter_blocks
    (fun blk ->
      if not (Poly.equal entry_blk blk) then
        Llvm.iter_instrs
          (fun instr ->
            match Llvm.instr_opcode instr with
            | Alloca ->
                todo "stack allocation after function entry:@ %a"
                  fmt_llvalue instr ()
            | _ -> () )
          blk )
    func ;
  let pop retn_loc =
    List.map entry_vars ~f:(fun var ->
        Llair.Inst.mkFree ~ptr:(Exp.mkVar var) ~loc:retn_loc )
  in
  pop


(** construct the types involved in landingpads: i32, std::type_info*, and
    __cxa_exception *)
let landingpad_typs : Llvm.llvalue -> Typ.t * Typ.t * Typ.t =
 fun instr ->
  let i32 = Typ.mkInteger ~bits:32 in
  if match xlate_type (Llvm.type_of instr) with
     | Tuple {elts} | Struct {elts} -> (
       match Vector.to_array elts with
       | [|i8p'; i32'|] ->
           not (Typ.equal Typ.i8p i8p') || not (Typ.equal i32 i32')
       | _ -> true )
     | _ -> true
  then
    todo "landingpad of type other than {i8*, i32}: %a" fmt_llvalue instr () ;
  let llcontext =
    Llvm.(module_context (global_parent (block_parent (instr_parent instr))))
  in
  let lli8p = Llvm.(pointer_type (integer_type llcontext 8)) in
  let ti = Llvm.(named_struct_type llcontext "class.std::type_info") in
  let tip = Llvm.pointer_type ti in
  let void = Llvm.void_type llcontext in
  let dtor = Llvm.(pointer_type (function_type void [|lli8p|])) in
  let cxa_exception = Llvm.struct_type llcontext [|tip; dtor|] in
  (i32, xlate_type tip, xlate_type cxa_exception)


(** construct the argument of a landingpad block, mainly fix the encoding
    scheme for landingpad instruction name to block arg name *)
let landingpad_arg : Llvm.llvalue -> Var.t =
 fun instr ->
  Var.mk (find_name instr ^ ".exc") Typ.i8p ~loc:(find_loc instr)


(** [rev_map_phis ~f blk] returns [(retn_arg, rev_args, pos)] by rev_mapping
    over the prefix of [PHI] instructions at the beginning of [blk].
    [retn_arg], if any, is [f] applied to the [PHI] instruction which takes
    the return value of every [Invoke] predecessor of [blk]. [rev_args] is
    the result of applying [f] to each of the other [PHI] instructions.
    [pos] is the instruction iterator position before the first non-[PHI]
    instruction of [blk]. *)
let rev_map_phis
    : f:(Llvm.llvalue -> 'a) -> Llvm.llbasicblock
      -> 'a option * 'a list * _ Llvm.llpos =
 fun ~f blk ->
  let rec block_args_ found_invoke_pred retn_arg rev_args pos =
    match (pos : _ Llvm.llpos) with
    | Before instr -> (
      match Llvm.instr_opcode instr with
      | PHI ->
          (* [has_invoke_pred] holds if some value selected by this PHI is
             the return value of an [invoke] instr. [is_retn_arg] holds if
             for each predecessor terminated by an invoke instr, this PHI
             instr takes the value of the invoke's return value. *)
          let has_invoke_pred, is_retn_arg =
            List.fold (Llvm.incoming instr) ~init:(false, true) ~f:
              (fun (has_invoke_pred, is_retn_arg) (arg, pred) ->
                match Llvm.block_terminator pred with
                | Some instr -> (
                  match Llvm.instr_opcode instr with
                  | Invoke when Poly.equal arg instr -> (true, is_retn_arg)
                  | Invoke -> (has_invoke_pred, false)
                  | _ -> (has_invoke_pred, is_retn_arg) )
                | None -> fail "rev_map_phis: %a" fmt_llblock blk () )
          in
          if found_invoke_pred && has_invoke_pred then
            (* Supporting multiple PHI instructions that take the return
               values of invoke instructions will require adding trampolines
               for the invoke instructions to return to, that each reorder
               arguments and invoke the translation of this block. *)
            todo "multiple PHI instructions taking invoke return values: %a"
              fmt_llblock blk () ;
          let retn_arg, rev_args =
            if has_invoke_pred && is_retn_arg then (Some (f instr), rev_args)
            else (None, f instr :: rev_args)
          in
          block_args_ has_invoke_pred retn_arg rev_args
            (Llvm.instr_succ instr)
      | LandingPad when Option.is_some retn_arg ->
          (* Supporting returning and throwing to the same block, with
             different arguments, will require adding trampolines. *)
          todo
            "return and throw to the same block with different arguments: %a"
            fmt_llblock blk ()
      | _ -> (retn_arg, rev_args, pos) )
    | At_end blk -> fail "rev_map_phis: %a" fmt_llblock blk ()
  in
  block_args_ false None [] (Llvm.instr_begin blk)


(** [trampoline_args jump_instr dest_block] is the actual arguments to which
    the translation of [dest_block] should be partially-applied, to yield a
    trampoline accepting the return parameter of the block and then jumping
    with all the args. *)
let trampoline_args : Llvm.llvalue -> Llvm.llbasicblock -> Exp.t vector =
 fun jmp dst ->
  let src = Llvm.instr_parent jmp in
  rev_map_phis dst ~f:(fun instr ->
      List.find_map_exn (Llvm.incoming instr) ~f:(fun (arg, pred) ->
          if Poly.equal pred src then Some (xlate_value arg) else None ) )
  |> snd3 |> Vector.of_list_rev


(** [unique_pred blk] is the unique predecessor of [blk], or [None] if there
    are 0 or >1 predecessors. *)
let unique_pred : Llvm.llbasicblock -> Llvm.llvalue option =
 fun blk ->
  match Llvm.use_begin (Llvm.value_of_block blk) with
  | Some use -> (
    match Llvm.use_succ use with
    | None -> Some (Llvm.user use)
    | Some _ -> None )
  | None -> None


(** [return_formal_is_used instr] holds if the return value of [instr] is
    used anywhere. *)
let return_formal_is_used : Llvm.llvalue -> bool =
 fun instr -> Option.is_some (Llvm.use_begin instr)


(** [need_return_trampoline instr blk] holds when the return formal of
    [instr] is used, but the returned to block [blk] does not take it as an
    argument (e.g. if it has multiple predecessors and no PHI node). *)
let need_return_trampoline : Llvm.llvalue -> Llvm.llbasicblock -> bool =
 fun instr blk ->
  Option.is_none (fst3 (rev_map_phis blk ~f:Fn.id))
  && Option.is_none (unique_pred blk) && return_formal_is_used instr


(** [unique_used_invoke_pred blk] is the unique predecessor of [blk], if it
    is an [Invoke] instruction, whose return value is used. *)
let unique_used_invoke_pred : Llvm.llbasicblock -> 'a option =
 fun blk ->
  let is_invoke i = Poly.equal (Llvm.instr_opcode i) Invoke in
  match unique_pred blk with
  | Some instr when is_invoke instr && return_formal_is_used instr ->
      Some instr
  | _ -> None


(** formal parameters accepted by a block *)
let block_formals : Llvm.llbasicblock -> Var.t list * _ Llvm.llpos =
 fun blk ->
  let retn_arg, rev_args, pos = rev_map_phis blk ~f:xlate_name in
  match pos with
  | Before instr ->
      let instr_arg =
        match Llvm.instr_opcode instr with
        | LandingPad ->
            assert (Option.is_none retn_arg (* ensured by rev_map_phis *)) ;
            Some (landingpad_arg instr)
        | _ ->
            Option.first_some retn_arg
              (Option.map (unique_used_invoke_pred blk) ~f:xlate_name)
      in
      (List.rev_append rev_args (Option.to_list instr_arg), pos)
  | At_end blk -> fail "block_formals: %a" fmt_llblock blk ()


(** actual arguments passed by a jump to a block *)
let jump_args : Llvm.llvalue -> Llvm.llbasicblock -> Exp.t vector =
 fun jmp dst ->
  let src = Llvm.instr_parent jmp in
  let retn_arg, rev_args, _ =
    rev_map_phis dst ~f:(fun phi ->
        Option.value_exn
          (List.find_map (Llvm.incoming phi) ~f:(fun (arg, pred) ->
               if Poly.equal pred src then Some (xlate_value arg) else None
           )) )
  in
  let retn_arg =
    Option.first_some retn_arg
      (Option.map (unique_used_invoke_pred dst) ~f:(fun invoke ->
           Exp.mkVar (xlate_name invoke) ))
  in
  Vector.of_list (List.rev_append rev_args (Option.to_list retn_arg))


(** An LLVM instruction is translated to a sequence of LLAIR instructions
    and a terminator, plus some additional blocks to which it may refer
    (that is, essentially a function body). These are needed since LLVM and
    LLAIR blocks are not in 1:1 correspondence. *)
type code = Llair.inst list * Llair.term * Llair.block list

let fmt_code ff (insts, term, blocks) =
  Format.fprintf ff "@[<hv>@[%a%t@]%t@[<hv>%a@]@]"
    (list_fmt "@ " Llair.Inst.fmt)
    insts
    (fun ff ->
      match term with
      | Llair.Unreachable -> ()
      | _ ->
          Format.fprintf ff "%t%a"
            (fun ff ->
              if List.is_empty insts then () else Format.fprintf ff "@ " )
            Llair.Term.fmt term )
    (fun ff -> if List.is_empty blocks then () else Format.fprintf ff "@\n")
    (list_fmt "@ " Llair.Block.fmt)
    blocks


let rec xlate_func_name llv =
  match Llvm.classify_value llv with
  | Function ->
      let fname = find_name llv in
      let lltyp = Llvm.type_of llv in
      let typ = xlate_type lltyp in
      Exp.mkGlobal (Global.mk fname typ ~loc:(find_loc llv))
  | ConstantExpr -> xlate_opcode llv (Llvm.constexpr_opcode llv)
  | Argument | Instruction _ -> xlate_value llv
  | GlobalAlias -> xlate_func_name (Llvm.operand llv 0)
  | GlobalIFunc -> todo "ifunc: %a" fmt_llvalue llv ()
  | InlineAsm -> todo "inline asm: %a" fmt_llvalue llv ()
  | _ -> fail "unknown function: %a" fmt_llvalue llv ()


let xlate_instr
    : pop_thunk -> Llvm.llvalue
      -> ((Llair.inst list * Llair.term -> code) -> code) -> code =
 fun pop instr continue ->
  [%Trace.call fun pf -> pf "%a" fmt_llvalue instr]
  ;
  let continue insts_term_to_code =
    [%Trace.retn
      fun pf () ->
        pf "%a" fmt_code (insts_term_to_code ([], Llair.Term.mkUnreachable))]
      () ;
    continue insts_term_to_code
  in
  let terminal insts term blocks =
    [%Trace.retn fun pf () -> pf "%a" fmt_code (insts, term, blocks)] () ;
    (insts, term, blocks)
  in
  let name = find_name instr in
  let loc = find_loc instr in
  let opcode = Llvm.instr_opcode instr in
  match opcode with
  | Load ->
      let reg = xlate_name instr in
      let ptr = xlate_value (Llvm.operand instr 0) in
      continue (fun (insts, term) ->
          (Llair.Inst.mkLoad ~reg ~ptr ~loc :: insts, term, []) )
  | Store ->
      let exp = xlate_value (Llvm.operand instr 0) in
      let ptr = xlate_value (Llvm.operand instr 1) in
      continue (fun (insts, term) ->
          (Llair.Inst.mkStore ~ptr ~exp ~loc :: insts, term, []) )
  | Alloca ->
      let reg = xlate_name instr in
      let num = xlate_value (Llvm.operand instr 0) in
      continue (fun (insts, term) ->
          (Llair.Inst.mkAlloc ~reg ~num ~loc :: insts, term, []) )
  | Call -> (
      let llfunc = Llvm.operand instr (Llvm.num_operands instr - 1) in
      let lltyp = Llvm.type_of llfunc in
      let fname = Llvm.value_name llfunc in
      let reg = xlate_name_opt instr in
      let skip msg =
        warn "ignoring uninterpreted %s %s" msg fname ;
        let msg = Llvm.string_of_llvalue instr in
        continue (fun (insts, term) ->
            (Llair.Inst.mkNondet ~reg ~msg ~loc :: insts, term, []) )
      in
      match String.split fname ~on:'.' with
      | "llvm" :: "memcpy" :: _ ->
          let dst = xlate_value (Llvm.operand instr 0) in
          let src = xlate_value (Llvm.operand instr 1) in
          let len = xlate_value (Llvm.operand instr 2) in
          continue (fun (insts, term) ->
              (Llair.Inst.mkMemcpy ~dst ~src ~len ~loc :: insts, term, [])
          )
      | "llvm" :: "memmov" :: _ ->
          let dst = xlate_value (Llvm.operand instr 0) in
          let src = xlate_value (Llvm.operand instr 1) in
          let len = xlate_value (Llvm.operand instr 2) in
          continue (fun (insts, term) ->
              (Llair.Inst.mkMemmov ~dst ~src ~len ~loc :: insts, term, [])
          )
      | "llvm" :: "memset" :: _ ->
          let dst = xlate_value (Llvm.operand instr 0) in
          let byt = xlate_value (Llvm.operand instr 1) in
          let len = xlate_value (Llvm.operand instr 2) in
          continue (fun (insts, term) ->
              (Llair.Inst.mkMemset ~dst ~byt ~len ~loc :: insts, term, [])
          )
      | _ when Option.is_some (xlate_intrinsic_exp fname) ->
          continue (fun (insts, term) -> (insts, term, []))
      | ["llvm"; "dbg"; ("declare" | "value")]
       |"llvm" :: "lifetime" :: ("start" | "end") :: _ ->
          continue (fun (insts, term) -> (insts, term, []))
      | ["llvm"; ("stacksave" | "stackrestore")] ->
          todo "stack allocation after function entry:@ %a" fmt_llvalue
            instr ()
      | ["llvm"; ("va_start" | "va_copy" | "va_end")] ->
          skip "variadic function intrinsic"
      | "llvm" :: _ -> skip "intrinsic"
      | _ when Poly.equal (Llvm.classify_value llfunc) InlineAsm ->
          skip "inline asm"
      | ["__llair_throw"] ->
          let exc = xlate_value (Llvm.operand instr 0) in
          terminal (pop loc) (Llair.Term.mkThrow ~exc ~loc) []
      | _ ->
          let func = xlate_func_name llfunc in
          let lbl = name ^ ".ret" in
          let call =
            let args =
              let num_args =
                if not (Llvm.is_var_arg (Llvm.element_type lltyp)) then
                  Llvm.num_arg_operands instr
                else (
                  warn
                    "ignoring variable arguments to variadic function: %a"
                    fmt_llvalue instr ;
                  Array.length (Llvm.param_types (Llvm.element_type lltyp)) )
              in
              Vector.init num_args ~f:(fun i ->
                  xlate_value (Llvm.operand instr i) )
            in
            let return = Llair.Jump.mk lbl Vector.empty in
            Llair.Term.mkCall ~func ~args ~loc ~return ~throw:None
              ~ignore_result:false
          in
          let params = Vector.of_option reg in
          continue (fun (insts, term) ->
              let cmnd = Vector.of_list insts in
              ([], call, [Llair.Block.mk ~lbl ~params ~cmnd ~term]) ) )
  | Invoke ->
      let llfunc = Llvm.operand instr (Llvm.num_operands instr - 3) in
      let lltyp = Llvm.type_of llfunc in
      let fname = Llvm.value_name llfunc in
      let unwind_blk = Llvm.get_unwind_dest instr in
      let unwind_dst = label_of_block unwind_blk in
      let args =
        let num_args =
          if not (Llvm.is_var_arg (Llvm.element_type lltyp)) then
            Llvm.num_arg_operands instr
          else (
            warn "ignoring variable arguments to variadic function: %a"
              fmt_llvalue instr ;
            Array.length (Llvm.param_types (Llvm.element_type lltyp)) )
        in
        Vector.init num_args ~f:(fun i -> xlate_value (Llvm.operand instr i))
      in
      if String.equal fname "__llair_throw" then
        let key = Exp.mkInteger Z.zero Typ.i1 in
        let tbl = Vector.empty in
        let els = Llair.Jump.mk unwind_dst args in
        terminal [] (Llair.Term.mkSwitch ~key ~tbl ~els ~loc) []
      else
        let func = xlate_func_name llfunc in
        let typ = xlate_type (Llvm.type_of llfunc) in
        let ignore_result =
          match typ with
          | Pointer {elt= Function {return= Some _}} ->
              not (return_formal_is_used instr)
          | _ -> false
        in
        let return, blocks =
          let blk = Llvm.get_normal_dest instr in
          if not (need_return_trampoline instr blk) then
            let dst = label_of_block blk in
            let args = trampoline_args instr blk in
            (Llair.Jump.mk dst args, [])
          else
            let lbl = name ^ ".ret" in
            let block =
              let params = Vector.of_array [|xlate_name instr|] in
              let cmnd = Vector.empty in
              let term =
                let key = Exp.mkInteger Z.zero Typ.i1 in
                let tbl = Vector.empty in
                let dst = label_of_block blk in
                let args = jump_args instr blk in
                let els = Llair.Jump.mk dst args in
                Llair.Term.mkSwitch ~key ~tbl ~els ~loc
              in
              Llair.Block.mk ~lbl ~params ~cmnd ~term
            in
            (Llair.Jump.mk lbl Vector.empty, [block])
        in
        let throw =
          let dst = unwind_dst in
          let args = trampoline_args instr unwind_blk in
          Some (Llair.Jump.mk dst args)
        in
        terminal []
          (Llair.Term.mkCall ~func ~args ~loc ~return ~throw ~ignore_result)
          blocks
  | Ret ->
      let exp =
        if Llvm.num_operands instr = 0 then None
        else Some (xlate_value (Llvm.operand instr 0))
      in
      terminal (pop loc) (Llair.Term.mkReturn ~exp ~loc) []
  | Br -> (
    match Option.value_exn (Llvm.get_branch instr) with
    | `Unconditional blk ->
        let key = Exp.mkInteger Z.zero Typ.i1 in
        let tbl = Vector.empty in
        let dst = label_of_block blk in
        let args = jump_args instr blk in
        let els = Llair.Jump.mk dst args in
        terminal [] (Llair.Term.mkSwitch ~key ~tbl ~els ~loc) []
    | `Conditional (cnd, thn, els) ->
        let key = xlate_value cnd in
        let thn_lbl = label_of_block thn in
        let thn_args = jump_args instr thn in
        let thn = Llair.Jump.mk thn_lbl thn_args in
        let tbl = Vector.of_array [|(Z.one, thn)|] in
        let els_lbl = label_of_block els in
        let els_args = jump_args instr els in
        let els = Llair.Jump.mk els_lbl els_args in
        terminal [] (Llair.Term.mkSwitch ~key ~tbl ~els ~loc) [] )
  | Switch ->
      let key = xlate_value (Llvm.operand instr 0) in
      let cases =
        let num_cases = Llvm.num_operands instr / 2 - 1 in
        let rec xlate_cases i =
          if i <= num_cases then
            let idx = Llvm.operand instr (2 * i) in
            let blk =
              Llvm.block_of_value (Llvm.operand instr (2 * i + 1))
            in
            let num =
              match xlate_value idx with
              | Exp.Integer {data} -> data
              | _ -> fail "xlate_instr: %a" fmt_llvalue instr ()
            in
            let dst = label_of_block blk in
            let args = jump_args instr blk in
            let rest = xlate_cases (i + 1) in
            (num, Llair.Jump.mk dst args) :: rest
          else []
        in
        xlate_cases 1
      in
      let tbl = Vector.of_list cases in
      let blk = Llvm.block_of_value (Llvm.operand instr 1) in
      let dst = label_of_block blk in
      let args = jump_args instr blk in
      let els = Llair.Jump.mk dst args in
      terminal [] (Llair.Term.mkSwitch ~key ~tbl ~els ~loc) []
  | IndirectBr ->
      let ptr = xlate_value (Llvm.operand instr 0) in
      let num_dests = Llvm.num_operands instr - 1 in
      let lldests =
        let rec dests i =
          if i <= num_dests then
            let v = Llvm.operand instr i in
            let blk = Llvm.block_of_value v in
            let dst = label_of_block blk in
            let args = jump_args instr blk in
            let rest = dests (i + 1) in
            Llair.Jump.mk dst args :: rest
          else []
        in
        dests 1
      in
      let tbl = Vector.of_list lldests in
      terminal [] (Llair.Term.mkISwitch ~ptr ~tbl ~loc) []
  | LandingPad ->
      (* Translate the landingpad clauses to code to load the type_info from
         the thrown exception, and test the type_info against the clauses,
         eventually jumping to the handler code following the landingpad,
         passing a value for the selector which the handler code tests to
         e.g. either cleanup or rethrow. *)
      let i32, tip, cxa_exception = landingpad_typs instr in
      let exc = Exp.mkVar (landingpad_arg instr) in
      let ti = Var.mk (name ^ ".ti") tip ~loc in
      (* std::type_info* ti = ((__cxa_exception* )exc - 1)->exceptionType *)
      let load_ti =
        (* index of the exceptionType member of __cxa_exception *)
        let exceptionType = 0 in
        let ptr =
          Exp.mkPtrFld
            ~ptr:
              (Exp.mkCast
                 (Exp.mkPtrIdx
                    ~ptr:
                      (Exp.mkCast exc
                         (Typ.mkPointer
                            ~elt:(Typ.mkArray ~elt:cxa_exception ~len:1)))
                    ~idx:
                      (Exp.mkInteger Z.minus_one (Typ.mkInteger ~bits:64)))
                 (Typ.mkPointer ~elt:cxa_exception))
            ~fld:exceptionType
        in
        Llair.Inst.mkLoad ~reg:ti ~ptr ~loc
      in
      let ti = Exp.mkVar ti in
      let typeid =
        Option.value_exn (xlate_intrinsic_exp "llvm.eh.typeid.for") ti
      in
      let lbl = name ^ ".unwind" in
      let param = xlate_name instr in
      let params = Vector.of_array [|param|] in
      let jump_unwind sel =
        let dst = lbl in
        let args =
          Vector.of_array
            [|Exp.mkStruct (Vector.of_array [|exc; sel|]) (Var.typ param)|]
        in
        Llair.Jump.mk dst args
      in
      let goto_unwind sel =
        let key = Exp.mkInteger Z.zero Typ.i1 in
        let tbl = Vector.empty in
        let els = jump_unwind sel in
        Llair.Term.mkSwitch ~key ~tbl ~els ~loc
      in
      let term_unwind, rev_blocks =
        if Llvm.is_cleanup instr then
          (goto_unwind (Exp.mkInteger Z.zero i32), [])
        else
          let num_clauses = Llvm.num_operands instr in
          let lbl i = name ^ "." ^ Int.to_string i in
          let jump i = Llair.Jump.mk (lbl i) Vector.empty in
          let block i term =
            Llair.Block.mk ~lbl:(lbl i) ~params:Vector.empty
              ~cmnd:Vector.empty ~term
          in
          let match_filter =
            jump_unwind (Exp.mkSub (Exp.mkInteger Z.zero i32) typeid)
          in
          let xlate_clause i =
            let clause = Llvm.operand instr i in
            let num_tis = Llvm.num_operands clause in
            if num_tis = 0 then
              let key = Exp.mkInteger Z.zero Typ.i1 in
              let tbl = Vector.empty in
              let els = match_filter in
              Llair.Term.mkSwitch ~key ~tbl ~els ~loc
            else
              match Llvm.classify_type (Llvm.type_of clause) with
              | Array (* filter *) -> (
                match Llvm.classify_value clause with
                | ConstantArray ->
                    let rec xlate_filter i =
                      let tiI =
                        Exp.mkCast (xlate_value (Llvm.operand clause i)) tip
                      in
                      if i < num_tis - 1 then
                        Exp.mkAnd (Exp.mkNe tiI ti) (xlate_filter (i + 1))
                      else Exp.mkNe tiI ti
                    in
                    let key = xlate_filter 0 in
                    let thn = match_filter in
                    let tbl = Vector.of_array [|(Z.one, thn)|] in
                    Llair.Term.mkSwitch ~key ~tbl ~els:(jump (i + 1)) ~loc
                | _ -> fail "xlate_instr: %a" fmt_llvalue instr () )
              | _ (* catch *) ->
                  let clause = Exp.mkCast (xlate_value clause) tip in
                  let key =
                    Exp.mkOr
                      (Exp.mkEq clause (Exp.mkNull tip))
                      (Exp.mkEq clause ti)
                  in
                  let thn = jump_unwind typeid in
                  let tbl = Vector.of_array [|(Z.one, thn)|] in
                  Llair.Term.mkSwitch ~key ~tbl ~els:(jump (i + 1)) ~loc
          in
          let rec rev_blocks i z =
            if i < num_clauses then
              rev_blocks (i + 1) (block i (xlate_clause i) :: z)
            else block i Llair.Term.mkUnreachable :: z
          in
          (xlate_clause 0, rev_blocks 1 [])
      in
      continue (fun (insts, term) ->
          ( [load_ti]
          , term_unwind
          , List.rev_append rev_blocks
              [ Llair.Block.mk ~lbl ~params ~cmnd:(Vector.of_list insts)
                  ~term ] ) )
  | Resume ->
      let exc =
        Exp.mkPrjFld ~agg:(xlate_value (Llvm.operand instr 0)) ~fld:0
      in
      terminal (pop loc) (Llair.Term.mkThrow ~exc ~loc) []
  | Unreachable -> terminal [] Llair.Term.mkUnreachable []
  | Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP | FPTrunc
   |FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast | Add | FAdd
   |Sub | FSub | Mul | FMul | UDiv | SDiv | FDiv | URem | SRem | FRem
   |Shl | LShr | AShr | And | Or | Xor | ICmp | FCmp | Select
   |GetElementPtr | ExtractElement | InsertElement | ShuffleVector
   |ExtractValue | InsertValue ->
      continue (fun (insts, term) -> (insts, term, []))
  | VAArg ->
      let reg = xlate_name_opt instr in
      let msg = Llvm.string_of_llvalue instr in
      warn "variadic function argument: %s" msg ;
      continue (fun (insts, term) ->
          (Llair.Inst.mkNondet ~reg ~msg ~loc :: insts, term, []) )
  | CleanupRet | CatchRet | CatchPad | CleanupPad | CatchSwitch ->
      todo "msvc exceptions: %a" fmt_llvalue instr ()
  | Fence | AtomicCmpXchg | AtomicRMW ->
      fail "xlate_instr: %a" fmt_llvalue instr ()
  | PHI | Invalid | Invalid2 | UserOp1 | UserOp2 -> assert false


let rec xlate_instrs : pop_thunk -> _ Llvm.llpos -> code =
 fun pop -> function
  | Before instrI ->
      xlate_instr pop instrI (fun xlate_instrI ->
          let instrJ = Llvm.instr_succ instrI in
          let instsJ, termJ, blocksJN = xlate_instrs pop instrJ in
          let instsI, termI, blocksI = xlate_instrI (instsJ, termJ) in
          (instsI, termI, blocksI @ blocksJN) )
  | At_end blk -> fail "xlate_instrs: %a" fmt_llblock blk ()


let xlate_block : pop_thunk -> Llvm.llbasicblock -> Llair.block list =
 fun pop blk ->
  [%Trace.call fun pf -> pf "%a" fmt_llblock blk]
  ;
  let lbl = label_of_block blk in
  let args, pos = block_formals blk in
  let insts, term, blocks = xlate_instrs pop pos in
  Llair.Block.mk ~lbl ~params:(Vector.of_list args)
    ~cmnd:(Vector.of_list insts) ~term
  :: blocks
  |>
  [%Trace.retn fun pf blocks -> pf "%s" (List.hd_exn blocks).Llair.lbl]


let xlate_function : Llvm.llvalue -> Llair.func =
 fun llf ->
  [%Trace.call fun pf -> pf "%a" fmt_llvalue llf]
  ;
  let name = xlate_global llf in
  let params =
    Llvm.fold_left_params
      (fun rev_args param -> xlate_name param :: rev_args)
      [] llf
    |> Vector.of_list_rev
  in
  ( match Llvm.block_begin llf with
  | Before entry_blk ->
      let pop = pop_stack_frame_of_function llf entry_blk in
      let[@warning "p"] entry_block :: entry_blocks =
        xlate_block pop entry_blk
      in
      let entry =
        let {Llair.lbl; cmnd; term} = entry_block in
        assert (Vector.is_empty entry_block.params) ;
        Llair.Block.mk ~lbl ~params ~cmnd ~term
      in
      let cfg =
        let rec trav_blocks rev_cfg prev =
          match Llvm.block_succ prev with
          | Before blk ->
              trav_blocks
                (List.rev_append (xlate_block pop blk) rev_cfg)
                blk
          | At_end _ -> Vector.of_list_rev rev_cfg
        in
        trav_blocks (List.rev entry_blocks) entry_blk
      in
      Llair.Func.mk ~name ~entry ~cfg
  | At_end _ -> Llair.Func.mk_undefined ~name ~params )
  |>
  [%Trace.retn fun pf -> pf "@\n%a" Llair.Func.fmt]


let transform : Llvm.llmodule -> unit =
 fun llmodule ->
  let pm = Llvm.PassManager.create () in
  Llvm_scalar_opts.add_lower_atomic pm ;
  Llvm_scalar_opts.add_scalar_repl_aggregation pm ;
  Llvm_scalar_opts.add_scalarizer pm ;
  Llvm_scalar_opts.add_merge_return pm ;
  Llvm_scalar_opts.add_cfg_simplification pm ;
  Llvm.PassManager.run_module llmodule pm |> (ignore : bool -> _) ;
  Llvm.PassManager.dispose pm


exception Invalid_llvm of string

let invalid_llvm : string -> 'a =
 fun msg ->
  let first_line =
    Option.value_map (String.index msg '\n') ~default:msg
      ~f:(String.slice msg 0)
  in
  Format.printf "@\n%s@\n" msg ;
  raise (Invalid_llvm first_line)


let translate : string -> Llair.t =
 fun file ->
  [%Trace.call fun pf -> pf "%s" file]
  ;
  Llvm.install_fatal_error_handler invalid_llvm ;
  let llcontext = Llvm.global_context () in
  let llmodule =
    let llmemorybuffer = Llvm.MemoryBuffer.of_file file in
    try Llvm_irreader.parse_ir llcontext llmemorybuffer
    with Llvm_irreader.Error msg -> invalid_llvm msg
  in
  Llvm_analysis.verify_module llmodule |> Option.iter ~f:invalid_llvm ;
  transform llmodule ;
  scan_locs llmodule ;
  scan_names llmodule ;
  let globals =
    Llvm.fold_left_globals
      (fun globals llg -> xlate_global llg :: globals)
      [] llmodule
  in
  let functions =
    Llvm.fold_left_functions
      (fun functions llf ->
        let name = Llvm.value_name llf in
        if String.is_prefix name ~prefix:"__llair_"
           || String.is_prefix name ~prefix:"llvm."
        then functions
        else xlate_function llf :: functions )
      [] llmodule
  in
  let typ_defns =
    let by_name x y =
      let name = function[@warning "p"]
        | Typ.Struct {name} | Opaque {name} -> name
      in
      String.compare (name x) (name y)
    in
    Hashtbl.fold memo_type ~init:[] ~f:(fun ~key:_ ~data defns ->
        match data with
        | Typ.Struct _ | Opaque _ -> data :: defns
        | _ -> defns )
    |> List.sort ~cmp:by_name
  in
  Llvm.dispose_module llmodule ;
  Llvm.dispose_context llcontext ;
  Llair.mk ~typ_defns ~globals ~functions
  |>
  [%Trace.retn fun pf _ -> pf ""]
