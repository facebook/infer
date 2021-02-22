(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Translate LLVM to LLAIR *)

open Llair

let pp_lltype fs t = Format.pp_print_string fs (Llvm.string_of_lltype t)

(* WARNING: SLOW on instructions and functions *)
let pp_llvalue fs t = Format.pp_print_string fs (Llvm.string_of_llvalue t)

let pp_llblock fs t =
  Format.pp_print_string fs (Llvm.string_of_llvalue (Llvm.value_of_block t))

;;
Exp.demangle :=
  let open Ctypes in
  let cxa_demangle =
    (* char *__cxa_demangle(const char *, char *, size_t *, int * ) *)
    Foreign.foreign "__cxa_demangle"
      ( string
      @-> ptr char
      @-> ptr size_t
      @-> ptr int
      @-> returning string_opt )
  in
  let null_ptr_char = from_voidp char null in
  let null_ptr_size_t = from_voidp size_t null in
  let status = allocate int 0 in
  fun mangled ->
    if String.prefix ~pre:"_Z" mangled then
      let demangled =
        cxa_demangle mangled null_ptr_char null_ptr_size_t status
      in
      if !@status = 0 then demangled else None
    else None

exception Invalid_llvm of string

let invalid_llvm : string -> 'a =
 fun msg ->
  let first_line =
    Option.map_or ~default:msg
      ~f:(fun i -> String.take i msg)
      (String.index msg '\n')
  in
  Format.printf "@\n%s@\n" msg ;
  raise (Invalid_llvm first_line)

(* gather names and debug locations *)

(* number of register ids used so far *)
let sym_count = ref 0

(* map frontend-synthesized register names to ids, registers that come from
   LLVM are stored in sym_tbl *)
let id_memo = String.Tbl.create ()

(* lookup or generate the id for a frontend-synthesized register name *)
let get_id name =
  String.Tbl.find_or_add id_memo name ~default:(fun () ->
      incr sym_count ;
      !sym_count )

module LlvalueTbl = HashTable.Make (struct
  type t = Llvm.llvalue

  include Poly
end)

module SymTbl = LlvalueTbl

let sym_tbl : (string * int * Loc.t) SymTbl.t =
  SymTbl.create ~size:4_194_304 ()

module ScopeTbl = HashTable.Make (struct
  type t = [`Fun of Llvm.llvalue | `Mod of Llvm.llmodule]

  include Poly
end)

let scope_tbl : (int ref * int String.Tbl.t) ScopeTbl.t =
  ScopeTbl.create ~size:32_768 ()

let realpath_tbl = String.Tbl.create ()

let get_debug_loc_directory llv =
  let+ dir = Llvm.get_debug_loc_directory llv in
  if String.is_empty dir then dir
  else
    String.Tbl.find_or_add realpath_tbl dir ~default:(fun () ->
        try Core.Filename.realpath dir with Unix.Unix_error _ -> dir )

open struct
  open struct
    let loc_of_global g =
      Loc.mk
        ?dir:(get_debug_loc_directory g)
        ?file:(Llvm.get_debug_loc_filename g)
        ~line:(Llvm.get_debug_loc_line g)
        ?col:None

    let loc_of_function f =
      Loc.mk
        ?dir:(get_debug_loc_directory f)
        ?file:(Llvm.get_debug_loc_filename f)
        ~line:(Llvm.get_debug_loc_line f)
        ?col:None

    let loc_of_instr i =
      Loc.mk
        ?dir:(get_debug_loc_directory i)
        ?file:(Llvm.get_debug_loc_filename i)
        ~line:(Llvm.get_debug_loc_line i)
        ~col:(Llvm.get_debug_loc_column i)

    let find_scope scope =
      ScopeTbl.find_or_add scope_tbl scope ~default:(fun () ->
          (ref 0, String.Tbl.create ()) )

    let add_sym llv loc =
      let maybe_scope =
        match Llvm.classify_value llv with
        | Argument -> Some (`Fun (Llvm.param_parent llv))
        | BasicBlock ->
            Some (`Fun (Llvm.block_parent (Llvm.block_of_value llv)))
        | Instruction _ ->
            Some (`Fun (Llvm.block_parent (Llvm.instr_parent llv)))
        | GlobalVariable | Function -> Some (`Mod (Llvm.global_parent llv))
        | UndefValue -> None
        | ConstantExpr -> None
        | ConstantPointerNull -> None
        | _ ->
            warn "Unexpected type of llv, might crash: %a" pp_llvalue llv () ;
            Some (`Mod (Llvm.global_parent llv))
      in
      match maybe_scope with
      | None -> ()
      | Some scope -> (
        match SymTbl.find sym_tbl llv with
        | Some (name, id, loc0) ->
            if Loc.equal loc0 Loc.none then
              SymTbl.set sym_tbl ~key:llv ~data:(name, id, loc)
        | None ->
            let name =
              if Poly.(Llvm.classify_type (Llvm.type_of llv) = Void) then
                if Poly.(Llvm.classify_value llv = Instruction Call) then (
                  (* LLVM does not give unique names to the result of
                     void-returning function calls. We need unique names for
                     these as they determine the labels of newly-created
                     return blocks. *)
                  let next, void_tbl = find_scope scope in
                  let fname =
                    match
                      Llvm.(value_name (operand llv (num_operands llv - 1)))
                    with
                    | "" -> Int.to_string (!next - 1)
                    | s -> s
                  in
                  match String.Tbl.find void_tbl fname with
                  | None ->
                      String.Tbl.set void_tbl ~key:fname ~data:1 ;
                      fname ^ ".void"
                  | Some count ->
                      String.Tbl.set void_tbl ~key:fname ~data:(count + 1) ;
                      String.concat ~sep:""
                        [fname; ".void."; Int.to_string count] )
                else ""
              else
                match Llvm.value_name llv with
                | "" ->
                    (* anonymous values take the next SSA name *)
                    let next, _ = find_scope scope in
                    let name = !next in
                    next := name + 1 ;
                    Int.to_string name
                | name -> (
                  match Int.of_string name with
                  | Some _ ->
                      (* escape to avoid clash with names of anonymous
                         values *)
                      "\"" ^ name ^ "\""
                  | None -> name )
            in
            let id = 1 + SymTbl.length sym_tbl in
            SymTbl.set sym_tbl ~key:llv ~data:(name, id, loc) )
  end

  let scan_names_and_locs : Llvm.llmodule -> unit =
   fun m ->
    assert (!sym_count = 0) ;
    let scan_global g = add_sym g (loc_of_global g) in
    let scan_instr i =
      let loc = loc_of_instr i in
      add_sym i loc ;
      match Llvm.instr_opcode i with
      | Call -> (
        match Llvm.(value_name (operand i (num_arg_operands i))) with
        | "llvm.dbg.declare" ->
            let md = Llvm.(get_mdnode_operands (operand i 0)) in
            if not (Array.is_empty md) then add_sym md.(0) loc
            else
              warn
                "could not find variable for debug info at %a with \
                 metadata %a"
                Loc.pp loc (List.pp ", " pp_llvalue) (Array.to_list md) ()
        | _ -> () )
      | _ -> ()
    in
    let scan_block b =
      add_sym (Llvm.value_of_block b) Loc.none ;
      Llvm.iter_instrs scan_instr b
    in
    let scan_function f =
      Llvm.iter_params (fun prm -> add_sym prm Loc.none) f ;
      add_sym f (loc_of_function f) ;
      Llvm.iter_blocks scan_block f
    in
    Llvm.iter_globals scan_global m ;
    Llvm.iter_functions scan_function m ;
    sym_count := SymTbl.length sym_tbl

  let find_name : Llvm.llvalue -> string * int =
   fun v ->
    let name, id, _ = SymTbl.find_exn sym_tbl v in
    assert (not (String.is_empty name)) ;
    (name, id)

  let find_loc : Llvm.llvalue -> Loc.t =
   fun v -> trd3 (SymTbl.find_exn sym_tbl v)
end

let label_of_block : Llvm.llbasicblock -> string =
 fun blk -> fst (find_name (Llvm.value_of_block blk))

module LltypeTbl = HashTable.Make (struct
  type t = Llvm.lltype

  include Poly
end)

let anon_struct_name : string LltypeTbl.t = LltypeTbl.create ()

let struct_name : Llvm.lltype -> string =
 fun llt ->
  match Llvm.struct_name llt with
  | Some name -> name
  | None ->
      LltypeTbl.find_or_add anon_struct_name llt ~default:(fun () ->
          Int.to_string (LltypeTbl.length anon_struct_name) )

type x = {llcontext: Llvm.llcontext; lldatalayout: Llvm_target.DataLayout.t}

let ptr_siz : x -> int =
 fun x -> Llvm_target.DataLayout.pointer_size x.lldatalayout

let size_of, bit_size_of =
  let size_to_int size_of x llt =
    if Llvm.type_is_sized llt then
      match Int64.unsigned_to_int (size_of llt x.lldatalayout) with
      | Some n -> n
      | None -> todo "type size too large: %a" pp_lltype llt ()
    else todo "types with undetermined size: %a" pp_lltype llt ()
  in
  ( size_to_int Llvm_target.DataLayout.abi_size
  , size_to_int Llvm_target.DataLayout.size_in_bits )

let memo_type : Typ.t LltypeTbl.t = LltypeTbl.create ()

let rec xlate_type : x -> Llvm.lltype -> Typ.t =
 fun x llt ->
  let xlate_type_ llt =
    if Llvm.type_is_sized llt then
      let byts = size_of x llt in
      let bits = bit_size_of x llt in
      match Llvm.classify_type llt with
      | Half | Float | Double | Fp128 -> Typ.float ~bits ~byts ~enc:`IEEE
      | X86fp80 -> Typ.float ~bits ~byts ~enc:`Extended
      | Ppc_fp128 -> Typ.float ~bits ~byts ~enc:`Pair
      | Integer -> Typ.integer ~bits ~byts
      | X86_mmx -> Typ.integer ~bits ~byts
      | Pointer ->
          if byts <> ptr_siz x then
            todo "non-integral pointer types: %a" pp_lltype llt () ;
          let elt = xlate_type x (Llvm.element_type llt) in
          Typ.pointer ~elt
      | Array ->
          let elt = xlate_type x (Llvm.element_type llt) in
          let len = Llvm.array_length llt in
          Typ.array ~elt ~len ~bits ~byts
      | Struct ->
          let llelts = Llvm.struct_element_types llt in
          let len = Array.length llelts in
          let fld_off i =
            match
              Int64.unsigned_to_int
                (Llvm_target.DataLayout.offset_of_element llt i
                   x.lldatalayout)
            with
            | Some i -> i
            | None -> todo "offset too large: %a" pp_lltype llt ()
          in
          if Llvm.is_literal llt then
            let elts =
              IArray.mapi
                ~f:(fun i elt -> (fld_off i, xlate_type x elt))
                (IArray.of_array llelts)
            in
            Typ.tuple elts ~bits ~byts
          else
            let name = struct_name llt in
            let elts =
              IArray.init len ~f:(fun i ->
                  lazy (fld_off i, xlate_type x llelts.(i)) )
            in
            Typ.struct_ ~name elts ~bits ~byts
      | Function -> fail "expected to be unsized: %a" pp_lltype llt ()
      | Vector -> todo "vector types: %a" pp_lltype llt ()
      | Void | Label | Metadata | Token -> assert false
    else
      match Llvm.classify_type llt with
      | Function ->
          let return = xlate_type_opt x (Llvm.return_type llt) in
          let llargs = Llvm.param_types llt in
          let len = Array.length llargs in
          let args =
            IArray.init len ~f:(fun i -> xlate_type x llargs.(i))
          in
          Typ.function_ ~return ~args
      | Struct when Llvm.is_opaque llt -> Typ.opaque ~name:(struct_name llt)
      | Token -> Typ.opaque ~name:"token"
      | Vector | Array | Struct ->
          todo "unsized non-opaque aggregate types: %a" pp_lltype llt ()
      | Half | Float | Double | X86fp80 | Fp128 | Ppc_fp128 | Integer
       |X86_mmx | Pointer ->
          fail "expected to be sized: %a" pp_lltype llt ()
      | Void | Label | Metadata -> assert false
  in
  LltypeTbl.find_or_add memo_type llt ~default:(fun () ->
      [%Trace.call fun {pf} -> pf "@ %a" pp_lltype llt]
      ;
      xlate_type_ llt
      |>
      [%Trace.retn fun {pf} ty ->
        pf "%a" Typ.pp_defn ty ;
        assert (
          (not (Llvm.type_is_sized llt))
          || (not (Typ.is_sized ty))
          || Typ.size_of ty = size_of x llt )] )

and xlate_type_opt : x -> Llvm.lltype -> Typ.t option =
 fun x llt ->
  match Llvm.classify_type llt with
  | Void -> None
  | _ -> Some (xlate_type x llt)

let i32 x = xlate_type x (Llvm.i32_type x.llcontext)

let suffix_after_last_space : string -> string =
 fun str -> String.drop (String.rindex_exn str ' ' + 1) str

let xlate_int : x -> Llvm.llvalue -> Exp.t =
 fun x llv ->
  let llt = Llvm.type_of llv in
  let typ = xlate_type x llt in
  let data =
    match Llvm.int64_of_const llv with
    | Some n -> Z.of_int64 n
    | None ->
        Z.of_string (suffix_after_last_space (Llvm.string_of_llvalue llv))
  in
  Exp.integer typ data

let xlate_float : x -> Llvm.llvalue -> Exp.t =
 fun x llv ->
  let llt = Llvm.type_of llv in
  let typ = xlate_type x llt in
  let data = suffix_after_last_space (Llvm.string_of_llvalue llv) in
  Exp.float typ data

let xlate_name x : Llvm.llvalue -> Reg.t =
 fun llv ->
  let typ = xlate_type x (Llvm.type_of llv) in
  let name, id = find_name llv in
  Reg.mk typ id name

let xlate_name_opt : x -> Llvm.llvalue -> Reg.t option =
 fun x instr ->
  let llt = Llvm.type_of instr in
  match Llvm.classify_type llt with
  | Void -> None
  | _ -> Some (xlate_name x instr)

let pp_prefix_exp fs (insts, exp) =
  Format.fprintf fs "@[%a%t%a@]" (List.pp "@ " Inst.pp) insts
    (fun fs -> if List.is_empty insts then () else Format.fprintf fs "@ ")
    Exp.pp exp

(* per-function count of 'undef' values, used to translate each occurrence
   of 'undef' to a distinct register *)
let undef_count = ref 0

module GlobTbl = LlvalueTbl

let memo_global : GlobalDefn.t GlobTbl.t = GlobTbl.create ()

module ValTbl = HashTable.Make (struct
  type t = bool * Llvm.llvalue

  include Poly
end)

let memo_value : (Inst.t list * Exp.t) ValTbl.t = ValTbl.create ()

let should_inline : Llvm.llvalue -> bool =
 fun llv ->
  match Llvm.use_begin llv with
  | Some use -> (
    match Llvm.use_succ use with
    | Some _ -> (
      match Llvm.classify_value llv with
      | Instruction
          ( Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP
          | FPTrunc | FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast
            ) ->
          true (* inline casts *)
      | _ -> false (* do not inline if >= 2 uses *) )
    | None -> true )
  | None -> true

let ptr_fld x ~ptr ~fld ~lltyp =
  let offset =
    Llvm_target.DataLayout.offset_of_element lltyp fld x.lldatalayout
  in
  Exp.add ~typ:Typ.ptr ptr (Exp.integer Typ.siz (Z.of_int64 offset))

let ptr_idx x ~ptr ~idx ~llelt =
  let stride = Llvm_target.DataLayout.abi_size llelt x.lldatalayout in
  Exp.add ~typ:Typ.ptr ptr
    (Exp.mul ~typ:Typ.siz (Exp.integer Typ.siz (Z.of_int64 stride)) idx)

let convert_to_siz =
  let siz_bits = Typ.bit_size_of Typ.siz in
  fun typ arg ->
    match (typ : Typ.t) with
    | Integer {bits} ->
        if siz_bits < bits then Exp.signed siz_bits arg ~to_:Typ.siz
        else if siz_bits > bits then Exp.signed bits arg ~to_:Typ.siz
        else arg
    | _ -> fail "convert_to_siz: %a" Typ.pp typ ()

let xlate_llvm_eh_typeid_for : x -> Typ.t -> Exp.t -> Exp.t =
 fun x typ arg -> Exp.convert typ ~to_:(i32 x) arg

let rec xlate_intrinsic_exp :
    string -> (x -> Llvm.llvalue -> Inst.t list * Exp.t) option =
 fun name ->
  match name with
  | "llvm.eh.typeid.for" ->
      Some
        (fun x llv ->
          let rand = Llvm.operand llv 0 in
          let pre, arg = xlate_value x rand in
          let src = xlate_type x (Llvm.type_of rand) in
          (pre, xlate_llvm_eh_typeid_for x src arg) )
  | _ -> None

and xlate_values x len val_i =
  let xlate_i j pre_0_i =
    let pre_j, arg_j = xlate_value x (val_i j) in
    (arg_j, Iter.append pre_0_i (Iter.of_list pre_j))
  in
  let pre, vals = Iter.(fold_map (0 -- (len - 1)) empty ~f:xlate_i) in
  (Iter.to_list pre, IArray.of_iter vals)

and xlate_value ?(inline = false) : x -> Llvm.llvalue -> Inst.t list * Exp.t
    =
 fun x llv ->
  let xlate_value_ llv =
    match Llvm.classify_value llv with
    | Instruction Call -> (
        let func = Llvm.operand llv (Llvm.num_arg_operands llv) in
        let fname = Llvm.value_name func in
        match xlate_intrinsic_exp fname with
        | Some intrinsic when inline || should_inline llv -> intrinsic x llv
        | _ -> ([], Exp.reg (xlate_name x llv)) )
    | Instruction (Invoke | Alloca | Load | PHI | LandingPad | VAArg)
     |Argument ->
        ([], Exp.reg (xlate_name x llv))
    | Function ->
        ( []
        , Exp.function_
            (Function.mk
               (xlate_type x (Llvm.type_of llv))
               (fst (find_name llv))) )
    | GlobalVariable -> ([], Exp.global (xlate_global x llv).name)
    | GlobalAlias -> xlate_value x (Llvm.operand llv 0)
    | ConstantInt -> ([], xlate_int x llv)
    | ConstantFP -> ([], xlate_float x llv)
    | ConstantPointerNull -> ([], Exp.null)
    | ConstantAggregateZero -> (
        let typ = xlate_type x (Llvm.type_of llv) in
        match typ with
        | Integer _ -> ([], Exp.integer typ Z.zero)
        | Pointer _ -> ([], Exp.null)
        | Array _ | Tuple _ | Struct _ ->
            ([], Exp.splat typ (Exp.integer Typ.byt Z.zero))
        | _ -> fail "ConstantAggregateZero of type %a" Typ.pp typ () )
    | ConstantVector | ConstantArray ->
        let typ = xlate_type x (Llvm.type_of llv) in
        let len = Llvm.num_operands llv in
        let pre, args = xlate_values x len (Llvm.operand llv) in
        (pre, Exp.record typ args)
    | ConstantDataVector ->
        let typ = xlate_type x (Llvm.type_of llv) in
        let len = Llvm.vector_size (Llvm.type_of llv) in
        let pre, args = xlate_values x len (Llvm.const_element llv) in
        (pre, Exp.record typ args)
    | ConstantDataArray ->
        let typ = xlate_type x (Llvm.type_of llv) in
        let len = Llvm.array_length (Llvm.type_of llv) in
        let pre, args = xlate_values x len (Llvm.const_element llv) in
        (pre, Exp.record typ args)
    | ConstantStruct ->
        let typ = xlate_type x (Llvm.type_of llv) in
        let len = Llvm.num_operands llv in
        let pre, args = xlate_values x len (Llvm.operand llv) in
        (pre, Exp.record typ args)
    | BlockAddress ->
        let parent, _ = find_name (Llvm.operand llv 0) in
        let name, _ = find_name (Llvm.operand llv 1) in
        ([], Exp.label ~parent ~name)
    | UndefValue ->
        let llt = Llvm.type_of llv in
        let typ = xlate_type x llt in
        if not (Typ.is_sized typ) then
          todo "types with undetermined size: %a" pp_lltype llt () ;
        let name = Printf.sprintf "undef_%i" !undef_count in
        let loc = Loc.none in
        let id = get_id name in
        let reg = Reg.mk typ id name in
        let msg = Llvm.string_of_llvalue llv in
        ([Inst.nondet ~reg:(Some reg) ~msg ~loc], Exp.reg reg)
    | Instruction
        ( ( Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP
          | FPTrunc | FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast
          | Add | FAdd | Sub | FSub | Mul | FMul | UDiv | SDiv | FDiv | URem
          | SRem | FRem | Shl | LShr | AShr | And | Or | Xor | ICmp | FCmp
          | Select | GetElementPtr | ExtractElement | InsertElement
          | ShuffleVector | ExtractValue | InsertValue ) as opcode ) ->
        if inline || should_inline llv then xlate_opcode x llv opcode
        else ([], Exp.reg (xlate_name x llv))
    | ConstantExpr -> xlate_opcode x llv (Llvm.constexpr_opcode llv)
    | GlobalIFunc -> todo "ifuncs: %a" pp_llvalue llv ()
    | Instruction (CatchPad | CleanupPad | CatchSwitch) ->
        todo "windows exception handling: %a" pp_llvalue llv ()
    | Instruction
        ( Invalid | Ret | Br | Switch | IndirectBr | Invalid2 | Unreachable
        | Store | UserOp1 | UserOp2 | Fence | AtomicCmpXchg | AtomicRMW
        | Resume | CleanupRet | CatchRet )
     |NullValue | BasicBlock | InlineAsm | MDNode | MDString ->
        fail "xlate_value: %a" pp_llvalue llv ()
  in
  ValTbl.find_or_add memo_value (inline, llv) ~default:(fun () ->
      [%Trace.call fun {pf} -> pf "@ %a" pp_llvalue llv]
      ;
      xlate_value_ llv
      |>
      [%Trace.retn fun {pf} -> pf "%a" pp_prefix_exp] )

and xlate_opcode : x -> Llvm.llvalue -> Llvm.Opcode.t -> Inst.t list * Exp.t
    =
 fun x llv opcode ->
  [%Trace.call fun {pf} -> pf "@ %a" pp_llvalue llv]
  ;
  let xlate_rand i = xlate_value x (Llvm.operand llv i) in
  let typ = lazy (xlate_type x (Llvm.type_of llv)) in
  let convert opcode =
    let dst = Lazy.force typ in
    let rand = Llvm.operand llv 0 in
    let src = xlate_type x (Llvm.type_of rand) in
    let pre, arg = xlate_value x rand in
    ( pre
    , match (opcode : Llvm.Opcode.t) with
      | Trunc -> Exp.signed (Typ.bit_size_of dst) arg ~to_:dst
      | SExt -> Exp.signed (Typ.bit_size_of src) arg ~to_:dst
      | ZExt -> Exp.unsigned (Typ.bit_size_of src) arg ~to_:dst
      | (BitCast | AddrSpaceCast) when Typ.equal dst src -> arg
      | FPToUI | FPToSI | UIToFP | SIToFP | FPTrunc | FPExt | PtrToInt
       |IntToPtr | BitCast | AddrSpaceCast ->
          Exp.convert src ~to_:dst arg
      | _ -> fail "convert: %a" pp_llvalue llv () )
  in
  let binary (mk : ?typ:_ -> _) =
    if Poly.equal (Llvm.classify_type (Llvm.type_of llv)) Vector then
      todo "vector operations: %a" pp_llvalue llv () ;
    let typ = xlate_type x (Llvm.type_of (Llvm.operand llv 0)) in
    let pre_0, arg_0 = xlate_rand 0 in
    let pre_1, arg_1 = xlate_rand 1 in
    (pre_0 @ pre_1, mk ~typ arg_0 arg_1)
  in
  let unordered_or mk =
    binary (fun ?typ e f ->
        Exp.or_ ~typ:Typ.bool (Exp.uno ?typ e f) (mk ?typ e f) )
  in
  ( match opcode with
  | Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP | FPTrunc
   |FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast ->
      convert opcode
  | ICmp -> (
    match Option.get_exn (Llvm.icmp_predicate llv) with
    | Eq -> binary Exp.eq
    | Ne -> binary Exp.dq
    | Sgt -> binary Exp.gt
    | Sge -> binary Exp.ge
    | Slt -> binary Exp.lt
    | Sle -> binary Exp.le
    | Ugt -> binary Exp.ugt
    | Uge -> binary Exp.uge
    | Ult -> binary Exp.ult
    | Ule -> binary Exp.ule )
  | FCmp -> (
    match Llvm.fcmp_predicate llv with
    | None | Some False -> binary (fun ?typ:_ _ _ -> Exp.false_)
    | Some Oeq -> binary Exp.eq
    | Some Ogt -> binary Exp.gt
    | Some Oge -> binary Exp.ge
    | Some Olt -> binary Exp.lt
    | Some Ole -> binary Exp.le
    | Some One -> binary Exp.dq
    | Some Ord -> binary Exp.ord
    | Some Uno -> binary Exp.uno
    | Some Ueq -> unordered_or Exp.eq
    | Some Ugt -> unordered_or Exp.gt
    | Some Uge -> unordered_or Exp.ge
    | Some Ult -> unordered_or Exp.lt
    | Some Ule -> unordered_or Exp.le
    | Some Une -> unordered_or Exp.dq
    | Some True -> binary (fun ?typ:_ _ _ -> Exp.true_) )
  | Add | FAdd -> binary Exp.add
  | Sub | FSub -> binary Exp.sub
  | Mul | FMul -> binary Exp.mul
  | SDiv | FDiv -> binary Exp.div
  | UDiv -> binary Exp.udiv
  | SRem | FRem -> binary Exp.rem
  | URem -> binary Exp.urem
  | Shl -> binary Exp.shl
  | LShr -> binary Exp.lshr
  | AShr -> binary Exp.ashr
  | And -> binary Exp.and_
  | Or -> binary Exp.or_
  | Xor -> binary Exp.xor
  | Select ->
      let typ = xlate_type x (Llvm.type_of (Llvm.operand llv 1)) in
      let pre_0, cnd = xlate_rand 0 in
      let pre_1, thn = xlate_rand 1 in
      let pre_2, els = xlate_rand 2 in
      (pre_0 @ pre_1 @ pre_2, Exp.conditional ~typ ~cnd ~thn ~els)
  | ExtractElement | InsertElement -> (
      let typ =
        let lltyp = Llvm.type_of (Llvm.operand llv 0) in
        let llelt = Llvm.element_type lltyp in
        let elt = xlate_type x llelt in
        let len = Llvm.vector_size llelt in
        let byts = size_of x lltyp in
        let bits = bit_size_of x lltyp in
        Typ.array ~elt ~len ~bits ~byts
      in
      let idx i =
        match xlate_rand i with
        | pre, Integer {data} -> (pre, Z.to_int data)
        | _ -> todo "vector operations: %a" pp_llvalue llv ()
      in
      let pre_0, rcd = xlate_rand 0 in
      match opcode with
      | ExtractElement ->
          let pre_1, idx_1 = idx 1 in
          (pre_0 @ pre_1, Exp.select typ rcd idx_1)
      | InsertElement ->
          let pre_1, elt = xlate_rand 1 in
          let pre_2, idx_2 = idx 2 in
          (pre_0 @ pre_1 @ pre_2, Exp.update typ ~rcd idx_2 ~elt)
      | _ -> assert false )
  | ExtractValue | InsertValue ->
      let pre_0, agg = xlate_rand 0 in
      let typ = xlate_type x (Llvm.type_of (Llvm.operand llv 0)) in
      let indices = Llvm.indices llv in
      let num = Array.length indices in
      let rec xlate_indices pre0 i rcd typ =
        let rcd_i, typ_i, upd =
          match (typ : Typ.t) with
          | Tuple {elts} | Struct {elts} ->
              ( Exp.select typ rcd indices.(i)
              , snd (IArray.get elts indices.(i))
              , Exp.update typ ~rcd indices.(i) )
          | Array {elt} ->
              ( Exp.select typ rcd indices.(i)
              , elt
              , Exp.update typ ~rcd indices.(i) )
          | _ -> fail "xlate_value: %a" pp_llvalue llv ()
        in
        let update_or_return elt ret =
          match[@warning "p"] opcode with
          | InsertValue ->
              let pre, elt = Lazy.force elt in
              (pre0 @ pre, upd ~elt)
          | ExtractValue -> (pre0, ret)
        in
        if i < num - 1 then
          let pre, elt = xlate_indices pre0 (i + 1) rcd_i typ_i in
          update_or_return (lazy (pre, elt)) elt
        else
          let pre_elt = lazy (xlate_rand 1) in
          update_or_return pre_elt rcd_i
      in
      xlate_indices pre_0 0 agg typ
  | GetElementPtr ->
      if Poly.equal (Llvm.classify_type (Llvm.type_of llv)) Vector then
        todo "vector operations: %a" pp_llvalue llv () ;
      let len = Llvm.num_operands llv in
      assert (len > 0 || invalid_llvm (Llvm.string_of_llvalue llv)) ;
      if len = 1 then convert BitCast
      else
        let rec xlate_indices i =
          [%Trace.call fun {pf} ->
            pf "@ %i %a" i pp_llvalue (Llvm.operand llv i)]
          ;
          let pre_i, arg_i = xlate_rand i in
          let idx =
            convert_to_siz
              (xlate_type x (Llvm.type_of (Llvm.operand llv i)))
              arg_i
          in
          ( if i = 1 then
            let pre_0, base = xlate_rand 0 in
            let lltyp = Llvm.type_of (Llvm.operand llv 0) in
            let llelt =
              match Llvm.classify_type lltyp with
              | Pointer -> Llvm.element_type lltyp
              | _ -> fail "xlate_opcode: %i %a" i pp_llvalue llv ()
            in
            (* translate [gep t*, iN M] as [gep [1 x t]*, iN M] *)
            ((pre_0 @ pre_i, ptr_idx x ~ptr:base ~idx ~llelt), llelt)
          else
            let (pre_i1, ptr), lltyp = xlate_indices (i - 1) in
            match Llvm.classify_type lltyp with
            | Array | Vector ->
                let llelt = Llvm.element_type lltyp in
                ((pre_i1 @ pre_i, ptr_idx x ~ptr ~idx ~llelt), llelt)
            | Struct ->
                let fld =
                  match
                    Option.bind ~f:Int64.unsigned_to_int
                      (Llvm.int64_of_const (Llvm.operand llv i))
                  with
                  | Some n -> n
                  | None -> fail "xlate_opcode: %i %a" i pp_llvalue llv ()
                in
                let llelt = (Llvm.struct_element_types lltyp).(fld) in
                ((pre_i1 @ pre_i, ptr_fld x ~ptr ~fld ~lltyp), llelt)
            | _ -> fail "xlate_opcode: %i %a" i pp_llvalue llv () )
          |>
          [%Trace.retn fun {pf} (pre_exp, llt) ->
            pf "%a %a" pp_prefix_exp pre_exp pp_lltype llt]
        in
        fst (xlate_indices (len - 1))
  | ShuffleVector -> (
      (* translate shufflevector <N x t> %x, _, <N x i32> zeroinitializer to
         %x *)
      let exp = xlate_value x (Llvm.operand llv 0) in
      let exp_typ = xlate_type x (Llvm.type_of (Llvm.operand llv 0)) in
      let llmask = Llvm.operand llv 2 in
      let mask_typ = xlate_type x (Llvm.type_of llmask) in
      match (exp_typ, mask_typ) with
      | Array {len= m}, Array {len= n} when m = n && Llvm.is_null llmask ->
          exp
      | _ -> todo "vector operations: %a" pp_llvalue llv () )
  | Invalid | Ret | Br | Switch | IndirectBr | Invoke | Invalid2
   |Unreachable | Alloca | Load | Store | PHI | Call | UserOp1 | UserOp2
   |Fence | AtomicCmpXchg | AtomicRMW | Resume | LandingPad | CleanupRet
   |CatchRet | CatchPad | CleanupPad | CatchSwitch | VAArg ->
      fail "xlate_opcode: %a" pp_llvalue llv () )
  |>
  [%Trace.retn fun {pf} -> pf "%a" pp_prefix_exp]

and xlate_global : x -> Llvm.llvalue -> GlobalDefn.t =
 fun x llg ->
  GlobTbl.find_or_add memo_global llg ~default:(fun () ->
      [%Trace.call fun {pf} -> pf "@ %a" pp_llvalue llg]
      ;
      let g =
        Global.mk (xlate_type x (Llvm.type_of llg)) (fst (find_name llg))
      in
      let loc = find_loc llg in
      (* add to tbl without initializer in case of recursive occurrences in
         its own initializer *)
      GlobTbl.set memo_global ~key:llg ~data:(GlobalDefn.mk g loc) ;
      let init =
        match Llvm.classify_value llg with
        | GlobalVariable ->
            Option.map (Llvm.global_initializer llg) ~f:(fun llv ->
                let pre, init = xlate_value x llv in
                (* Nondet insts to set up globals can be dropped to simply
                   leave the undef regs unconstrained. Other insts to set up
                   globals are currently not supported *)
                let is_nondet = function Nondet _ -> true | _ -> false in
                if not (List.for_all ~f:is_nondet pre) then
                  todo "global initializer instructions" () ;
                init )
        | _ -> None
      in
      GlobalDefn.mk ?init g loc
      |>
      [%Trace.retn fun {pf} -> pf "%a" GlobalDefn.pp] )

type pop_thunk = Loc.t -> Llair.inst list

let pop_stack_frame_of_function :
    x -> Llvm.llvalue -> Llvm.llbasicblock -> pop_thunk =
 fun x func entry_blk ->
  let append_stack_regs blk regs =
    Llvm.fold_right_instrs
      (fun instr regs ->
        match Llvm.instr_opcode instr with
        | Alloca -> xlate_name x instr :: regs
        | _ -> regs )
      blk regs
  in
  let entry_regs = append_stack_regs entry_blk [] in
  Llvm.iter_blocks
    (fun blk ->
      if not (Poly.equal entry_blk blk) then
        Llvm.iter_instrs
          (fun instr ->
            match Llvm.instr_opcode instr with
            | Alloca ->
                warn "stack allocation after function entry:@ %a" Loc.pp
                  (find_loc instr) ()
            | _ -> () )
          blk )
    func ;
  let pop retn_loc =
    List.map entry_regs ~f:(fun reg ->
        Inst.free ~ptr:(Exp.reg reg) ~loc:retn_loc )
  in
  pop

(** construct the types involved in landingpads: i32, std::type_info*, and
    __cxa_exception *)
let landingpad_typs : x -> Llvm.llvalue -> Typ.t * Typ.t * Llvm.lltype =
 fun x instr ->
  let llt = Llvm.type_of instr in
  let i32 = i32 x in
  if
    not
      ( Poly.(Llvm.classify_type llt = Struct)
      &&
      let llelts = Llvm.struct_element_types llt in
      Array.length llelts = 2
      && Poly.(llelts.(0) = Llvm.pointer_type (Llvm.i8_type x.llcontext))
      && Poly.(llelts.(1) = Llvm.i32_type x.llcontext) )
  then
    todo "landingpad of type other than {i8*, i32}: %a" pp_llvalue instr () ;
  let llcontext =
    Llvm.(
      module_context (global_parent (block_parent (instr_parent instr))))
  in
  let llpi8 = Llvm.(pointer_type (integer_type llcontext 8)) in
  let ti = Llvm.(named_struct_type llcontext "class.std::type_info") in
  let tip = Llvm.pointer_type ti in
  let void = Llvm.void_type llcontext in
  let dtor = Llvm.(pointer_type (function_type void [|llpi8|])) in
  let cxa_exception = Llvm.struct_type llcontext [|tip; dtor|] in
  (i32, xlate_type x tip, cxa_exception)

let exception_typs =
  let pi8 = Typ.pointer ~elt:Typ.byt in
  let i32 = Typ.integer ~bits:32 ~byts:4 in
  let exc =
    Typ.tuple (IArray.of_array [|(0, pi8); (8, i32)|]) ~bits:96 ~byts:12
  in
  (pi8, i32, exc)

(** Translate a control transfer from instruction [instr] to block [dst] to
    a jump, if necessary by extending [blocks] with a trampoline containing
    the PHIs of [dst] translated to a move. *)
let xlate_jump :
       x
    -> ?reg_exps:(Reg.t * (Inst.t list * Exp.t)) list
    -> Llvm.llvalue
    -> Llvm.llbasicblock
    -> Loc.t
    -> Llair.block list
    -> Inst.t list * Llair.jump * Llair.block list =
 fun x ?(reg_exps = []) instr dst loc blocks ->
  let src = Llvm.instr_parent instr in
  let rec xlate_jump_ reg_exps (pos : _ Llvm.llpos) =
    match pos with
    | Before dst_instr -> (
      match Llvm.instr_opcode dst_instr with
      | PHI ->
          let reg_exp =
            List.find_map_exn (Llvm.incoming dst_instr)
              ~f:(fun (arg, pred) ->
                if Poly.equal pred src then
                  Some (xlate_name x dst_instr, xlate_value x arg)
                else None )
          in
          xlate_jump_ (reg_exp :: reg_exps) (Llvm.instr_succ dst_instr)
      | _ -> reg_exps )
    | At_end blk -> fail "xlate_jump: %a" pp_llblock blk ()
  in
  let dst_lbl = label_of_block dst in
  let jmp = Jump.mk dst_lbl in
  match xlate_jump_ reg_exps (Llvm.instr_begin dst) with
  | [] -> ([], jmp, blocks)
  | rev_reg_pre_exps ->
      let rev_reg_exps, rev_pre =
        List.fold_map rev_reg_pre_exps []
          ~f:(fun (reg, (pre, exp)) rev_pre ->
            ((reg, exp), List.rev_append pre rev_pre) )
      in
      let mov =
        Inst.move ~reg_exps:(IArray.of_list_rev rev_reg_exps) ~loc
      in
      let src_lbl = label_of_block (Llvm.instr_parent instr) in
      let lbl = src_lbl ^ ".jmp." ^ dst_lbl in
      let blk =
        Block.mk ~lbl
          ~cmnd:(IArray.of_array [|mov|])
          ~term:(Term.goto ~dst:jmp ~loc)
      in
      let blocks =
        match List.find blocks ~f:(fun b -> String.equal lbl b.lbl) with
        | None -> blk :: blocks
        | Some blk0 ->
            assert (Block.equal blk0 blk) ;
            blocks
      in
      (List.rev rev_pre, Jump.mk lbl, blocks)

(** An LLVM instruction is translated to a sequence of LLAIR instructions
    and a terminator, plus some additional blocks to which it may refer
    (that is, essentially a function body). These are needed since LLVM and
    LLAIR blocks are not in 1:1 correspondence. *)
type code = Llair.inst list * Llair.term * Llair.block list

let pp_code fs (insts, term, blocks) =
  Format.fprintf fs "@[<hv>@,@[%a%t@]%t@[<hv>%a@]@]" (List.pp "@ " Inst.pp)
    insts
    (fun fs ->
      match term with
      | Unreachable -> ()
      | _ ->
          Format.fprintf fs "%t%a"
            (fun fs ->
              if List.is_empty insts then () else Format.fprintf fs "@ " )
            Term.pp term )
    (fun fs -> if List.is_empty blocks then () else Format.fprintf fs "@\n")
    (List.pp "@ " Block.pp) blocks

module StringS = HashSet.Make (String)

let ignored_callees = StringS.create 0

let xlate_size_of x llv =
  Exp.integer Typ.siz (Z.of_int (size_of x (Llvm.type_of llv)))

let norm_callee llfunc =
  match Llvm.classify_value llfunc with
  | Function | Instruction _ | InlineAsm | Argument -> llfunc
  | ConstantExpr -> (
    match Llvm.constexpr_opcode llfunc with
    | BitCast -> Llvm.operand llfunc 0
    | _ -> todo "callee kind %a" pp_llvalue llfunc () )
  | _ -> todo "callee kind %a" pp_llvalue llfunc ()

let num_actuals instr lltyp llfunc =
  assert (Poly.(Llvm.classify_type lltyp = Pointer)) ;
  if not (Llvm.is_var_arg (Llvm.element_type lltyp)) then
    Llvm.num_arg_operands instr
  else
    let fname = Llvm.value_name llfunc in
    if StringS.add ignored_callees fname && not (Llvm.is_declaration llfunc)
    then
      warn "ignoring variable arguments to variadic function: %s" fname () ;
    let llelt = Llvm.element_type lltyp in
    ( match Llvm.classify_type llelt with
    | Function -> ()
    | _ ->
        fail "called function not of function type: %a" pp_llvalue instr ()
    ) ;
    Array.length (Llvm.param_types llelt)

let xlate_intrinsic_inst emit_inst x name_segs instr num_actuals loc =
  let emit_inst ?prefix inst = Some (emit_inst ?prefix inst) in
  match name_segs with
  | ["__llair_choice"] ->
      let reg = xlate_name x instr in
      let msg = "__llair_choice" in
      emit_inst (Inst.nondet ~reg:(Some reg) ~msg ~loc)
  | ["__llair_alloc" (* void* __llair_alloc(unsigned size) *)] ->
      let reg = xlate_name x instr in
      let num_operand = Llvm.operand instr 0 in
      let prefix, arg = xlate_value x num_operand in
      let num =
        convert_to_siz (xlate_type x (Llvm.type_of num_operand)) arg
      in
      let len = 1 in
      emit_inst ~prefix (Inst.alloc ~reg ~num ~len ~loc)
  | ["_Znam" (* operator new[](unsigned long) *)]
   |["_Znwm" (* operator new(size_t num) *)]
   |[ "_ZnwmSt11align_val_t"
      (* operator new(unsigned long, std::align_val_t) *) ]
    when num_actuals > 0 ->
      let reg = xlate_name x instr in
      let prefix, num = xlate_value x (Llvm.operand instr 0) in
      let len = size_of x (Llvm.type_of instr) in
      emit_inst ~prefix (Inst.alloc ~reg ~num ~len ~loc)
  | ["_ZdlPv" (* operator delete(void* ptr) *)]
   |[ "_ZdlPvSt11align_val_t"
      (* operator delete(void* ptr, std::align_val_t) *) ]
   |[ "_ZdlPvmSt11align_val_t"
      (* operator delete(void* ptr, unsigned long, std::align_val_t) *) ]
   |["free" (* void free(void* ptr) *)] ->
      let prefix, ptr = xlate_value x (Llvm.operand instr 0) in
      emit_inst ~prefix (Inst.free ~ptr ~loc)
  | ["abort"] | ["llvm"; "trap"] -> emit_inst (Inst.abort ~loc)
  | [iname] | "llvm" :: iname :: _ -> (
    match Intrinsic.of_name iname with
    | Some name ->
        let reg = xlate_name_opt x instr in
        let xlate_arg i pre =
          let pre_i, arg_i = xlate_value x (Llvm.operand instr i) in
          (arg_i, pre_i @ pre)
        in
        let prefix, args =
          Iter.fold_map ~f:xlate_arg Iter.(0 -- (num_actuals - 1)) []
        in
        let args = IArray.of_iter args in
        emit_inst ~prefix (Inst.intrinsic ~reg ~name ~args ~loc)
    | None -> None )
  | _ -> None

let calls_to_backpatch = ref []

let term_call x llcallee ~typ ~actuals ~areturn ~return ~throw ~loc =
  match Llvm.classify_value llcallee with
  | Function ->
      let name = Llvm.value_name llcallee in
      let call, backpatch =
        Term.call ~name ~typ ~actuals ~areturn ~return ~throw ~loc
      in
      calls_to_backpatch :=
        (llcallee, typ, backpatch) :: !calls_to_backpatch ;
      ([], call)
  | _ ->
      let prefix, callee = xlate_value x llcallee in
      (prefix, Term.icall ~callee ~typ ~actuals ~areturn ~return ~throw ~loc)

let xlate_instr :
       pop_thunk
    -> x
    -> Llvm.llvalue
    -> ((Llair.inst list * Llair.term -> code) -> code)
    -> code =
 fun pop x instr continue ->
  [%Trace.call fun {pf} -> pf "@ %a" pp_llvalue instr]
  ;
  let continue insts_term_to_code =
    [%Trace.retn
      fun {pf} () ->
        pf "%a" pp_code (insts_term_to_code ([], Term.unreachable))]
      () ;
    continue insts_term_to_code
  in
  let nop () = continue (fun (insts, term) -> (insts, term, [])) in
  let emit_inst ?(prefix = []) inst =
    continue (fun (insts, term) -> (prefix @ (inst :: insts), term, []))
  in
  let emit_term ?(prefix = []) ?(blocks = []) term =
    [%Trace.retn fun {pf} () -> pf "%a" pp_code (prefix, term, blocks)] () ;
    (prefix, term, blocks)
  in
  let loc = find_loc instr in
  let inline_or_move xlate =
    if should_inline instr then nop ()
    else
      let reg = xlate_name x instr in
      let prefix, exp = xlate instr in
      let reg_exps = IArray.of_array [|(reg, exp)|] in
      emit_inst ~prefix (Inst.move ~reg_exps ~loc)
  in
  let opcode = Llvm.instr_opcode instr in
  match opcode with
  | Load ->
      let reg = xlate_name x instr in
      let len = xlate_size_of x instr in
      let prefix, ptr = xlate_value x (Llvm.operand instr 0) in
      emit_inst ~prefix (Inst.load ~reg ~ptr ~len ~loc)
  | Store ->
      let rand0 = Llvm.operand instr 0 in
      let pre0, exp = xlate_value x rand0 in
      let len = xlate_size_of x rand0 in
      let pre1, ptr = xlate_value x (Llvm.operand instr 1) in
      emit_inst ~prefix:(pre0 @ pre1) (Inst.store ~ptr ~exp ~len ~loc)
  | Alloca ->
      let reg = xlate_name x instr in
      let num_elts = Llvm.operand instr 0 in
      let prefix, num = xlate_value x num_elts in
      let num = convert_to_siz (xlate_type x (Llvm.type_of num_elts)) num in
      assert (Poly.(Llvm.classify_type (Llvm.type_of instr) = Pointer)) ;
      let len = size_of x (Llvm.element_type (Llvm.type_of instr)) in
      emit_inst ~prefix (Inst.alloc ~reg ~num ~len ~loc)
  | Call -> (
      let llcallee = Llvm.operand instr (Llvm.num_operands instr - 1) in
      let lltyp = Llvm.type_of llcallee in
      let llcallee = norm_callee llcallee in
      let num_actuals = num_actuals instr lltyp llcallee in
      let fname = Llvm.value_name llcallee in
      let name_segs = String.split_on_char fname ~by:'.' in
      let skip msg =
        if StringS.add ignored_callees fname then
          warn "ignoring uninterpreted %s %s" msg fname () ;
        let reg = xlate_name_opt x instr in
        emit_inst (Inst.nondet ~reg ~msg:fname ~loc)
      in
      (* intrinsics *)
      match xlate_intrinsic_exp fname with
      | Some intrinsic -> inline_or_move (intrinsic x)
      | None -> (
        match
          xlate_intrinsic_inst emit_inst x name_segs instr num_actuals loc
        with
        | Some code -> code
        | None -> (
          match name_segs with
          | ["__llair_throw"] ->
              let pre, exc = xlate_value x (Llvm.operand instr 0) in
              emit_term ~prefix:(pop loc @ pre) (Term.throw ~exc ~loc)
          | ["__llair_unreachable"] -> emit_term Term.unreachable
          (* dropped / handled elsewhere *)
          | ["llvm"; "dbg"; ("declare" | "value")]
           |"llvm" :: ("lifetime" | "invariant") :: ("start" | "end") :: _
            ->
              nop ()
          (* unimplemented *)
          | ["llvm"; ("stacksave" | "stackrestore")] ->
              skip "dynamic stack deallocation"
          | "llvm" :: "coro" :: _ ->
              todo "coroutines:@ %a" pp_llvalue instr ()
          | "llvm" :: "experimental" :: "gc" :: "statepoint" :: _ ->
              todo "statepoints:@ %a" pp_llvalue instr ()
          | ["llvm"; ("va_start" | "va_copy" | "va_end")] ->
              skip "variadic function intrinsic"
          | "llvm" :: _ -> skip "intrinsic"
          | _ when Poly.equal (Llvm.classify_value llcallee) InlineAsm ->
              skip "inline asm"
          (* general function call that may not throw *)
          | _ ->
              let typ = xlate_type x lltyp in
              let name, _ = find_name instr in
              let lbl = name ^ ".ret" in
              let pre_1, actuals =
                xlate_values x num_actuals (Llvm.operand instr)
              in
              let areturn = xlate_name_opt x instr in
              let return = Jump.mk lbl in
              let pre_0, call =
                term_call x llcallee ~typ ~actuals ~areturn ~return
                  ~throw:None ~loc
              in
              continue (fun (insts, term) ->
                  let cmnd = IArray.of_list insts in
                  (pre_0 @ pre_1, call, [Block.mk ~lbl ~cmnd ~term]) ) ) ) )
  | Invoke -> (
      let llcallee = Llvm.operand instr (Llvm.num_operands instr - 3) in
      let lltyp = Llvm.type_of llcallee in
      let llcallee = norm_callee llcallee in
      let num_actuals = num_actuals instr lltyp llcallee in
      let fname = Llvm.value_name llcallee in
      let name_segs = String.split_on_char fname ~by:'.' in
      let return_blk = Llvm.get_normal_dest instr in
      let unwind_blk = Llvm.get_unwind_dest instr in
      (* intrinsics *)
      match xlate_intrinsic_exp fname with
      | Some _ ->
          (* instr will be translated to an exp by xlate_value, so only need
             to wire up control flow here *)
          let prefix, dst, blocks = xlate_jump x instr return_blk loc [] in
          emit_term ~prefix (Term.goto ~dst ~loc) ~blocks
      | None -> (
          let k ?prefix:(pre_inst = []) inst =
            let pre_jump, dst, blocks =
              xlate_jump x instr return_blk loc []
            in
            let prefix = pre_inst @ (inst :: pre_jump) in
            emit_term ~prefix (Term.goto ~dst ~loc) ~blocks
          in
          match
            xlate_intrinsic_inst k x name_segs instr num_actuals loc
          with
          | Some code -> code
          | None -> (
            match name_segs with
            | ["__llair_throw"] ->
                let prefix, dst, blocks =
                  xlate_jump x instr unwind_blk loc []
                in
                emit_term ~prefix (Term.goto ~dst ~loc) ~blocks
            | ["__llair_unreachable"] -> emit_term Term.unreachable
            (* unimplemented *)
            | "llvm" :: "experimental" :: "gc" :: "statepoint" :: _ ->
                todo "statepoints:@ %a" pp_llvalue instr ()
            | _ when Poly.equal (Llvm.classify_value llcallee) InlineAsm ->
                todo "inline asm: @ %a" pp_llvalue instr ()
            (* general function call that may throw *)
            | _ ->
                let typ = xlate_type x lltyp in
                let pre_1, actuals =
                  xlate_values x num_actuals (Llvm.operand instr)
                in
                let areturn = xlate_name_opt x instr in
                let pre_2, return, blocks =
                  xlate_jump x instr return_blk loc []
                in
                let pre_3, throw, blocks =
                  xlate_jump x instr unwind_blk loc blocks
                in
                let pre_0, call =
                  term_call x llcallee ~typ ~actuals ~areturn ~return
                    ~throw:(Some throw) ~loc
                in
                let prefix = List.concat [pre_0; pre_1; pre_2; pre_3] in
                emit_term ~prefix call ~blocks ) ) )
  | Ret ->
      let pre, exp =
        if Llvm.num_operands instr = 0 then ([], None)
        else
          let pre, arg = xlate_value x (Llvm.operand instr 0) in
          (pre, Some arg)
      in
      emit_term ~prefix:(pop loc @ pre) (Term.return ~exp ~loc)
  | Br -> (
    match Option.get_exn (Llvm.get_branch instr) with
    | `Unconditional blk ->
        let prefix, dst, blocks = xlate_jump x instr blk loc [] in
        emit_term ~prefix (Term.goto ~dst ~loc) ~blocks
    | `Conditional (cnd, thn, els) ->
        let pre_c, key = xlate_value x cnd in
        let pre_t, thn, blocks = xlate_jump x instr thn loc [] in
        let pre_e, els, blocks = xlate_jump x instr els loc blocks in
        emit_term
          ~prefix:(List.concat [pre_c; pre_t; pre_e])
          (Term.branch ~key ~nzero:thn ~zero:els ~loc)
          ~blocks )
  | Switch ->
      let pre_k, key = xlate_value x (Llvm.operand instr 0) in
      let pre_t, cases, blocks =
        let num_cases = (Llvm.num_operands instr / 2) - 1 in
        let rec xlate_cases i blocks =
          if i <= num_cases then
            let idx = Llvm.operand instr (2 * i) in
            let blk =
              Llvm.block_of_value (Llvm.operand instr ((2 * i) + 1))
            in
            let pre_i, num = xlate_value x idx in
            let pre_j, jmp, blocks = xlate_jump x instr blk loc blocks in
            let pre, rest, blocks = xlate_cases (i + 1) blocks in
            (List.concat [pre_i; pre_j; pre], (num, jmp) :: rest, blocks)
          else ([], [], blocks)
        in
        xlate_cases 1 []
      in
      let tbl = IArray.of_list cases in
      let blk = Llvm.block_of_value (Llvm.operand instr 1) in
      let pre_e, els, blocks = xlate_jump x instr blk loc blocks in
      emit_term
        ~prefix:(List.concat [pre_k; pre_t; pre_e])
        (Term.switch ~key ~tbl ~els ~loc)
        ~blocks
  | IndirectBr ->
      let pre_0, ptr = xlate_value x (Llvm.operand instr 0) in
      let num_dests = Llvm.num_operands instr - 1 in
      let pre, lldests, blocks =
        let rec dests i blocks =
          if i <= num_dests then
            let v = Llvm.operand instr i in
            let blk = Llvm.block_of_value v in
            let pre_j, jmp, blocks = xlate_jump x instr blk loc blocks in
            let pre, rest, blocks = dests (i + 1) blocks in
            (pre_j @ pre, jmp :: rest, blocks)
          else ([], [], blocks)
        in
        dests 1 []
      in
      let tbl = IArray.of_list lldests in
      emit_term ~prefix:(pre_0 @ pre) (Term.iswitch ~ptr ~tbl ~loc) ~blocks
  | LandingPad ->
      (* Translate the landingpad clauses to code to load the type_info from
         the thrown exception, and test the type_info against the clauses,
         eventually jumping to the handler code following the landingpad,
         passing a value for the selector which the handler code tests to
         e.g. either cleanup or rethrow. *)
      let name, _ = find_name instr in
      let i32, tip, cxa_exception = landingpad_typs x instr in
      let pi8, _, exc_typ = exception_typs in
      let exc_name = name ^ ".exc" in
      let exc_id = get_id exc_name in
      let exc = Exp.reg (Reg.mk pi8 exc_id exc_name) in
      let ti_name = name ^ ".ti" in
      let ti_id = get_id ti_name in
      let ti = Reg.mk tip ti_id ti_name in
      (* std::type_info* ti = ((__cxa_exception* )exc - 1)->exceptionType *)
      let load_ti =
        let typ = cxa_exception in
        (* field number of the exceptionType member of __cxa_exception *)
        let fld = 0 in
        (* index from exc that points to header *)
        let idx = Exp.integer Typ.siz Z.minus_one in
        let ptr =
          ptr_fld x
            ~ptr:(ptr_idx x ~ptr:exc ~idx ~llelt:typ)
            ~fld ~lltyp:typ
        in
        let len = Exp.integer Typ.siz (Z.of_int (size_of x typ)) in
        Inst.load ~reg:ti ~ptr ~len ~loc
      in
      let ti = Exp.reg ti in
      let typeid = xlate_llvm_eh_typeid_for x tip ti in
      let lbl = name ^ ".unwind" in
      let reg = xlate_name x instr in
      let jump_unwind i sel rev_blocks =
        let exp = Exp.record exc_typ (IArray.of_array [|exc; sel|]) in
        let mov =
          Inst.move ~reg_exps:(IArray.of_array [|(reg, exp)|]) ~loc
        in
        let lbl_i = lbl ^ "." ^ Int.to_string i in
        let blk =
          Block.mk ~lbl:lbl_i
            ~cmnd:(IArray.of_array [|mov|])
            ~term:(Term.goto ~dst:(Jump.mk lbl) ~loc)
        in
        (Jump.mk lbl_i, blk :: rev_blocks)
      in
      let goto_unwind i sel blocks =
        let dst, blocks = jump_unwind i sel blocks in
        ([], Term.goto ~dst ~loc, blocks)
      in
      let pre, term_unwind, rev_blocks =
        if Llvm.is_cleanup instr then
          goto_unwind 0 (Exp.integer i32 Z.zero) []
        else
          let num_clauses = Llvm.num_operands instr in
          let lbl i = name ^ "." ^ Int.to_string i in
          let jump i = Jump.mk (lbl i) in
          let block i term =
            Block.mk ~lbl:(lbl i) ~cmnd:IArray.empty ~term
          in
          let match_filter i rev_blocks =
            jump_unwind i
              (Exp.sub ~typ:i32 (Exp.integer i32 Z.zero) typeid)
              rev_blocks
          in
          let xlate_clause i rev_blocks =
            let clause = Llvm.operand instr i in
            let num_tis = Llvm.num_operands clause in
            if num_tis = 0 then
              let dst, rev_blocks = match_filter i rev_blocks in
              ([], Term.goto ~dst ~loc, rev_blocks)
            else
              match Llvm.classify_type (Llvm.type_of clause) with
              | Array (* filter *) -> (
                match Llvm.classify_value clause with
                | ConstantArray ->
                    let rec xlate_filter i =
                      let preI, tiI =
                        xlate_value x (Llvm.operand clause i)
                      in
                      if i < num_tis - 1 then
                        let pre, dqs = xlate_filter (i + 1) in
                        ( preI @ pre
                        , Exp.and_ ~typ:Typ.bool (Exp.dq ~typ:tip tiI ti)
                            dqs )
                      else (preI, Exp.dq ~typ:tip tiI ti)
                    in
                    let pre, key = xlate_filter 0 in
                    let nzero, rev_blocks = match_filter i rev_blocks in
                    ( pre
                    , Term.branch ~loc ~key ~nzero ~zero:(jump (i + 1))
                    , rev_blocks )
                | _ -> fail "xlate_instr: %a" pp_llvalue instr () )
              | _ (* catch *) ->
                  let typ = xlate_type x (Llvm.type_of clause) in
                  let pre, clause = xlate_value x clause in
                  let key =
                    Exp.or_ ~typ:Typ.bool
                      (Exp.eq ~typ clause Exp.null)
                      (Exp.eq ~typ clause ti)
                  in
                  let nzero, rev_blocks = jump_unwind i typeid rev_blocks in
                  ( pre
                  , Term.branch ~loc ~key ~nzero ~zero:(jump (i + 1))
                  , rev_blocks )
          in
          let rec rev_blocks i z =
            if i < num_clauses then
              let pre_i, term, z = xlate_clause i z in
              let pre, blks = rev_blocks (i + 1) (block i term :: z) in
              (pre_i @ pre, blks)
            else ([], block i Term.unreachable :: z)
          in
          let pre1, rev_blks = rev_blocks 1 [] in
          let pre0, term, blks = xlate_clause 0 rev_blks in
          (pre0 @ pre1, term, blks)
      in
      continue (fun (insts, term) ->
          ( load_ti :: pre
          , term_unwind
          , List.rev_append rev_blocks
              [Block.mk ~lbl ~cmnd:(IArray.of_list insts) ~term] ) )
  | Resume ->
      let llrcd = Llvm.operand instr 0 in
      let typ = xlate_type x (Llvm.type_of llrcd) in
      let pre, rcd = xlate_value x llrcd in
      let exc = Exp.select typ rcd 0 in
      emit_term ~prefix:(pop loc @ pre) (Term.throw ~exc ~loc)
  | Unreachable -> emit_term Term.unreachable
  | Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP | FPTrunc
   |FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast | Add | FAdd
   |Sub | FSub | Mul | FMul | UDiv | SDiv | FDiv | URem | SRem | FRem
   |Shl | LShr | AShr | And | Or | Xor | ICmp | FCmp | Select
   |GetElementPtr | ExtractElement | InsertElement | ShuffleVector
   |ExtractValue | InsertValue ->
      inline_or_move (xlate_value ~inline:true x)
  | VAArg ->
      let reg = xlate_name_opt x instr in
      warn "variadic function argument: %a" Loc.pp loc () ;
      emit_inst (Inst.nondet ~reg ~msg:"vaarg" ~loc)
  | CleanupRet | CatchRet | CatchPad | CleanupPad | CatchSwitch ->
      todo "windows exception handling: %a" pp_llvalue instr ()
  | Fence | AtomicCmpXchg | AtomicRMW ->
      fail "xlate_instr: %a" pp_llvalue instr ()
  | PHI | Invalid | Invalid2 | UserOp1 | UserOp2 -> assert false

let rec xlate_instrs : pop_thunk -> x -> _ Llvm.llpos -> code =
 fun pop x -> function
  | Before instrI ->
      xlate_instr pop x instrI (fun xlate_instrI ->
          let instrJ = Llvm.instr_succ instrI in
          let instsJ, termJ, blocksJN = xlate_instrs pop x instrJ in
          let instsI, termI, blocksI = xlate_instrI (instsJ, termJ) in
          (instsI, termI, blocksI @ blocksJN) )
  | At_end blk -> fail "xlate_instrs: %a" pp_llblock blk ()

let skip_phis : Llvm.llbasicblock -> _ Llvm.llpos =
 fun blk ->
  let rec skip_phis_ (pos : _ Llvm.llpos) =
    match pos with
    | Before instr -> (
      match Llvm.instr_opcode instr with
      | PHI -> skip_phis_ (Llvm.instr_succ instr)
      | _ -> pos )
    | _ -> pos
  in
  skip_phis_ (Llvm.instr_begin blk)

let xlate_block : pop_thunk -> x -> Llvm.llbasicblock -> Llair.block list =
 fun pop x blk ->
  [%Trace.call fun {pf} -> pf "@ %a" pp_llblock blk]
  ;
  let lbl = label_of_block blk in
  let pos = skip_phis blk in
  let insts, term, blocks = xlate_instrs pop x pos in
  Block.mk ~lbl ~cmnd:(IArray.of_list insts) ~term :: blocks
  |>
  [%Trace.retn fun {pf} blocks -> pf "%s" (List.hd_exn blocks).lbl]

let report_undefined func name =
  if Option.is_some (Llvm.use_begin func) then
    [%Trace.info "undefined function: %a" Function.pp name]

let xlate_function_decl x llfunc typ k =
  let loc = find_loc llfunc in
  let name = Function.mk typ (fst (find_name llfunc)) in
  let formals =
    Iter.from_iter (fun f -> Llvm.iter_params f llfunc)
    |> Iter.map ~f:(xlate_name x)
    |> IArray.of_iter
  in
  let freturn =
    match typ with
    | Pointer {elt= Function {return= Some typ; _}} ->
        let name = "freturn" in
        let id = get_id name in
        Some (Reg.mk typ id name)
    | _ -> None
  in
  let _, _, exc_typ = exception_typs in
  let exc_name = "fthrow" in
  let exc_id = get_id exc_name in
  let fthrow = Reg.mk exc_typ exc_id exc_name in
  k ~name ~formals ~freturn ~fthrow ~loc

let xlate_function : x -> Llvm.llvalue -> Llair.func =
 fun x llf ->
  [%Trace.call fun {pf} -> pf "@ %a" pp_llvalue llf]
  ;
  undef_count := 0 ;
  let typ = xlate_type x (Llvm.type_of llf) in
  xlate_function_decl x llf typ
  @@ fun ~name ~formals ~freturn ~fthrow ~loc ->
  ( match Llvm.block_begin llf with
  | Before entry_blk ->
      let pop = pop_stack_frame_of_function x llf entry_blk in
      let[@warning "p"] (entry_block :: entry_blocks) =
        xlate_block pop x entry_blk
      in
      let entry =
        let {Llair.lbl; cmnd; term} = entry_block in
        Block.mk ~lbl ~cmnd ~term
      in
      let cfg =
        let rec trav_blocks rev_cfg prev =
          match Llvm.block_succ prev with
          | Before blk ->
              trav_blocks
                (List.rev_append (xlate_block pop x blk) rev_cfg)
                blk
          | At_end _ -> IArray.of_list_rev rev_cfg
        in
        trav_blocks (List.rev entry_blocks) entry_blk
      in
      Func.mk ~name ~formals ~freturn ~fthrow ~entry ~cfg ~loc
  | At_end _ ->
      report_undefined llf name ;
      Func.mk_undefined ~name ~formals ~freturn ~fthrow ~loc )
  |>
  [%Trace.retn fun {pf} -> pf "@\n%a" Func.pp]

let backpatch_calls x func_tbl =
  List.iter !calls_to_backpatch ~f:(fun (llfunc, typ, backpatch) ->
      match LlvalueTbl.find func_tbl llfunc with
      | Some callee -> backpatch ~callee
      | None ->
          xlate_function_decl x llfunc typ
          @@ fun ~name ~formals ~freturn ~fthrow ~loc ->
          let callee =
            Func.mk_undefined ~name ~formals ~freturn ~fthrow ~loc
          in
          backpatch ~callee )

let transform ~internalize : Llvm.llmodule -> unit =
 fun llmodule ->
  let pm = Llvm.PassManager.create () in
  let entry_points = Config.find_list "entry-points" in
  if internalize then
    Llvm_ipo.add_internalize_predicate pm (fun fn ->
        List.exists entry_points ~f:(String.equal fn) ) ;
  Llvm_ipo.add_global_dce pm ;
  Llvm_ipo.add_global_optimizer pm ;
  Llvm_ipo.add_merge_functions pm ;
  Llvm_ipo.add_constant_merge pm ;
  Llvm_ipo.add_argument_promotion pm ;
  Llvm_ipo.add_ipsccp pm ;
  Llvm_scalar_opts.add_memory_to_register_promotion pm ;
  Llvm_scalar_opts.add_dce pm ;
  Llvm_ipo.add_global_dce pm ;
  Llvm_ipo.add_dead_arg_elimination pm ;
  Llvm_scalar_opts.add_lower_atomic pm ;
  Llvm_scalar_opts.add_scalar_repl_aggregation pm ;
  Llvm_scalar_opts.add_scalarizer pm ;
  Llvm_scalar_opts.add_unify_function_exit_nodes pm ;
  Llvm_scalar_opts.add_cfg_simplification pm ;
  Llvm.PassManager.run_module llmodule pm |> (ignore : bool -> _) ;
  Llvm.PassManager.dispose pm

let read_and_parse llcontext bc_file =
  [%Trace.call fun {pf} -> pf "@ %s" bc_file]
  ;
  let llmemorybuffer =
    try Llvm.MemoryBuffer.of_file bc_file
    with Llvm.IoError msg -> fail "%s: %s" bc_file msg ()
  in
  ( try Llvm_irreader.parse_ir llcontext llmemorybuffer
    with Llvm_irreader.Error msg -> invalid_llvm msg )
  |>
  [%Trace.retn fun {pf} _ -> pf ""]

let link_in : Llvm.llcontext -> Llvm.lllinker -> string -> unit =
 fun llcontext link_ctx bc_file ->
  Llvm_linker.link_in link_ctx (read_and_parse llcontext bc_file)

let check_datalayout llcontext lldatalayout =
  let check_size llt typ =
    let llsiz =
      match
        Int64.unsigned_to_int
          (Llvm_target.DataLayout.abi_size llt lldatalayout)
      with
      | Some n -> n
      | None -> fail "type size too large: %a" pp_lltype llt ()
    in
    let siz = Typ.size_of typ in
    if llsiz != siz then
      todo "size_of %a = %i != %i" Typ.pp typ llsiz siz ()
  in
  check_size (Llvm.i1_type llcontext) Typ.bool ;
  check_size (Llvm.i8_type llcontext) Typ.byt ;
  check_size (Llvm.i32_type llcontext) Typ.int ;
  check_size (Llvm.i64_type llcontext) Typ.siz ;
  check_size
    (Llvm_target.DataLayout.intptr_type llcontext lldatalayout)
    Typ.ptr

(* The Llvm.dispose_ functions free memory allocated off the OCaml heap. The
   OCaml heap can later grow into that memory once it is freed. There are
   naked pointers into the LLVM-allocated memory from various values
   returned from Llvm functions. If the GC scans a block with such a naked
   pointer after the heap has grown into the memory previously allocated by
   Llvm, the GC will follow the pointer expecting a well-formed OCaml value,
   and likely segfault. Therefore it is necessary to ensure that all the
   values containing naked pointers are dead (which is the reason for
   clearing the hashtbls) and then collected (which is the reason for the
   Gc.full_major) before freeing the memory with Llvm.dispose_module and
   Llvm.dispose_context. *)
let cleanup llmodule llcontext =
  SymTbl.clear sym_tbl ;
  ScopeTbl.clear scope_tbl ;
  LltypeTbl.clear anon_struct_name ;
  LltypeTbl.clear memo_type ;
  GlobTbl.clear memo_global ;
  ValTbl.clear memo_value ;
  calls_to_backpatch := [] ;
  Gc.full_major () ;
  Llvm.dispose_module llmodule ;
  Llvm.dispose_context llcontext

let translate ~models ~fuzzer ~internalize : string list -> Llair.program =
 fun inputs ->
  [%Trace.call fun {pf} ->
    pf "@ %a" (List.pp "@ " Format.pp_print_string) inputs]
  ;
  Llvm.install_fatal_error_handler invalid_llvm ;
  let llcontext = Llvm.global_context () in
  let input, inputs = List.pop_exn inputs in
  let llmodule = read_and_parse llcontext input in
  let link_ctx = Llvm_linker.get_linker llmodule in
  List.iter ~f:(link_in llcontext link_ctx) inputs ;
  let link_model_file name =
    Llvm_linker.link_in link_ctx
      (Llvm_irreader.parse_ir llcontext
         (Llvm.MemoryBuffer.of_string (Option.get_exn (Model.read name))))
  in
  if models then link_model_file "/cxxabi.bc" ;
  if fuzzer then link_model_file "/lib_fuzzer_main.bc" ;
  Llvm_linker.linker_dispose link_ctx ;
  assert (
    Llvm_analysis.verify_module llmodule |> Option.for_all ~f:invalid_llvm
  ) ;
  transform ~internalize llmodule ;
  scan_names_and_locs llmodule ;
  let lldatalayout =
    Llvm_target.DataLayout.of_string (Llvm.data_layout llmodule)
  in
  check_datalayout llcontext lldatalayout ;
  let x = {llcontext; lldatalayout} in
  let globals =
    Llvm.fold_left_globals
      (fun globals llg ->
        if
          Poly.equal (Llvm.linkage llg) Appending
          && Llvm.(array_length (element_type (type_of llg))) = 0
        then globals
        else xlate_global x llg :: globals )
      [] llmodule
  in
  let func_tbl : Func.t LlvalueTbl.t = LlvalueTbl.create () in
  let functions =
    Llvm.fold_left_functions
      (fun functions llf ->
        let name = Llvm.value_name llf in
        if
          String.prefix name ~pre:"__llair_"
          || String.prefix name ~pre:"llvm."
        then functions
        else
          let func = xlate_function x llf in
          LlvalueTbl.set func_tbl ~key:llf ~data:func ;
          func :: functions )
      [] llmodule
  in
  backpatch_calls x func_tbl ;
  cleanup llmodule llcontext ;
  Llair.Program.mk ~globals ~functions
  |>
  [%Trace.retn fun {pf} _ ->
    pf "number of globals %d, number of functions %d" (List.length globals)
      (List.length functions)]
