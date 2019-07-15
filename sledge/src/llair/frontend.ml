(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Translate LLVM to LLAIR *)

let pp_lltype fs t = Format.pp_print_string fs (Llvm.string_of_lltype t)

(* WARNING: SLOW on instructions and functions *)
let pp_llvalue fs t = Format.pp_print_string fs (Llvm.string_of_llvalue t)

let pp_llblock fs t =
  Format.pp_print_string fs (Llvm.string_of_llvalue (Llvm.value_of_block t))

type lllinkage = [%import: Llvm.Linkage.t] [@@deriving sexp]
type llopcode = [%import: Llvm.Opcode.t] [@@deriving sexp]

type llvaluekind = [%import: (Llvm.ValueKind.t[@with Opcode.t := llopcode])]
[@@deriving sexp]

let _pp_lllinkage fs l = Sexp.pp_hum fs (sexp_of_lllinkage l)
let _pp_llopcode fs l = Sexp.pp_hum fs (sexp_of_llopcode l)
let pp_llvaluekind fs l = Sexp.pp_hum fs (sexp_of_llvaluekind l)

exception Invalid_llvm of string

let invalid_llvm : string -> 'a =
 fun msg ->
  let first_line =
    Option.value_map ~default:msg ~f:(String.prefix msg)
      (String.index msg '\n')
  in
  Format.printf "@\n%s@\n" msg ;
  raise (Invalid_llvm first_line)

(* gather names and debug locations *)

let sym_tbl : (Llvm.llvalue, string * Loc.t) Hashtbl.t =
  Hashtbl.Poly.create ~size:4_194_304 ()

let scope_tbl :
    ( [`Fun of Llvm.llvalue | `Mod of Llvm.llmodule]
    , int ref * (string, int) Hashtbl.t )
    Hashtbl.t =
  Hashtbl.Poly.create ~size:32_768 ()

let ( (scan_names_and_locs : Llvm.llmodule -> unit)
    , (find_name : Llvm.llvalue -> string)
    , (find_loc : Llvm.llvalue -> Loc.t) ) =
  let loc_of_global g =
    Loc.mk
      ?dir:(Llvm.get_debug_loc_directory g)
      ?file:(Llvm.get_debug_loc_filename g)
      ~line:(Llvm.get_debug_loc_line g)
      ?col:None
  in
  let loc_of_function f =
    Loc.mk
      ?dir:(Llvm.get_debug_loc_directory f)
      ?file:(Llvm.get_debug_loc_filename f)
      ~line:(Llvm.get_debug_loc_line f)
      ?col:None
  in
  let loc_of_instr i =
    Loc.mk
      ?dir:(Llvm.get_debug_loc_directory i)
      ?file:(Llvm.get_debug_loc_filename i)
      ~line:(Llvm.get_debug_loc_line i)
      ~col:(Llvm.get_debug_loc_column i)
  in
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
          warn "Unexpected type %a of llv, might crash: %a" pp_llvaluekind
            (Llvm.classify_value llv) pp_llvalue llv () ;
          Some (`Mod (Llvm.global_parent llv))
    in
    match maybe_scope with
    | None -> ()
    | Some scope ->
        let next, void_tbl =
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
                  fname ^ ".void"
              | Some count ->
                  Hashtbl.set void_tbl ~key:fname ~data:(count + 1) ;
                  String.concat_array
                    [|fname; ".void."; Int.to_string count|] )
          | _ -> (
            match Llvm.value_name llv with
            | "" ->
                (* anonymous values take the next SSA name *)
                let name = !next in
                next := name + 1 ;
                Int.to_string name
            | name -> (
              match Int.of_string name with
              | _ ->
                  (* escape to avoid clash with names of anonymous values *)
                  String.concat_array [|"\""; name; "\""|]
              | exception _ -> name ) )
        in
        Hashtbl.set sym_tbl ~key:llv ~data:(name, loc)
  in
  let scan_names_and_locs m =
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
    Llvm.iter_functions scan_function m
  in
  let find_name v = fst (Hashtbl.find_exn sym_tbl v) in
  let find_loc v = snd (Hashtbl.find_exn sym_tbl v) in
  (scan_names_and_locs, find_name, find_loc)

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

type x =
  { llcontext: Llvm.llcontext
  ; llmodule: Llvm.llmodule
  ; lldatalayout: Llvm_target.DataLayout.t }

let ptr_siz : x -> int =
 fun x -> Llvm_target.DataLayout.pointer_size x.lldatalayout

let size_of : x -> Llvm.lltype -> int =
 fun x llt ->
  if Llvm.type_is_sized llt then
    match
      Int64.to_int (Llvm_target.DataLayout.abi_size llt x.lldatalayout)
    with
    | Some n -> n
    | None -> fail "size_of: %a" pp_lltype llt ()
  else todo "types with undetermined size: %a" pp_lltype llt ()

let memo_type : (Llvm.lltype, Typ.t) Hashtbl.t = Hashtbl.Poly.create ()

let rec xlate_type : x -> Llvm.lltype -> Typ.t =
 fun x llt ->
  let xlate_type_ llt =
    match Llvm.classify_type llt with
    | Half -> Typ.float ~bits:16 ~enc:`IEEE
    | Float -> Typ.float ~bits:32 ~enc:`IEEE
    | Double -> Typ.float ~bits:64 ~enc:`IEEE
    | X86fp80 -> Typ.float ~bits:80 ~enc:`Extended
    | Fp128 -> Typ.float ~bits:128 ~enc:`IEEE
    | Ppc_fp128 -> Typ.float ~bits:128 ~enc:`Pair
    | Integer -> Typ.integer ~bits:(Llvm.integer_bitwidth llt)
    | X86_mmx -> Typ.integer ~bits:64
    | Function ->
        let return = xlate_type_opt x (Llvm.return_type llt) in
        let llargs = Llvm.param_types llt in
        let len = Array.length llargs in
        let args = Vector.init len ~f:(fun i -> xlate_type x llargs.(i)) in
        Typ.function_ ~return ~args
    | Pointer ->
        if size_of x llt <> ptr_siz x then
          todo "non-integral pointer types: %a" pp_lltype llt () ;
        let elt = xlate_type x (Llvm.element_type llt) in
        Typ.pointer ~elt
    | Vector ->
        let elt = xlate_type x (Llvm.element_type llt) in
        let len = Llvm.vector_size llt in
        Typ.array ~elt ~len
    | Array ->
        let elt = xlate_type x (Llvm.element_type llt) in
        let len = Llvm.array_length llt in
        Typ.array ~elt ~len
    | Struct ->
        let llelts = Llvm.struct_element_types llt in
        let len = Array.length llelts in
        let packed = Llvm.is_packed llt in
        if Llvm.is_literal llt then
          let elts =
            Vector.map ~f:(xlate_type x) (Vector.of_array llelts)
          in
          Typ.tuple elts ~packed
        else
          let name = struct_name llt in
          if Llvm.is_opaque llt then Typ.opaque ~name
          else
            let elts =
              Vector.init len ~f:(fun i -> lazy (xlate_type x llelts.(i)))
            in
            Typ.struct_ ~name elts ~packed
    | Token -> Typ.opaque ~name:"token"
    | Void | Label | Metadata -> assert false
  in
  Hashtbl.find_or_add memo_type llt ~default:(fun () ->
      [%Trace.call fun {pf} -> pf "%a" pp_lltype llt]
      ;
      xlate_type_ llt
      |>
      [%Trace.retn fun {pf} -> pf "%a" Typ.pp_defn] )

and xlate_type_opt : x -> Llvm.lltype -> Typ.t option =
 fun x llt ->
  match Llvm.classify_type llt with
  | Void -> None
  | _ -> Some (xlate_type x llt)

let i32 x = xlate_type x (Llvm.i32_type x.llcontext)

let suffix_after_last_space : string -> string =
 fun str -> String.drop_prefix str (String.rindex_exn str ' ' + 1)

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
  Exp.integer data typ

let xlate_float : Llvm.llvalue -> Exp.t =
 fun llv ->
  let data = suffix_after_last_space (Llvm.string_of_llvalue llv) in
  Exp.float data

let xlate_name : Llvm.llvalue -> Var.t =
 fun llv -> Var.program (find_name llv)

let xlate_name_opt : Llvm.llvalue -> Var.t option =
 fun instr ->
  match Llvm.classify_type (Llvm.type_of instr) with
  | Void -> None
  | _ -> Some (xlate_name instr)

let memo_value : (Llvm.llvalue, Exp.t) Hashtbl.t = Hashtbl.Poly.create ()

let memo_global : (Llvm.llvalue, Global.t) Hashtbl.t =
  Hashtbl.Poly.create ()

module Llvalue = struct
  type t = Llvm.llvalue

  let hash = Hashtbl.hash
  let compare = Poly.compare
  let sexp_of_t llv = Sexp.Atom (Llvm.string_of_llvalue llv)
end

let struct_rec = Staged.unstage (Exp.struct_rec (module Llvalue))

let ptr_fld x ~ptr ~fld ~lltyp =
  let offset =
    Llvm_target.DataLayout.offset_of_element lltyp fld x.lldatalayout
  in
  Exp.add Typ.ptr ptr (Exp.integer (Z.of_int64 offset) Typ.siz)

let ptr_idx x ~ptr ~idx ~llelt =
  let stride = Llvm_target.DataLayout.abi_size llelt x.lldatalayout in
  Exp.add Typ.ptr ptr
    (Exp.mul Typ.siz (Exp.integer (Z.of_int64 stride) Typ.siz) idx)

let xlate_llvm_eh_typeid_for : x -> Typ.t -> Exp.t -> Exp.t =
 fun x typ arg -> Exp.convert ~dst:(i32 x) ~src:typ arg

let rec xlate_intrinsic_exp : string -> (x -> Llvm.llvalue -> Exp.t) option
    =
 fun name ->
  match name with
  | "llvm.eh.typeid.for" ->
      Some
        (fun x llv ->
          let rand = Llvm.operand llv 0 in
          let arg = xlate_value x rand in
          let src = xlate_type x (Llvm.type_of rand) in
          xlate_llvm_eh_typeid_for x src arg )
  | _ -> None

and xlate_value : x -> Llvm.llvalue -> Exp.t =
 fun x llv ->
  let xlate_value_ llv =
    match Llvm.classify_value llv with
    | Instruction Call -> (
        let func = Llvm.operand llv (Llvm.num_arg_operands llv) in
        let fname = Llvm.value_name func in
        match xlate_intrinsic_exp fname with
        | Some intrinsic -> intrinsic x llv
        | None -> Exp.var (xlate_name llv) )
    | Instruction (Invoke | Alloca | Load | PHI | LandingPad | VAArg)
     |Argument ->
        Exp.var (xlate_name llv)
    | Function | GlobalVariable -> Exp.var (xlate_global x llv).var
    | GlobalAlias -> xlate_value x (Llvm.operand llv 0)
    | ConstantInt -> xlate_int x llv
    | ConstantFP -> xlate_float llv
    | ConstantPointerNull | ConstantAggregateZero -> Exp.null
    | ConstantVector | ConstantArray ->
        let len = Llvm.num_operands llv in
        let f i = xlate_value x (Llvm.operand llv i) in
        Exp.record (List.init len ~f)
    | ConstantDataVector ->
        let len = Llvm.vector_size (Llvm.type_of llv) in
        let f i = xlate_value x (Llvm.const_element llv i) in
        Exp.record (List.init len ~f)
    | ConstantDataArray ->
        let len = Llvm.array_length (Llvm.type_of llv) in
        let f i = xlate_value x (Llvm.const_element llv i) in
        Exp.record (List.init len ~f)
    | ConstantStruct ->
        let is_recursive =
          Llvm.fold_left_uses
            (fun b use -> b || llv == Llvm.used_value use)
            false llv
        in
        if is_recursive then
          let elt_thks =
            Vector.init (Llvm.num_operands llv) ~f:(fun i ->
                lazy (xlate_value x (Llvm.operand llv i)) )
          in
          struct_rec ~id:llv elt_thks
        else
          Exp.record
            (List.init (Llvm.num_operands llv) ~f:(fun i ->
                 xlate_value x (Llvm.operand llv i) ))
    | BlockAddress ->
        let parent = find_name (Llvm.operand llv 0) in
        let name = find_name (Llvm.operand llv 1) in
        Exp.label ~parent ~name
    | UndefValue -> Exp.nondet (Llvm.string_of_llvalue llv)
    | Instruction
        ( ( Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP
          | FPTrunc | FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast
          | Add | FAdd | Sub | FSub | Mul | FMul | UDiv | SDiv | FDiv | URem
          | SRem | FRem | Shl | LShr | AShr | And | Or | Xor | ICmp | FCmp
          | Select | GetElementPtr | ExtractElement | InsertElement
          | ExtractValue | InsertValue | ShuffleVector ) as opcode ) ->
        xlate_opcode x llv opcode
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
  Hashtbl.find_or_add memo_value llv ~default:(fun () ->
      [%Trace.call fun {pf} -> pf "%a" pp_llvalue llv]
      ;
      xlate_value_ llv
      |>
      [%Trace.retn fun {pf} exp -> pf "%a" Exp.pp exp] )

and xlate_opcode : x -> Llvm.llvalue -> Llvm.Opcode.t -> Exp.t =
 fun x llv opcode ->
  [%Trace.call fun {pf} -> pf "%a" pp_llvalue llv]
  ;
  let xlate_rand i = xlate_value x (Llvm.operand llv i) in
  let typ = lazy (xlate_type x (Llvm.type_of llv)) in
  let cast () = xlate_rand 0 in
  let convert signed =
    let rand = Llvm.operand llv 0 in
    let dst = Lazy.force typ in
    let src = xlate_type x (Llvm.type_of rand) in
    let arg = xlate_value x rand in
    Exp.convert ~signed ~dst ~src arg
  in
  let binary mk =
    if Poly.equal (Llvm.classify_type (Llvm.type_of llv)) Vector then
      todo "vector operations: %a" pp_llvalue llv () ;
    mk (xlate_rand 0) (xlate_rand 1)
  in
  let unordered_or mk =
    binary (fun x y -> Exp.or_ (Exp.uno x y) (mk x y))
  in
  ( match opcode with
  | AddrSpaceCast | BitCast -> cast ()
  | Trunc | ZExt | FPToUI | UIToFP | FPTrunc | FPExt | PtrToInt | IntToPtr
    ->
      convert false
  | SExt | FPToSI | SIToFP -> convert true
  | ICmp -> (
    match Option.value_exn (Llvm.icmp_predicate llv) with
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
    | None | Some False -> binary (fun _ _ -> Exp.bool false)
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
    | Some True -> binary (fun _ _ -> Exp.bool true) )
  | Add | FAdd -> binary (Exp.add (Lazy.force typ))
  | Sub | FSub -> binary (Exp.sub (Lazy.force typ))
  | Mul | FMul -> binary (Exp.mul (Lazy.force typ))
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
      Exp.conditional ~cnd:(xlate_rand 0) ~thn:(xlate_rand 1)
        ~els:(xlate_rand 2)
  | ExtractElement -> Exp.select ~rcd:(xlate_rand 0) ~idx:(xlate_rand 1)
  | InsertElement ->
      Exp.update ~rcd:(xlate_rand 0) ~elt:(xlate_rand 1) ~idx:(xlate_rand 2)
  | ExtractValue | InsertValue ->
      let agg = xlate_rand 0 in
      let typ = xlate_type x (Llvm.type_of (Llvm.operand llv 0)) in
      let indices = Llvm.indices llv in
      let num = Array.length indices in
      let rec xlate_indices i rcd =
        let rcd_i, upd =
          match typ with
          | Tuple _ | Struct _ ->
              let idx = Exp.integer (Z.of_int indices.(i)) Typ.siz in
              (Exp.select ~rcd ~idx, Exp.update ~rcd ~idx)
          | Array _ ->
              let idx = Exp.integer (Z.of_int indices.(i)) Typ.siz in
              (Exp.select ~rcd ~idx, Exp.update ~rcd ~idx)
          | _ -> fail "xlate_value: %a" pp_llvalue llv ()
        in
        let update_or_return elt ret =
          match[@warning "p"] opcode with
          | InsertValue -> upd ~elt:(Lazy.force elt)
          | ExtractValue -> ret
        in
        if i < num - 1 then
          let elt = xlate_indices (i + 1) rcd_i in
          update_or_return (lazy elt) elt
        else
          let elt = lazy (xlate_rand 1) in
          update_or_return elt rcd_i
      in
      xlate_indices 0 agg
  | GetElementPtr ->
      if Poly.equal (Llvm.classify_type (Llvm.type_of llv)) Vector then
        todo "vector operations: %a" pp_llvalue llv () ;
      let len = Llvm.num_operands llv in
      if len <= 1 then cast ()
      else
        let rec xlate_indices i =
          [%Trace.call fun {pf} ->
            pf "%i %a" i pp_llvalue (Llvm.operand llv i)]
          ;
          let idx =
            Exp.convert ~dst:Typ.siz
              ~src:(xlate_type x (Llvm.type_of (Llvm.operand llv i)))
              (xlate_rand i)
          in
          ( if i = 1 then
            let base = xlate_rand 0 in
            let lltyp = Llvm.type_of (Llvm.operand llv 0) in
            let llelt =
              match Llvm.classify_type lltyp with
              | Pointer -> Llvm.element_type lltyp
              | _ -> fail "xlate_opcode: %i %a" i pp_llvalue llv ()
            in
            (* translate [gep t*, iN M] as [gep [1 x t]*, iN M] *)
            (ptr_idx x ~ptr:base ~idx ~llelt, llelt)
          else
            let ptr, lltyp = xlate_indices (i - 1) in
            match Llvm.classify_type lltyp with
            | Array | Vector ->
                let llelt = Llvm.element_type lltyp in
                (ptr_idx x ~ptr ~idx ~llelt, llelt)
            | Struct ->
                let fld =
                  Option.bind ~f:Int64.to_int
                    (Llvm.int64_of_const (Llvm.operand llv i))
                  |> function
                  | Some n -> n
                  | None -> fail "xlate_opcode: %i %a" i pp_llvalue llv ()
                in
                let llelt = (Llvm.struct_element_types lltyp).(fld) in
                (ptr_fld x ~ptr ~fld ~lltyp, llelt)
            | _ -> fail "xlate_opcode: %i %a" i pp_llvalue llv () )
          |>
          [%Trace.retn fun {pf} (exp, llt) ->
            pf "%a %a" Exp.pp exp pp_lltype llt]
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
      | _ -> fail "xlate_opcode: %a" pp_llvalue llv () )
  | Invalid | Ret | Br | Switch | IndirectBr | Invoke | Invalid2
   |Unreachable | Alloca | Load | Store | PHI | Call | UserOp1 | UserOp2
   |Fence | AtomicCmpXchg | AtomicRMW | Resume | LandingPad | CleanupRet
   |CatchRet | CatchPad | CleanupPad | CatchSwitch | VAArg ->
      fail "xlate_opcode: %a" pp_llvalue llv () )
  |>
  [%Trace.retn fun {pf} exp -> pf "%a" Exp.pp exp]

and xlate_global : x -> Llvm.llvalue -> Global.t =
 fun x llg ->
  Hashtbl.find_or_add memo_global llg ~default:(fun () ->
      [%Trace.call fun {pf} -> pf "%a" pp_llvalue llg]
      ;
      let g = xlate_name llg in
      let llt = Llvm.type_of llg in
      let typ = xlate_type x llt in
      let loc = find_loc llg in
      (* add to tbl without initializer in case of recursive occurrences in
         its own initializer *)
      Hashtbl.set memo_global ~key:llg ~data:(Global.mk g typ loc) ;
      let init =
        match Llvm.classify_value llg with
        | GlobalVariable -> (
          match Llvm.global_initializer llg with
          | Some llinit ->
              let siz = size_of x (Llvm.element_type llt) in
              let init = xlate_value x llinit in
              Some (init, siz)
          | _ -> None )
        | _ -> None
      in
      Global.mk ?init g typ loc
      |>
      [%Trace.retn fun {pf} -> pf "%a" Global.pp_defn] )

type pop_thunk = Loc.t -> Llair.inst list

let pop_stack_frame_of_function :
    Llvm.llvalue -> Llvm.llbasicblock -> pop_thunk =
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
                warn "stack allocation after function entry:@ %a" Loc.pp
                  (find_loc instr) ()
            | _ -> () )
          blk )
    func ;
  let pop retn_loc =
    List.map entry_vars ~f:(fun var ->
        Llair.Inst.free ~ptr:(Exp.var var) ~loc:retn_loc )
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

(** construct the argument of a landingpad block, mainly fix the encoding
    scheme for landingpad instruction name to block arg name *)
let landingpad_arg : Llvm.llvalue -> Var.t =
 fun instr -> Var.program (find_name instr ^ ".exc")

(** [rev_map_phis ~f blk] returns [(retn_arg, rev_args, pos)] by rev_mapping
    over the prefix of [PHI] instructions at the beginning of [blk].
    [retn_arg], if any, is [f] applied to the [PHI] instruction which takes
    the return value of every [Invoke] predecessor of [blk]. [rev_args] is
    the result of applying [f] to each of the other [PHI] instructions.
    [pos] is the instruction iterator position before the first non-[PHI]
    instruction of [blk]. *)
let rev_map_phis :
       f:(Llvm.llvalue -> 'a)
    -> Llvm.llbasicblock
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
            List.fold (Llvm.incoming instr) ~init:(false, true)
              ~f:(fun (has_invoke_pred, is_retn_arg) (arg, pred) ->
                match Llvm.block_terminator pred with
                | Some instr -> (
                  match Llvm.instr_opcode instr with
                  | Invoke when Poly.equal arg instr -> (true, is_retn_arg)
                  | Invoke -> (has_invoke_pred, false)
                  | _ -> (has_invoke_pred, is_retn_arg) )
                | None -> fail "rev_map_phis: %a" pp_llblock blk () )
          in
          if found_invoke_pred && has_invoke_pred then
            (* Supporting multiple PHI instructions that take the return
               values of invoke instructions will require adding trampolines
               for the invoke instructions to return to, that each reorder
               arguments and invoke the translation of this block. *)
            todo "multiple PHI instructions taking invoke return values: %a"
              pp_llblock blk () ;
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
            pp_llblock blk ()
      | _ -> (retn_arg, rev_args, pos) )
    | At_end blk -> fail "rev_map_phis: %a" pp_llblock blk ()
  in
  block_args_ false None [] (Llvm.instr_begin blk)

(** [trampoline_args jump_instr dest_block] is the actual arguments to which
    the translation of [dest_block] should be partially-applied, to yield a
    trampoline accepting the return parameter of the block and then jumping
    with all the args. *)
let trampoline_args : x -> Llvm.llvalue -> Llvm.llbasicblock -> Exp.t list =
 fun x jmp dst ->
  let src = Llvm.instr_parent jmp in
  rev_map_phis dst ~f:(fun instr ->
      List.find_map_exn (Llvm.incoming instr) ~f:(fun (arg, pred) ->
          if Poly.equal pred src then Some (xlate_value x arg) else None )
  )
  |> snd3

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
  && Option.is_none (unique_pred blk)
  && return_formal_is_used instr

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
      (Option.cons instr_arg rev_args, pos)
  | At_end blk -> fail "block_formals: %a" pp_llblock blk ()

(** actual arguments passed by a jump to a block *)
let jump_args : x -> Llvm.llvalue -> Llvm.llbasicblock -> Exp.t list =
 fun x jmp dst ->
  let src = Llvm.instr_parent jmp in
  let retn_arg, rev_args, _ =
    rev_map_phis dst ~f:(fun phi ->
        Option.value_exn
          (List.find_map (Llvm.incoming phi) ~f:(fun (arg, pred) ->
               if Poly.equal pred src then Some (xlate_value x arg)
               else None )) )
  in
  let retn_arg =
    Option.first_some retn_arg
      (Option.map (unique_used_invoke_pred dst) ~f:(fun invoke ->
           Exp.var (xlate_name invoke) ))
  in
  Option.cons retn_arg rev_args

(** An LLVM instruction is translated to a sequence of LLAIR instructions
    and a terminator, plus some additional blocks to which it may refer
    (that is, essentially a function body). These are needed since LLVM and
    LLAIR blocks are not in 1:1 correspondence. *)
type code = Llair.inst list * Llair.term * Llair.block list

let pp_code fs (insts, term, blocks) =
  Format.fprintf fs "@[<hv>@,@[%a%t@]%t@[<hv>%a@]@]"
    (List.pp "@ " Llair.Inst.pp)
    insts
    (fun fs ->
      match term with
      | Llair.Unreachable -> ()
      | _ ->
          Format.fprintf fs "%t%a"
            (fun fs ->
              if List.is_empty insts then () else Format.fprintf fs "@ " )
            Llair.Term.pp term )
    (fun fs -> if List.is_empty blocks then () else Format.fprintf fs "@\n")
    (List.pp "@ " Llair.Block.pp)
    blocks

let rec xlate_func_name x llv =
  match Llvm.classify_value llv with
  | Function -> Exp.var (xlate_name llv)
  | ConstantExpr -> xlate_opcode x llv (Llvm.constexpr_opcode llv)
  | Argument | Instruction _ -> xlate_value x llv
  | GlobalAlias -> xlate_func_name x (Llvm.operand llv 0)
  | GlobalIFunc -> todo "ifunc: %a" pp_llvalue llv ()
  | InlineAsm -> todo "inline asm: %a" pp_llvalue llv ()
  | ConstantPointerNull -> todo "call null: %a" pp_llvalue llv ()
  | _ -> fail "unknown function: %a" pp_llvalue llv ()

let ignored_callees = Hash_set.create (module String)

let xlate_instr :
       pop_thunk
    -> x
    -> Llvm.llvalue
    -> ((Llair.inst list * Llair.term -> code) -> code)
    -> code =
 fun pop x instr continue ->
  [%Trace.call fun {pf} -> pf "%a" pp_llvalue instr]
  ;
  let continue insts_term_to_code =
    [%Trace.retn
      fun {pf} () ->
        pf "%a" pp_code (insts_term_to_code ([], Llair.Term.unreachable))]
      () ;
    continue insts_term_to_code
  in
  let nop () = continue (fun (insts, term) -> (insts, term, [])) in
  let emit_inst inst =
    continue (fun (insts, term) -> (inst :: insts, term, []))
  in
  let emit_term ?(prefix = []) ?(blocks = []) term =
    [%Trace.retn fun {pf} () -> pf "%a" pp_code (prefix, term, blocks)] () ;
    (prefix, term, blocks)
  in
  let name = find_name instr in
  let loc = find_loc instr in
  let opcode = Llvm.instr_opcode instr in
  match opcode with
  | Load ->
      let reg = xlate_name instr in
      let len =
        Exp.integer (Z.of_int (size_of x (Llvm.type_of instr))) Typ.siz
      in
      let ptr = xlate_value x (Llvm.operand instr 0) in
      emit_inst (Llair.Inst.load ~reg ~ptr ~len ~loc)
  | Store ->
      let exp = xlate_value x (Llvm.operand instr 0) in
      let llt = Llvm.type_of (Llvm.operand instr 0) in
      let len = Exp.integer (Z.of_int (size_of x llt)) Typ.siz in
      let ptr = xlate_value x (Llvm.operand instr 1) in
      emit_inst (Llair.Inst.store ~ptr ~exp ~len ~loc)
  | Alloca ->
      let reg = xlate_name instr in
      let rand = Llvm.operand instr 0 in
      let num =
        Exp.convert ~dst:Typ.siz
          ~src:(xlate_type x (Llvm.type_of rand))
          (xlate_value x rand)
      in
      assert (Poly.(Llvm.classify_type (Llvm.type_of instr) = Pointer)) ;
      let llt = Llvm.element_type (Llvm.type_of instr) in
      let len = Exp.integer (Z.of_int (size_of x llt)) Typ.siz in
      emit_inst (Llair.Inst.alloc ~reg ~num ~len ~loc)
  | Call -> (
      let llfunc =
        let maybe_llfunc =
          Llvm.operand instr (Llvm.num_operands instr - 1)
        in
        let llfunc_valuekind = Llvm.classify_value maybe_llfunc in
        match llfunc_valuekind with
        | Function | Instruction _ | InlineAsm | Argument -> maybe_llfunc
        | ConstantExpr -> (
          match Llvm.constexpr_opcode maybe_llfunc with
          | BitCast -> Llvm.operand maybe_llfunc 0
          | IntToPtr -> todo "maybe handle calls with inttoptr" ()
          | _ ->
              fail "Unknown value in a call instruction %a" pp_llvalue
                maybe_llfunc () )
        | _ ->
            fail "Unhandled operand type in a call instruction %a"
              pp_llvaluekind llfunc_valuekind ()
      in
      let lltyp = Llvm.type_of llfunc in
      assert (Poly.(Llvm.classify_type lltyp = Pointer)) ;
      let fname = Llvm.value_name llfunc in
      let skip msg =
        ( match Hash_set.strict_add ignored_callees fname with
        | Ok () -> warn "ignoring uninterpreted %s %s" msg fname ()
        | Error _ -> () ) ;
        let reg = xlate_name_opt instr in
        emit_inst (Llair.Inst.nondet ~reg ~msg:fname ~loc)
      in
      (* intrinsics *)
      match String.split fname ~on:'.' with
      | _ when Option.is_some (xlate_intrinsic_exp fname) -> nop ()
      | ["__llair_throw"] ->
          let exc = xlate_value x (Llvm.operand instr 0) in
          emit_term ~prefix:(pop loc) (Llair.Term.throw ~exc ~loc)
      | ["_Znwm" (* operator new(size_t num) *)]
       |["__llair_alloc" (* void* __llair_alloc(unsigned size) *)]
       |[ "_ZnwmSt11align_val_t"
          (* operator new(unsigned long, std::align_val_t) *) ] ->
          let reg = xlate_name instr in
          let num_operand = Llvm.operand instr 0 in
          let num =
            Exp.convert ~dst:Typ.siz
              (xlate_value x num_operand)
              ~src:(xlate_type x (Llvm.type_of num_operand))
          in
          let llt = Llvm.type_of instr in
          let len = Exp.integer (Z.of_int (size_of x llt)) Typ.siz in
          emit_inst (Llair.Inst.alloc ~reg ~num ~len ~loc)
      | ["_ZdlPv" (* operator delete(void* ptr) *)]
       |[ "_ZdlPvSt11align_val_t"
          (* operator delete(void* ptr, std::align_val_t) *) ]
       |[ "_ZdlPvmSt11align_val_t"
          (* operator delete(void* ptr, unsigned long, std::align_val_t) *)
         ]
       |["free" (* void free(void* ptr) *)] ->
          let ptr = xlate_value x (Llvm.operand instr 0) in
          emit_inst (Llair.Inst.free ~ptr ~loc)
      | "llvm" :: "memset" :: _ ->
          let dst = xlate_value x (Llvm.operand instr 0) in
          let byt = xlate_value x (Llvm.operand instr 1) in
          let len = xlate_value x (Llvm.operand instr 2) in
          emit_inst (Llair.Inst.memset ~dst ~byt ~len ~loc)
      | "llvm" :: "memcpy" :: _ ->
          let dst = xlate_value x (Llvm.operand instr 0) in
          let src = xlate_value x (Llvm.operand instr 1) in
          let len = xlate_value x (Llvm.operand instr 2) in
          emit_inst (Llair.Inst.memcpy ~dst ~src ~len ~loc)
      | "llvm" :: "memmove" :: _ ->
          let dst = xlate_value x (Llvm.operand instr 0) in
          let src = xlate_value x (Llvm.operand instr 1) in
          let len = xlate_value x (Llvm.operand instr 2) in
          emit_inst (Llair.Inst.memmov ~dst ~src ~len ~loc)
      | ["abort"] | ["llvm"; "trap"] -> emit_inst (Llair.Inst.abort ~loc)
      (* dropped / handled elsewhere *)
      | ["llvm"; "dbg"; ("declare" | "value")]
       |"llvm" :: ("lifetime" | "invariant") :: ("start" | "end") :: _ ->
          nop ()
      (* unimplemented *)
      | ["llvm"; ("stacksave" | "stackrestore")] ->
          skip "dynamic stack deallocation"
      | "llvm" :: "coro" :: _ -> todo "coroutines:@ %a" pp_llvalue instr ()
      | "llvm" :: "experimental" :: "gc" :: "statepoint" :: _ ->
          todo "statepoints:@ %a" pp_llvalue instr ()
      | ["llvm"; ("va_start" | "va_copy" | "va_end")] ->
          skip "variadic function intrinsic"
      | "llvm" :: _ -> skip "intrinsic"
      | _ when Poly.equal (Llvm.classify_value llfunc) InlineAsm ->
          skip "inline asm"
      (* general function call that may not throw *)
      | _ ->
          let func = xlate_func_name x llfunc in
          let typ = xlate_type x (Llvm.type_of llfunc) in
          let lbl = name ^ ".ret" in
          let call =
            let args =
              let num_args =
                if not (Llvm.is_var_arg (Llvm.element_type lltyp)) then
                  Llvm.num_arg_operands instr
                else
                  let fname = Llvm.value_name llfunc in
                  ( match Hash_set.strict_add ignored_callees fname with
                  | Ok () when not (Llvm.is_declaration llfunc) ->
                      warn
                        "ignoring variable arguments to variadic function: \
                         %a"
                        Exp.pp func ()
                  | _ -> () ) ;
                  Array.length (Llvm.param_types (Llvm.element_type lltyp))
              in
              List.rev_init num_args ~f:(fun i ->
                  xlate_value x (Llvm.operand instr i) )
            in
            let return = Llair.Jump.mk lbl [] in
            Llair.Term.call ~func ~typ ~args ~loc ~return ~throw:None
              ~ignore_result:false
          in
          let params = Option.to_list (xlate_name_opt instr) in
          continue (fun (insts, term) ->
              let cmnd = Vector.of_list insts in
              ([], call, [Llair.Block.mk ~lbl ~params ~cmnd ~term]) ) )
  | Invoke -> (
      let reg = xlate_name_opt instr in
      let llfunc = Llvm.operand instr (Llvm.num_operands instr - 3) in
      let lltyp = Llvm.type_of llfunc in
      assert (Poly.(Llvm.classify_type lltyp = Pointer)) ;
      let fname = Llvm.value_name llfunc in
      let return_blk = Llvm.get_normal_dest instr in
      let return_dst = label_of_block return_blk in
      let unwind_blk = Llvm.get_unwind_dest instr in
      let unwind_dst = label_of_block unwind_blk in
      let num_args =
        if not (Llvm.is_var_arg (Llvm.element_type lltyp)) then
          Llvm.num_arg_operands instr
        else (
          ( match Hash_set.strict_add ignored_callees fname with
          | Ok () when not (Llvm.is_declaration llfunc) ->
              warn "ignoring variable arguments to variadic function: %a"
                Global.pp (xlate_global x llfunc) ()
          | _ -> () ) ;
          Array.length (Llvm.param_types (Llvm.element_type lltyp)) )
      in
      let args =
        List.rev_init num_args ~f:(fun i ->
            xlate_value x (Llvm.operand instr i) )
      in
      (* intrinsics *)
      match String.split fname ~on:'.' with
      | _ when Option.is_some (xlate_intrinsic_exp fname) ->
          let arg = Option.to_list (Option.map ~f:Exp.var reg) in
          let dst = Llair.Jump.mk return_dst arg in
          emit_term (Llair.Term.goto ~dst ~loc)
      | ["__llair_throw"] ->
          let dst = Llair.Jump.mk unwind_dst args in
          emit_term (Llair.Term.goto ~dst ~loc)
      | ["abort"] ->
          emit_term ~prefix:[Llair.Inst.abort ~loc] Llair.Term.unreachable
      | ["_Znwm" (* operator new(size_t num) *)]
       |[ "_ZnwmSt11align_val_t"
          (* operator new(unsigned long num, std::align_val_t) *) ]
        when num_args > 0 ->
          let reg = xlate_name instr in
          let num = xlate_value x (Llvm.operand instr 0) in
          let llt = Llvm.type_of instr in
          let len = Exp.integer (Z.of_int (size_of x llt)) Typ.siz in
          let args = jump_args x instr return_blk in
          let dst = Llair.Jump.mk return_dst args in
          emit_term
            ~prefix:[Llair.Inst.alloc ~reg ~num ~len ~loc]
            (Llair.Term.goto ~dst ~loc)
      (* unimplemented *)
      | "llvm" :: "experimental" :: "gc" :: "statepoint" :: _ ->
          todo "statepoints:@ %a" pp_llvalue instr ()
      (* general function call that may throw *)
      | _ ->
          let func = xlate_func_name x llfunc in
          let typ = xlate_type x (Llvm.type_of llfunc) in
          let ignore_result =
            match typ with
            | Pointer {elt= Function {return= Some _}} ->
                not (return_formal_is_used instr)
            | _ -> false
          in
          let return, blocks =
            let args = trampoline_args x instr return_blk in
            if not (need_return_trampoline instr return_blk) then
              (Llair.Jump.mk return_dst args, [])
            else
              let lbl = name ^ ".ret" in
              let block =
                let params = [xlate_name instr] in
                let cmnd = Vector.empty in
                let term =
                  let dst = Llair.Jump.mk return_dst args in
                  Llair.Term.goto ~dst ~loc
                in
                Llair.Block.mk ~lbl ~params ~cmnd ~term
              in
              (Llair.Jump.mk lbl [], [block])
          in
          let throw =
            let dst = unwind_dst in
            let args = trampoline_args x instr unwind_blk in
            Some (Llair.Jump.mk dst args)
          in
          emit_term
            (Llair.Term.call ~func ~typ ~args ~loc ~return ~throw
               ~ignore_result)
            ~blocks )
  | Ret ->
      let exp =
        if Llvm.num_operands instr = 0 then None
        else Some (xlate_value x (Llvm.operand instr 0))
      in
      emit_term ~prefix:(pop loc) (Llair.Term.return ~exp ~loc)
  | Br -> (
    match Option.value_exn (Llvm.get_branch instr) with
    | `Unconditional blk ->
        let args = jump_args x instr blk in
        let dst = Llair.Jump.mk (label_of_block blk) args in
        emit_term (Llair.Term.goto ~dst ~loc)
    | `Conditional (cnd, thn, els) ->
        let key = xlate_value x cnd in
        let thn_lbl = label_of_block thn in
        let thn_args = jump_args x instr thn in
        let thn = Llair.Jump.mk thn_lbl thn_args in
        let els_lbl = label_of_block els in
        let els_args = jump_args x instr els in
        let els = Llair.Jump.mk els_lbl els_args in
        emit_term (Llair.Term.branch ~key ~nzero:thn ~zero:els ~loc) )
  | Switch ->
      let key = xlate_value x (Llvm.operand instr 0) in
      let cases =
        let num_cases = (Llvm.num_operands instr / 2) - 1 in
        let rec xlate_cases i =
          if i <= num_cases then
            let idx = Llvm.operand instr (2 * i) in
            let blk =
              Llvm.block_of_value (Llvm.operand instr ((2 * i) + 1))
            in
            let num = xlate_value x idx in
            let dst = label_of_block blk in
            let args = jump_args x instr blk in
            let rest = xlate_cases (i + 1) in
            (num, Llair.Jump.mk dst args) :: rest
          else []
        in
        xlate_cases 1
      in
      let tbl = Vector.of_list cases in
      let blk = Llvm.block_of_value (Llvm.operand instr 1) in
      let dst = label_of_block blk in
      let args = jump_args x instr blk in
      let els = Llair.Jump.mk dst args in
      emit_term (Llair.Term.switch ~key ~tbl ~els ~loc)
  | IndirectBr ->
      let ptr = xlate_value x (Llvm.operand instr 0) in
      let num_dests = Llvm.num_operands instr - 1 in
      let lldests =
        let rec dests i =
          if i <= num_dests then
            let v = Llvm.operand instr i in
            let blk = Llvm.block_of_value v in
            let dst = label_of_block blk in
            let args = jump_args x instr blk in
            let rest = dests (i + 1) in
            Llair.Jump.mk dst args :: rest
          else []
        in
        dests 1
      in
      let tbl = Vector.of_list lldests in
      emit_term (Llair.Term.iswitch ~ptr ~tbl ~loc)
  | LandingPad ->
      (* Translate the landingpad clauses to code to load the type_info from
         the thrown exception, and test the type_info against the clauses,
         eventually jumping to the handler code following the landingpad,
         passing a value for the selector which the handler code tests to
         e.g. either cleanup or rethrow. *)
      let i32, tip, cxa_exception = landingpad_typs x instr in
      let exc = Exp.var (landingpad_arg instr) in
      let ti = Var.program (name ^ ".ti") in
      (* std::type_info* ti = ((__cxa_exception* )exc - 1)->exceptionType *)
      let load_ti =
        let typ = cxa_exception in
        (* field number of the exceptionType member of __cxa_exception *)
        let fld = 0 in
        (* index from exc that points to header *)
        let idx = Exp.integer Z.minus_one Typ.siz in
        let ptr =
          ptr_fld x
            ~ptr:(ptr_idx x ~ptr:exc ~idx ~llelt:typ)
            ~fld ~lltyp:typ
        in
        let len = Exp.integer (Z.of_int (size_of x typ)) Typ.siz in
        Llair.Inst.load ~reg:ti ~ptr ~len ~loc
      in
      let ti = Exp.var ti in
      let typeid = xlate_llvm_eh_typeid_for x tip ti in
      let lbl = name ^ ".unwind" in
      let param = xlate_name instr in
      let params = [param] in
      let jump_unwind sel =
        let dst = lbl in
        let args = [Exp.record [exc; sel]] in
        Llair.Jump.mk dst args
      in
      let goto_unwind sel =
        let dst = jump_unwind sel in
        Llair.Term.goto ~dst ~loc
      in
      let term_unwind, rev_blocks =
        if Llvm.is_cleanup instr then
          (goto_unwind (Exp.integer Z.zero i32), [])
        else
          let num_clauses = Llvm.num_operands instr in
          let lbl i = name ^ "." ^ Int.to_string i in
          let jump i = Llair.Jump.mk (lbl i) [] in
          let block i term =
            Llair.Block.mk ~lbl:(lbl i) ~params:[] ~cmnd:Vector.empty ~term
          in
          let match_filter =
            jump_unwind (Exp.sub i32 (Exp.integer Z.zero i32) typeid)
          in
          let xlate_clause i =
            let clause = Llvm.operand instr i in
            let num_tis = Llvm.num_operands clause in
            if num_tis = 0 then Llair.Term.goto ~dst:match_filter ~loc
            else
              match Llvm.classify_type (Llvm.type_of clause) with
              | Array (* filter *) -> (
                match Llvm.classify_value clause with
                | ConstantArray ->
                    let rec xlate_filter i =
                      let tiI = xlate_value x (Llvm.operand clause i) in
                      if i < num_tis - 1 then
                        Exp.and_ (Exp.dq tiI ti) (xlate_filter (i + 1))
                      else Exp.dq tiI ti
                    in
                    let key = xlate_filter 0 in
                    Llair.Term.branch ~loc ~key ~nzero:match_filter
                      ~zero:(jump (i + 1))
                | _ -> fail "xlate_instr: %a" pp_llvalue instr () )
              | _ (* catch *) ->
                  let clause = xlate_value x clause in
                  let key =
                    Exp.or_ (Exp.eq clause Exp.null) (Exp.eq clause ti)
                  in
                  Llair.Term.branch ~loc ~key ~nzero:(jump_unwind typeid)
                    ~zero:(jump (i + 1))
          in
          let rec rev_blocks i z =
            if i < num_clauses then
              rev_blocks (i + 1) (block i (xlate_clause i) :: z)
            else block i Llair.Term.unreachable :: z
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
      let rcd = xlate_value x (Llvm.operand instr 0) in
      let exc = Exp.select ~rcd ~idx:(Exp.integer Z.zero Typ.siz) in
      emit_term ~prefix:(pop loc) (Llair.Term.throw ~exc ~loc)
  | Unreachable -> emit_term Llair.Term.unreachable
  | Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP | FPTrunc
   |FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast | Add | FAdd
   |Sub | FSub | Mul | FMul | UDiv | SDiv | FDiv | URem | SRem | FRem
   |Shl | LShr | AShr | And | Or | Xor | ICmp | FCmp | Select
   |GetElementPtr | ExtractElement | InsertElement | ShuffleVector
   |ExtractValue | InsertValue ->
      nop ()
  | VAArg ->
      let reg = xlate_name_opt instr in
      warn "variadic function argument: %a" Loc.pp loc () ;
      emit_inst (Llair.Inst.nondet ~reg ~msg:"vaarg" ~loc)
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

let xlate_block : pop_thunk -> x -> Llvm.llbasicblock -> Llair.block list =
 fun pop x blk ->
  [%Trace.call fun {pf} -> pf "%a" pp_llblock blk]
  ;
  let lbl = label_of_block blk in
  let params, pos = block_formals blk in
  let insts, term, blocks = xlate_instrs pop x pos in
  Llair.Block.mk ~lbl ~params ~cmnd:(Vector.of_list insts) ~term :: blocks
  |>
  [%Trace.retn fun {pf} blocks -> pf "%s" (List.hd_exn blocks).Llair.lbl]

let report_undefined func name =
  if Option.is_some (Llvm.use_begin func) then
    [%Trace.info "undefined function: %a" Global.pp name]

let xlate_function : x -> Llvm.llvalue -> Llair.func =
 fun x llf ->
  [%Trace.call fun {pf} -> pf "%a" pp_llvalue llf]
  ;
  let name = xlate_global x llf in
  let params =
    Llvm.fold_left_params
      (fun rev_args param -> xlate_name param :: rev_args)
      [] llf
  in
  ( match Llvm.block_begin llf with
  | Before entry_blk ->
      let pop = pop_stack_frame_of_function llf entry_blk in
      let[@warning "p"] (entry_block :: entry_blocks) =
        xlate_block pop x entry_blk
      in
      let entry =
        let {Llair.lbl; cmnd; term} = entry_block in
        assert (List.is_empty entry_block.params) ;
        Llair.Block.mk ~lbl ~params ~cmnd ~term
      in
      let cfg =
        let rec trav_blocks rev_cfg prev =
          match Llvm.block_succ prev with
          | Before blk ->
              trav_blocks
                (List.rev_append (xlate_block pop x blk) rev_cfg)
                blk
          | At_end _ -> Vector.of_list_rev rev_cfg
        in
        trav_blocks (List.rev entry_blocks) entry_blk
      in
      Llair.Func.mk ~name ~entry ~cfg
  | At_end _ ->
      report_undefined llf name ;
      Llair.Func.mk_undefined ~name ~params )
  |>
  [%Trace.retn fun {pf} -> pf "@\n%a" Llair.Func.pp]

let transform : Llvm.llmodule -> unit =
 fun llmodule ->
  let pm = Llvm.PassManager.create () in
  let entry_points = Config.find_list "entry-points" in
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

let link_in : Llvm.llcontext -> Llvm.lllinker -> string -> unit =
 fun llcontext link_ctx bc_file ->
  [%Trace.call fun {pf} -> pf "%s" bc_file]
  ;
  let read_and_parse bc_file =
    let llmemorybuffer =
      try Llvm.MemoryBuffer.of_file bc_file
      with Llvm.IoError msg -> fail "%s: %s" bc_file msg ()
    in
    try Llvm_irreader.parse_ir llcontext llmemorybuffer
    with Llvm_irreader.Error msg -> invalid_llvm msg
  in
  Llvm_linker.link_in link_ctx (read_and_parse bc_file)
  |>
  [%Trace.retn fun {pf} _ -> pf ""]

let translate ~fuzzer : string list -> Llair.t =
 fun inputs ->
  [%Trace.call fun {pf} ->
    pf "%a" (List.pp "@ " Format.pp_print_string) inputs]
  ;
  Llvm.install_fatal_error_handler invalid_llvm ;
  let llcontext = Llvm.global_context () in
  let llmodule =
    let model_memorybuffer =
      Llvm.MemoryBuffer.of_string
        (Option.value_exn (Model.read "/cxxabi.bc"))
    in
    Llvm_irreader.parse_ir llcontext model_memorybuffer
  in
  ( if fuzzer then
    let lib_fuzzer_memorybuffer =
      Llvm.MemoryBuffer.of_string
        (Option.value_exn (Model.read "/lib_fuzzer_main.bc"))
    in
    Llvm_linker.link_modules' llmodule
      (Llvm_irreader.parse_ir llcontext lib_fuzzer_memorybuffer) ) ;
  let link_ctx = Llvm_linker.get_linker llmodule in
  List.iter inputs ~f:(link_in llcontext link_ctx) ;
  Llvm_linker.linker_dispose link_ctx ;
  assert (
    Llvm_analysis.verify_module llmodule |> Option.for_all ~f:invalid_llvm
  ) ;
  transform llmodule ;
  scan_names_and_locs llmodule ;
  let lldatalayout =
    Llvm_target.DataLayout.of_string (Llvm.data_layout llmodule)
  in
  let x = {llcontext; llmodule; lldatalayout} in
  let globals =
    Llvm.fold_left_globals
      (fun globals llg -> xlate_global x llg :: globals)
      [] llmodule
  in
  let functions =
    Llvm.fold_left_functions
      (fun functions llf ->
        let name = Llvm.value_name llf in
        if
          String.is_prefix name ~prefix:"__llair_"
          || String.is_prefix name ~prefix:"llvm."
        then functions
        else xlate_function x llf :: functions )
      [] llmodule
  in
  Hashtbl.clear sym_tbl ;
  Hashtbl.clear scope_tbl ;
  Hashtbl.clear anon_struct_name ;
  Hashtbl.clear memo_type ;
  Hashtbl.clear memo_global ;
  Hashtbl.clear memo_value ;
  Hash_set.clear ignored_callees ;
  Llvm.dispose_module llmodule ;
  Llair.mk ~globals ~functions
  |>
  [%Trace.retn fun {pf} _ ->
    pf "number of globals %d, number of functions %d" (List.length globals)
      (List.length functions)]
