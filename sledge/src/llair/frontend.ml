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
let pp_llopcode fs l = Sexp.pp_hum fs (sexp_of_llopcode l)
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

let size_of, bit_size_of =
  let size_to_int size_of x llt =
    if Llvm.type_is_sized llt then
      match Int64.to_int (size_of llt x.lldatalayout) with
      | Some n -> n
      | None -> fail "type size too large: %a" pp_lltype llt ()
    else fail "types with undetermined size: %a" pp_lltype llt ()
  in
  ( size_to_int Llvm_target.DataLayout.abi_size
  , size_to_int Llvm_target.DataLayout.size_in_bits )

let memo_type : (Llvm.lltype, Typ.t) Hashtbl.t = Hashtbl.Poly.create ()

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
      | Vector ->
          let elt = xlate_type x (Llvm.element_type llt) in
          let len = Llvm.vector_size llt in
          Typ.array ~elt ~len ~bits ~byts
      | Array ->
          let elt = xlate_type x (Llvm.element_type llt) in
          let len = Llvm.array_length llt in
          Typ.array ~elt ~len ~bits ~byts
      | Struct ->
          let llelts = Llvm.struct_element_types llt in
          let len = Array.length llelts in
          let packed = Llvm.is_packed llt in
          if Llvm.is_literal llt then
            let elts =
              Vector.map ~f:(xlate_type x) (Vector.of_array llelts)
            in
            Typ.tuple elts ~bits ~byts ~packed
          else
            let name = struct_name llt in
            let elts =
              Vector.init len ~f:(fun i -> lazy (xlate_type x llelts.(i)))
            in
            Typ.struct_ ~name elts ~bits ~byts ~packed
      | Function -> fail "expected to be unsized: %a" pp_lltype llt ()
      | Void | Label | Metadata | Token -> assert false
    else
      match Llvm.classify_type llt with
      | Function ->
          let return = xlate_type_opt x (Llvm.return_type llt) in
          let llargs = Llvm.param_types llt in
          let len = Array.length llargs in
          let args =
            Vector.init len ~f:(fun i -> xlate_type x llargs.(i))
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
  Exp.integer typ data

let xlate_float : x -> Llvm.llvalue -> Exp.t =
 fun x llv ->
  let llt = Llvm.type_of llv in
  let typ = xlate_type x llt in
  let data = suffix_after_last_space (Llvm.string_of_llvalue llv) in
  Exp.float typ data

let xlate_name x ?global : Llvm.llvalue -> Reg.t =
 fun llv ->
  let typ = xlate_type x (Llvm.type_of llv) in
  Reg.program ?global typ (find_name llv)

let xlate_name_opt : x -> Llvm.llvalue -> Reg.t option =
 fun x instr ->
  let llt = Llvm.type_of instr in
  match Llvm.classify_type llt with
  | Void -> None
  | _ -> Some (xlate_name x instr)

let memo_value : (bool * Llvm.llvalue, Exp.t) Hashtbl.t =
  Hashtbl.Poly.create ()

let memo_global : (Llvm.llvalue, Global.t) Hashtbl.t =
  Hashtbl.Poly.create ()

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

and xlate_value ?(inline = false) : x -> Llvm.llvalue -> Exp.t =
 fun x llv ->
  let xlate_value_ llv =
    match Llvm.classify_value llv with
    | Instruction Call -> (
        let func = Llvm.operand llv (Llvm.num_arg_operands llv) in
        let fname = Llvm.value_name func in
        match xlate_intrinsic_exp fname with
        | Some intrinsic when inline || should_inline llv -> intrinsic x llv
        | _ -> Exp.reg (xlate_name x llv) )
    | Instruction (Invoke | Alloca | Load | PHI | LandingPad | VAArg)
     |Argument ->
        Exp.reg (xlate_name x llv)
    | Function | GlobalVariable -> Exp.reg (xlate_global x llv).reg
    | GlobalAlias -> xlate_value x (Llvm.operand llv 0)
    | ConstantInt -> xlate_int x llv
    | ConstantFP -> xlate_float x llv
    | ConstantPointerNull -> Exp.null
    | ConstantAggregateZero -> (
        let typ = xlate_type x (Llvm.type_of llv) in
        match typ with
        | Integer _ -> Exp.integer typ Z.zero
        | Pointer _ -> Exp.null
        | Array _ | Tuple _ | Struct _ ->
            let siz = Typ.size_of typ in
            if siz = 0 then todo "ConstantAggregateZero of size 0" () ;
            Exp.splat typ
              ~byt:(Exp.integer Typ.byt Z.zero)
              ~siz:(Exp.integer Typ.siz (Z.of_int siz))
        | _ -> fail "ConstantAggregateZero of type %a" Typ.pp typ () )
    | ConstantVector | ConstantArray ->
        let typ = xlate_type x (Llvm.type_of llv) in
        let len = Llvm.num_operands llv in
        let f i = xlate_value x (Llvm.operand llv i) in
        Exp.record typ (Vector.init len ~f)
    | ConstantDataVector ->
        let typ = xlate_type x (Llvm.type_of llv) in
        let len = Llvm.vector_size (Llvm.type_of llv) in
        let f i = xlate_value x (Llvm.const_element llv i) in
        Exp.record typ (Vector.init len ~f)
    | ConstantDataArray ->
        let typ = xlate_type x (Llvm.type_of llv) in
        let len = Llvm.array_length (Llvm.type_of llv) in
        let f i = xlate_value x (Llvm.const_element llv i) in
        Exp.record typ (Vector.init len ~f)
    | ConstantStruct ->
        let typ = xlate_type x (Llvm.type_of llv) in
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
          struct_rec ~id:llv typ elt_thks
        else
          Exp.record typ
            (Vector.init (Llvm.num_operands llv) ~f:(fun i ->
                 xlate_value x (Llvm.operand llv i) ))
    | BlockAddress ->
        let parent = find_name (Llvm.operand llv 0) in
        let name = find_name (Llvm.operand llv 1) in
        Exp.label ~parent ~name
    | UndefValue ->
        let typ = xlate_type x (Llvm.type_of llv) in
        Exp.nondet typ (Llvm.string_of_llvalue llv)
    | Instruction
        ( ( Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP
          | FPTrunc | FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast
          | Add | FAdd | Sub | FSub | Mul | FMul | UDiv | SDiv | FDiv | URem
          | SRem | FRem | Shl | LShr | AShr | And | Or | Xor | ICmp | FCmp
          | Select | GetElementPtr | ExtractElement | InsertElement
          | ShuffleVector | ExtractValue | InsertValue ) as opcode ) ->
        if inline || should_inline llv then xlate_opcode x llv opcode
        else Exp.reg (xlate_name x llv)
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
  Hashtbl.find_or_add memo_value (inline, llv) ~default:(fun () ->
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
  let check_vector =
    lazy
      ( if Poly.equal (Llvm.classify_type (Llvm.type_of llv)) Vector then
        todo "vector operations: %a" pp_llvalue llv () )
  in
  let convert opcode =
    let dst = Lazy.force typ in
    let rand = Llvm.operand llv 0 in
    let src = xlate_type x (Llvm.type_of rand) in
    let arg = xlate_value x rand in
    match opcode with
    | Trunc -> Exp.signed (Typ.bit_size_of dst) arg ~to_:dst
    | SExt -> Exp.signed (Typ.bit_size_of src) arg ~to_:dst
    | ZExt -> Exp.unsigned (Typ.bit_size_of src) arg ~to_:dst
    | (BitCast | AddrSpaceCast) when Typ.equal dst src -> arg
    | FPToUI | FPToSI | UIToFP | SIToFP | FPTrunc | FPExt | PtrToInt
     |IntToPtr | BitCast | AddrSpaceCast ->
        Exp.convert src ~to_:dst arg
    | _ -> fail "convert: %a" pp_llopcode opcode ()
  in
  let binary (mk : ?typ:_ -> _) =
    Lazy.force check_vector ;
    let typ = xlate_type x (Llvm.type_of (Llvm.operand llv 0)) in
    mk ~typ (xlate_rand 0) (xlate_rand 1)
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
      Exp.conditional ~typ ~cnd:(xlate_rand 0) ~thn:(xlate_rand 1)
        ~els:(xlate_rand 2)
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
        match (xlate_rand i).desc with
        | Integer {data} -> Z.to_int data
        | _ -> todo "vector operations: %a" pp_llvalue llv ()
      in
      let rcd = xlate_rand 0 in
      match opcode with
      | ExtractElement -> Exp.select typ rcd (idx 1)
      | InsertElement -> Exp.update typ ~rcd (idx 2) ~elt:(xlate_rand 1)
      | _ -> assert false )
  | ExtractValue | InsertValue ->
      let agg = xlate_rand 0 in
      let typ = xlate_type x (Llvm.type_of (Llvm.operand llv 0)) in
      let indices = Llvm.indices llv in
      let num = Array.length indices in
      let rec xlate_indices i rcd typ =
        let rcd_i, typ_i, upd =
          match (typ : Typ.t) with
          | Tuple {elts} | Struct {elts} ->
              ( Exp.select typ rcd indices.(i)
              , Vector.get elts indices.(i)
              , Exp.update typ ~rcd indices.(i) )
          | Array {elt} ->
              ( Exp.select typ rcd indices.(i)
              , elt
              , Exp.update typ ~rcd indices.(i) )
          | _ -> fail "xlate_value: %a" pp_llvalue llv ()
        in
        let update_or_return elt ret =
          match[@warning "p"] opcode with
          | InsertValue -> upd ~elt:(Lazy.force elt)
          | ExtractValue -> ret
        in
        if i < num - 1 then
          let elt = xlate_indices (i + 1) rcd_i typ_i in
          update_or_return (lazy elt) elt
        else
          let elt = lazy (xlate_rand 1) in
          update_or_return elt rcd_i
      in
      xlate_indices 0 agg typ
  | GetElementPtr ->
      if Poly.equal (Llvm.classify_type (Llvm.type_of llv)) Vector then
        todo "vector operations: %a" pp_llvalue llv () ;
      let len = Llvm.num_operands llv in
      assert (len > 0 || invalid_llvm (Llvm.string_of_llvalue llv)) ;
      if len = 1 then convert BitCast
      else
        let rec xlate_indices i =
          [%Trace.call fun {pf} ->
            pf "%i %a" i pp_llvalue (Llvm.operand llv i)]
          ;
          let idx =
            convert_to_siz
              (xlate_type x (Llvm.type_of (Llvm.operand llv i)))
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
      | _ -> todo "vector operations: %a" pp_llvalue llv () )
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
      let g = xlate_name x ~global:() llg in
      let llt = Llvm.type_of llg in
      let typ = xlate_type x llt in
      let loc = find_loc llg in
      (* add to tbl without initializer in case of recursive occurrences in
         its own initializer *)
      Hashtbl.set memo_global ~key:llg ~data:(Global.mk g typ loc) ;
      let init =
        match Llvm.classify_value llg with
        | GlobalVariable ->
            Option.map ~f:(xlate_value x) (Llvm.global_initializer llg)
        | _ -> None
      in
      Global.mk ?init g typ loc
      |>
      [%Trace.retn fun {pf} -> pf "%a" Global.pp_defn] )

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
        Llair.Inst.free ~ptr:(Exp.reg reg) ~loc:retn_loc )
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
    Typ.tuple ~packed:false (Vector.of_array [|pi8; i32|]) ~bits:96 ~byts:12
  in
  (pi8, i32, exc)

(** Translate a control transfer from instruction [instr] to block [dst] to
    a jump, if necessary by extending [blocks] with a trampoline containing
    the PHIs of [dst] translated to a move. *)
let xlate_jump :
       x
    -> ?reg_exps:(Reg.t * Exp.t) list
    -> Llvm.llvalue
    -> Llvm.llbasicblock
    -> Loc.t
    -> Llair.block list
    -> Llair.jump * Llair.block list =
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
  let jmp = Llair.Jump.mk dst_lbl in
  match xlate_jump_ reg_exps (Llvm.instr_begin dst) with
  | [] -> (jmp, blocks)
  | reg_exps ->
      let mov =
        Llair.Inst.move ~reg_exps:(Vector.of_list_rev reg_exps) ~loc
      in
      let lbl = find_name instr ^ ".jmp." ^ dst_lbl in
      let blk =
        Llair.Block.mk ~lbl
          ~cmnd:(Vector.of_array [|mov|])
          ~term:(Llair.Term.goto ~dst:jmp ~loc)
      in
      let blocks =
        match List.find blocks ~f:(fun b -> String.equal lbl b.lbl) with
        | None -> blk :: blocks
        | Some blk0 ->
            assert (Llair.Block.equal blk0 blk) ;
            blocks
      in
      (Llair.Jump.mk lbl, blocks)

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
  | Function | GlobalVariable -> Exp.reg (xlate_name x ~global:() llv)
  | ConstantExpr -> xlate_opcode x llv (Llvm.constexpr_opcode llv)
  | Argument | Instruction _ -> xlate_value x llv
  | GlobalAlias -> xlate_func_name x (Llvm.operand llv 0)
  | GlobalIFunc -> todo "ifunc: %a" pp_llvalue llv ()
  | InlineAsm -> todo "inline asm: %a" pp_llvalue llv ()
  | ConstantPointerNull -> todo "call null: %a" pp_llvalue llv ()
  | k -> todo "function kind %a in %a" pp_llvaluekind k pp_llvalue llv ()

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
  let inline_or_move xlate =
    if should_inline instr then nop ()
    else
      let reg = xlate_name x instr in
      let exp = xlate instr in
      let reg_exps = Vector.of_array [|(reg, exp)|] in
      emit_inst (Llair.Inst.move ~reg_exps ~loc)
  in
  let opcode = Llvm.instr_opcode instr in
  match opcode with
  | Load ->
      let reg = xlate_name x instr in
      let len = Exp.size_of (Exp.reg reg) in
      let ptr = xlate_value x (Llvm.operand instr 0) in
      emit_inst (Llair.Inst.load ~reg ~ptr ~len ~loc)
  | Store ->
      let exp = xlate_value x (Llvm.operand instr 0) in
      let len = Exp.size_of exp in
      let ptr = xlate_value x (Llvm.operand instr 1) in
      emit_inst (Llair.Inst.store ~ptr ~exp ~len ~loc)
  | Alloca ->
      let reg = xlate_name x instr in
      let rand = Llvm.operand instr 0 in
      let num =
        convert_to_siz
          (xlate_type x (Llvm.type_of rand))
          (xlate_value x rand)
      in
      assert (Poly.(Llvm.classify_type (Llvm.type_of instr) = Pointer)) ;
      let len = Exp.size_of (Exp.reg reg) in
      emit_inst (Llair.Inst.alloc ~reg ~num ~len ~loc)
  | Call -> (
      let maybe_llfunc = Llvm.operand instr (Llvm.num_operands instr - 1) in
      let lltyp = Llvm.type_of maybe_llfunc in
      assert (Poly.(Llvm.classify_type lltyp = Pointer)) ;
      let llfunc =
        let llfunc_valuekind = Llvm.classify_value maybe_llfunc in
        match llfunc_valuekind with
        | Function | Instruction _ | InlineAsm | Argument -> maybe_llfunc
        | ConstantExpr -> (
          match Llvm.constexpr_opcode maybe_llfunc with
          | BitCast -> Llvm.operand maybe_llfunc 0
          | _ ->
              todo "opcode kind in call instruction %a" pp_llvalue
                maybe_llfunc () )
        | _ ->
            todo "operand kind in call instruction %a" pp_llvaluekind
              llfunc_valuekind ()
      in
      let fname = Llvm.value_name llfunc in
      let skip msg =
        ( match Hash_set.strict_add ignored_callees fname with
        | Ok () -> warn "ignoring uninterpreted %s %s" msg fname ()
        | Error _ -> () ) ;
        let reg = xlate_name_opt x instr in
        emit_inst (Llair.Inst.nondet ~reg ~msg:fname ~loc)
      in
      (* intrinsics *)
      match xlate_intrinsic_exp fname with
      | Some intrinsic -> inline_or_move (intrinsic x)
      | None -> (
        match String.split fname ~on:'.' with
        | ["__llair_throw"] ->
            let exc = xlate_value x (Llvm.operand instr 0) in
            emit_term ~prefix:(pop loc) (Llair.Term.throw ~exc ~loc)
        | ["__llair_alloc" (* void* __llair_alloc(unsigned size) *)] ->
            let reg = xlate_name x instr in
            let num_operand = Llvm.operand instr 0 in
            let num =
              convert_to_siz
                (xlate_type x (Llvm.type_of num_operand))
                (xlate_value x num_operand)
            in
            let len = Exp.integer Typ.siz (Z.of_int 1) in
            emit_inst (Llair.Inst.alloc ~reg ~num ~len ~loc)
        | ["_Znwm" (* operator new(size_t num) *)]
         |[ "_ZnwmSt11align_val_t"
            (* operator new(unsigned long, std::align_val_t) *) ] ->
            let reg = xlate_name x instr in
            let num = xlate_value x (Llvm.operand instr 0) in
            let len = Exp.size_of (Exp.reg reg) in
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
        | "llvm" :: "coro" :: _ ->
            todo "coroutines:@ %a" pp_llvalue instr ()
        | "llvm" :: "experimental" :: "gc" :: "statepoint" :: _ ->
            todo "statepoints:@ %a" pp_llvalue instr ()
        | ["llvm"; ("va_start" | "va_copy" | "va_end")] ->
            skip "variadic function intrinsic"
        | "llvm" :: _ -> skip "intrinsic"
        | _ when Poly.equal (Llvm.classify_value llfunc) InlineAsm ->
            skip "inline asm"
        (* general function call that may not throw *)
        | _ ->
            let callee = xlate_func_name x llfunc in
            let typ = xlate_type x lltyp in
            let lbl = name ^ ".ret" in
            let call =
              let actuals =
                let num_actuals =
                  if not (Llvm.is_var_arg (Llvm.element_type lltyp)) then
                    Llvm.num_arg_operands instr
                  else
                    let fname = Llvm.value_name llfunc in
                    ( match Hash_set.strict_add ignored_callees fname with
                    | Ok () when not (Llvm.is_declaration llfunc) ->
                        warn
                          "ignoring variable arguments to variadic \
                           function: %a"
                          Exp.pp callee ()
                    | _ -> () ) ;
                    let llfty = Llvm.element_type lltyp in
                    ( match Llvm.classify_type llfty with
                    | Function -> ()
                    | _ ->
                        fail "called function not of function type: %a"
                          pp_llvalue instr () ) ;
                    Array.length (Llvm.param_types llfty)
                in
                List.rev_init num_actuals ~f:(fun i ->
                    xlate_value x (Llvm.operand instr i) )
              in
              let areturn = xlate_name_opt x instr in
              let return = Llair.Jump.mk lbl in
              Llair.Term.call ~callee ~typ ~actuals ~areturn ~return
                ~throw:None ~loc
            in
            continue (fun (insts, term) ->
                let cmnd = Vector.of_list insts in
                ([], call, [Llair.Block.mk ~lbl ~cmnd ~term]) ) ) )
  | Invoke -> (
      let llfunc = Llvm.operand instr (Llvm.num_operands instr - 3) in
      let lltyp = Llvm.type_of llfunc in
      assert (Poly.(Llvm.classify_type lltyp = Pointer)) ;
      let fname = Llvm.value_name llfunc in
      let return_blk = Llvm.get_normal_dest instr in
      let unwind_blk = Llvm.get_unwind_dest instr in
      let num_actuals =
        if not (Llvm.is_var_arg (Llvm.element_type lltyp)) then
          Llvm.num_arg_operands instr
        else (
          ( match Hash_set.strict_add ignored_callees fname with
          | Ok () when not (Llvm.is_declaration llfunc) ->
              warn "ignoring variable arguments to variadic function: %a"
                Global.pp (xlate_global x llfunc) ()
          | _ -> () ) ;
          assert (Poly.(Llvm.classify_type lltyp = Pointer)) ;
          Array.length (Llvm.param_types (Llvm.element_type lltyp)) )
      in
      (* intrinsics *)
      match String.split fname ~on:'.' with
      | _ when Option.is_some (xlate_intrinsic_exp fname) ->
          let dst, blocks = xlate_jump x instr return_blk loc [] in
          emit_term (Llair.Term.goto ~dst ~loc) ~blocks
      | ["__llair_throw"] ->
          let dst, blocks = xlate_jump x instr unwind_blk loc [] in
          emit_term (Llair.Term.goto ~dst ~loc) ~blocks
      | ["abort"] ->
          emit_term ~prefix:[Llair.Inst.abort ~loc] Llair.Term.unreachable
      | ["_Znwm" (* operator new(size_t num) *)]
       |[ "_ZnwmSt11align_val_t"
          (* operator new(unsigned long num, std::align_val_t) *) ]
        when num_actuals > 0 ->
          let reg = xlate_name x instr in
          let num = xlate_value x (Llvm.operand instr 0) in
          let len = Exp.size_of (Exp.reg reg) in
          let dst, blocks = xlate_jump x instr return_blk loc [] in
          emit_term
            ~prefix:[Llair.Inst.alloc ~reg ~num ~len ~loc]
            (Llair.Term.goto ~dst ~loc)
            ~blocks
      (* unimplemented *)
      | "llvm" :: "experimental" :: "gc" :: "statepoint" :: _ ->
          todo "statepoints:@ %a" pp_llvalue instr ()
      (* general function call that may throw *)
      | _ ->
          let callee = xlate_func_name x llfunc in
          let typ = xlate_type x (Llvm.type_of llfunc) in
          let actuals =
            List.rev_init num_actuals ~f:(fun i ->
                xlate_value x (Llvm.operand instr i) )
          in
          let areturn = xlate_name_opt x instr in
          let return, blocks = xlate_jump x instr return_blk loc [] in
          let throw, blocks = xlate_jump x instr unwind_blk loc blocks in
          let throw = Some throw in
          emit_term
            (Llair.Term.call ~callee ~typ ~actuals ~areturn ~return ~throw
               ~loc)
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
        let dst, blocks = xlate_jump x instr blk loc [] in
        emit_term (Llair.Term.goto ~dst ~loc) ~blocks
    | `Conditional (cnd, thn, els) ->
        let key = xlate_value x cnd in
        let thn, blocks = xlate_jump x instr thn loc [] in
        let els, blocks = xlate_jump x instr els loc blocks in
        emit_term (Llair.Term.branch ~key ~nzero:thn ~zero:els ~loc) ~blocks
    )
  | Switch ->
      let key = xlate_value x (Llvm.operand instr 0) in
      let cases, blocks =
        let num_cases = (Llvm.num_operands instr / 2) - 1 in
        let rec xlate_cases i blocks =
          if i <= num_cases then
            let idx = Llvm.operand instr (2 * i) in
            let blk =
              Llvm.block_of_value (Llvm.operand instr ((2 * i) + 1))
            in
            let num = xlate_value x idx in
            let jmp, blocks = xlate_jump x instr blk loc blocks in
            let rest, blocks = xlate_cases (i + 1) blocks in
            ((num, jmp) :: rest, blocks)
          else ([], blocks)
        in
        xlate_cases 1 []
      in
      let tbl = Vector.of_list cases in
      let blk = Llvm.block_of_value (Llvm.operand instr 1) in
      let els, blocks = xlate_jump x instr blk loc blocks in
      emit_term (Llair.Term.switch ~key ~tbl ~els ~loc) ~blocks
  | IndirectBr ->
      let ptr = xlate_value x (Llvm.operand instr 0) in
      let num_dests = Llvm.num_operands instr - 1 in
      let lldests, blocks =
        let rec dests i blocks =
          if i <= num_dests then
            let v = Llvm.operand instr i in
            let blk = Llvm.block_of_value v in
            let jmp, blocks = xlate_jump x instr blk loc blocks in
            let rest, blocks = dests (i + 1) blocks in
            (jmp :: rest, blocks)
          else ([], blocks)
        in
        dests 1 []
      in
      let tbl = Vector.of_list lldests in
      emit_term (Llair.Term.iswitch ~ptr ~tbl ~loc) ~blocks
  | LandingPad ->
      (* Translate the landingpad clauses to code to load the type_info from
         the thrown exception, and test the type_info against the clauses,
         eventually jumping to the handler code following the landingpad,
         passing a value for the selector which the handler code tests to
         e.g. either cleanup or rethrow. *)
      let i32, tip, cxa_exception = landingpad_typs x instr in
      let pi8, _, exc_typ = exception_typs in
      let exc = Exp.reg (Reg.program pi8 (find_name instr ^ ".exc")) in
      let ti = Reg.program tip (name ^ ".ti") in
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
        Llair.Inst.load ~reg:ti ~ptr ~len ~loc
      in
      let ti = Exp.reg ti in
      let typeid = xlate_llvm_eh_typeid_for x tip ti in
      let lbl = name ^ ".unwind" in
      let reg = xlate_name x instr in
      let jump_unwind i sel rev_blocks =
        let exp = Exp.record exc_typ (Vector.of_array [|exc; sel|]) in
        let mov =
          Llair.Inst.move ~reg_exps:(Vector.of_array [|(reg, exp)|]) ~loc
        in
        let lbl_i = lbl ^ "." ^ Int.to_string i in
        let blk =
          Llair.Block.mk ~lbl:lbl_i
            ~cmnd:(Vector.of_array [|mov|])
            ~term:(Llair.Term.goto ~dst:(Llair.Jump.mk lbl) ~loc)
        in
        (Llair.Jump.mk lbl_i, blk :: rev_blocks)
      in
      let goto_unwind i sel blocks =
        let dst, blocks = jump_unwind i sel blocks in
        (Llair.Term.goto ~dst ~loc, blocks)
      in
      let term_unwind, rev_blocks =
        if Llvm.is_cleanup instr then
          goto_unwind 0 (Exp.integer i32 Z.zero) []
        else
          let num_clauses = Llvm.num_operands instr in
          let lbl i = name ^ "." ^ Int.to_string i in
          let jump i = Llair.Jump.mk (lbl i) in
          let block i term =
            Llair.Block.mk ~lbl:(lbl i) ~cmnd:Vector.empty ~term
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
              (Llair.Term.goto ~dst ~loc, rev_blocks)
            else
              match Llvm.classify_type (Llvm.type_of clause) with
              | Array (* filter *) -> (
                match Llvm.classify_value clause with
                | ConstantArray ->
                    let rec xlate_filter i =
                      let tiI = xlate_value x (Llvm.operand clause i) in
                      if i < num_tis - 1 then
                        Exp.and_ ~typ:Typ.bool (Exp.dq ~typ:tip tiI ti)
                          (xlate_filter (i + 1))
                      else Exp.dq ~typ:tip tiI ti
                    in
                    let key = xlate_filter 0 in
                    let nzero, rev_blocks = match_filter i rev_blocks in
                    ( Llair.Term.branch ~loc ~key ~nzero ~zero:(jump (i + 1))
                    , rev_blocks )
                | _ -> fail "xlate_instr: %a" pp_llvalue instr () )
              | _ (* catch *) ->
                  let typ = xlate_type x (Llvm.type_of clause) in
                  let clause = xlate_value x clause in
                  let key =
                    Exp.or_ ~typ:Typ.bool
                      (Exp.eq ~typ clause Exp.null)
                      (Exp.eq ~typ clause ti)
                  in
                  let nzero, rev_blocks = jump_unwind i typeid rev_blocks in
                  ( Llair.Term.branch ~loc ~key ~nzero ~zero:(jump (i + 1))
                  , rev_blocks )
          in
          let rec rev_blocks i z =
            if i < num_clauses then
              let term, z = xlate_clause i z in
              rev_blocks (i + 1) (block i term :: z)
            else block i Llair.Term.unreachable :: z
          in
          xlate_clause 0 (rev_blocks 1 [])
      in
      continue (fun (insts, term) ->
          ( [load_ti]
          , term_unwind
          , List.rev_append rev_blocks
              [Llair.Block.mk ~lbl ~cmnd:(Vector.of_list insts) ~term] ) )
  | Resume ->
      let llrcd = Llvm.operand instr 0 in
      let typ = xlate_type x (Llvm.type_of llrcd) in
      let rcd = xlate_value x llrcd in
      let exc = Exp.select typ rcd 0 in
      emit_term ~prefix:(pop loc) (Llair.Term.throw ~exc ~loc)
  | Unreachable -> emit_term Llair.Term.unreachable
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
      emit_inst (Llair.Inst.nondet ~reg ~msg:"vaarg" ~loc)
  | CleanupRet | CatchRet | CatchPad | CleanupPad | CatchSwitch ->
      todo "windows exception handling: %a" pp_llvalue instr ()
  | Fence | AtomicCmpXchg | AtomicRMW ->
      fail "xlate_instr: %a" pp_llvalue instr ()
  | PHI | Invalid | Invalid2 | UserOp1 | UserOp2 -> assert false

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
  let pos = skip_phis blk in
  let insts, term, blocks = xlate_instrs pop x pos in
  Llair.Block.mk ~lbl ~cmnd:(Vector.of_list insts) ~term :: blocks
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
  let formals =
    Llvm.fold_left_params
      (fun rev_args param -> xlate_name x param :: rev_args)
      [] llf
  in
  let freturn =
    match name.typ with
    | Pointer {elt= Function {return= Some typ; _}} ->
        Some (Reg.program typ "freturn")
    | _ -> None
  in
  let _, _, exc_typ = exception_typs in
  let fthrow = Reg.program exc_typ "fthrow" in
  ( match Llvm.block_begin llf with
  | Before entry_blk ->
      let pop = pop_stack_frame_of_function x llf entry_blk in
      let[@warning "p"] (entry_block :: entry_blocks) =
        xlate_block pop x entry_blk
      in
      let entry =
        let {Llair.lbl; cmnd; term} = entry_block in
        Llair.Block.mk ~lbl ~cmnd ~term
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
      Llair.Func.mk ~name ~formals ~freturn ~fthrow ~entry ~cfg
  | At_end _ ->
      report_undefined llf name ;
      Llair.Func.mk_undefined ~name ~formals ~freturn ~fthrow )
  |>
  [%Trace.retn fun {pf} -> pf "@\n%a" Llair.Func.pp]

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
  [%Trace.call fun {pf} -> pf "%s" bc_file]
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
      Int64.to_int_exn (Llvm_target.DataLayout.abi_size llt lldatalayout)
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

let translate ~models ~fuzzer ~internalize : string list -> Llair.t =
 fun inputs ->
  [%Trace.call fun {pf} ->
    pf "%a" (List.pp "@ " Format.pp_print_string) inputs]
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
         (Llvm.MemoryBuffer.of_string (Option.value_exn (Model.read name))))
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
  let x = {llcontext; llmodule; lldatalayout} in
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
