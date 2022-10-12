(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** An execution history is a current instruction pointer and some
    predecessors. [preds] are empty iff this is an entrypoint. *)
type t =
  | Init
  | Step of {curr: Llair.IP.t; preds: t iarray}
  | Goal_progress of t
[@@deriving sexp_of]

let init = Init
let extend curr preds = Step {curr; preds= IArray.of_list preds}
let progress_goal x = Goal_progress x

let pp_ip fs ip =
  let open Llair in
  Format.fprintf fs "%-8s%a\t%a"
    (Format.asprintf "%a" IP.pp ip)
    FuncName.pp (IP.block ip).parent.name Loc.pp (IP.loc ip)

type path = (Llair.ip * bool) iter

let pp_path ?(show_root = false) fs path =
  Iter.foldi path (not show_root)
    ~f:(fun idx (ip, goal_progress) seen_root ->
      let is_root = goal_progress && not seen_root in
      Format.fprintf fs "@ %6i: %-8s%a" idx
        (if is_root then "ROOT:" else "")
        pp_ip ip ;
      seen_root || goal_progress )
  |> ignore

let dump ?(show_root = false) h fs =
  (* todo: output nicely-formatted DAG; just printing a single
     arbitrarily-chosen witness path from the root for now. *)
  let path =
    let rec path_impl = function
      | Init -> []
      | Goal_progress path -> path_impl path
      | Step {curr; preds} ->
          let tail =
            if IArray.is_empty preds then []
            else path_impl (IArray.get preds 0)
          in
          if Llair.IP.index curr = 0 || IArray.length preds > 1 then
            let goal_progress =
              IArray.exists preds ~f:(function
                | Goal_progress _ -> true
                | _ -> false )
            in
            (curr, goal_progress) :: tail
          else tail
    in
    path_impl >> List.rev
  in
  Format.fprintf fs "@[<v 2>Witness Trace:%a@]" (pp_path ~show_root)
    (Iter.of_list (path h))

module Env = Llair.Reg.Tbl

type xcontext =
  { ctx: Z3.context
  ; time_sort: Z3.Sort.sort
  ; addr_sort: Z3.Sort.sort
  ; registers: Z3.Expr.expr option Env.t
  ; stack: Llair.callee Llair.call Stack.t
  ; memory: Z3.FuncDecl.func_decl
  ; mutable next_alloc: Z3.Expr.expr }

let mk_xcontext () =
  let registers = Env.create () in
  let stack = Stack.create () in
  let ctx = Z3.mk_context [("model", "true"); ("timeout", "3600000")] in
  let time_sort = Z3.Arithmetic.Integer.mk_sort ctx in
  let addr_sort = Z3.BitVector.mk_sort ctx 64 in
  let byte_sort = Z3.BitVector.mk_sort ctx 8 in
  let memory =
    Z3.FuncDecl.mk_func_decl_s ctx "memory" [time_sort]
      (Z3.Z3Array.mk_sort ctx addr_sort byte_sort)
  in
  let next_alloc = Z3.Expr.mk_numeral_int ctx 1 addr_sort in
  {ctx; time_sort; addr_sort; registers; stack; memory; next_alloc}

let time_to_z3 {ctx; time_sort} time =
  Z3.Expr.mk_numeral_int ctx time time_sort

let typ_to_z3 : Z3.context -> Llair.Typ.t -> Z3.Sort.sort =
 fun ctx typ ->
  let bits = Llair.Typ.bit_size_of typ in
  Z3.BitVector.mk_sort ctx bits

let pp_z3_exp fs e =
  let e = Z3.Expr.simplify e None in
  let s = Z3.Expr.to_string e in
  if String.contains s '\n' then Format.fprintf fs "\n@<1>%s" s
  else Format.fprintf fs "%s" s

let pp_z3_exps = List.pp ";@ " pp_z3_exp

let push_reg {registers} reg =
  [%Dbg.info " %a" Llair.Reg.pp reg] ;
  Env.add registers reg None

let pop_reg {registers} reg =
  [%Dbg.info " %a" Llair.Reg.pp reg] ;
  Env.remove registers reg

let get_reg {registers} reg = Env.find_exn registers reg |> Option.get_exn

let set_reg {registers} reg exp =
  [%Dbg.info " %a := %a" Llair.Reg.pp reg pp_z3_exp exp] ;
  Env.set registers ~key:reg ~data:(Some exp)

let typ_to_fp_sort ctx typ =
  match (typ : Llair.Typ.t) with
  | Float {bits= 16; enc= `Brain} -> Z3.FloatingPoint.mk_sort ctx 8 7
  | Float {bits= 16; enc= `IEEE} -> Z3.FloatingPoint.mk_sort_16 ctx
  | Float {bits= 32; enc= `IEEE} -> Z3.FloatingPoint.mk_sort_32 ctx
  | Float {bits= 64; enc= `IEEE} -> Z3.FloatingPoint.mk_sort_64 ctx
  | Float {bits= 128; enc= `IEEE} -> Z3.FloatingPoint.mk_sort_128 ctx
  | _ -> todo "%a" Llair.Typ.pp typ ()

let rec exp_to_z3 : xcontext -> int -> Llair.Exp.t -> Z3.Expr.expr =
 fun ({ctx} as x) time exp ->
  [%dbg]
    ~call:(fun {pf} -> pf " %i %a" time Llair.Exp.pp exp)
    ~retn:(fun {pf} -> pf "@[%a@]" pp_z3_exp)
  @@ fun () ->
  let mk_dq ctx d e = Z3.Boolean.(mk_not ctx (mk_eq ctx d e)) in
  let bv2bool b =
    let bit_sort = Z3.BitVector.mk_sort ctx 1 in
    mk_dq ctx b (Z3.Expr.mk_numeral_int ctx 0 bit_sort)
  in
  let bool2bv b =
    let bit_sort = Z3.BitVector.mk_sort ctx 1 in
    Z3.Boolean.mk_ite ctx b
      (Z3.Expr.mk_numeral_int ctx 1 bit_sort)
      (Z3.Expr.mk_numeral_int ctx 0 bit_sort)
  in
  let binop op d e = op ctx (exp_to_z3 x time d) (exp_to_z3 x time e) in
  let boolbinop op d e = bool2bv (binop op d e) in
  match exp with
  | Reg _ as r -> get_reg x (r |> Llair.Reg.of_exp |> Option.get_exn)
  | Global {name; typ= _} | FuncName {name; typ= _} ->
      Z3.BitVector.mk_const_s ctx name 64
  (* | Label {parent; name} -> *)
  | Integer {data; typ} -> (
      let sort = typ_to_z3 ctx typ in
      match Z.to_int data with
      | n -> Z3.Expr.mk_numeral_int ctx n sort
      | exception Z.Overflow ->
          Z3.Expr.mk_numeral_string ctx (Z.to_string data) sort )
  | Float {data; typ= Float _ as typ} ->
      let sort = typ_to_fp_sort ctx typ in
      let num =
        try Z3.FloatingPoint.mk_numeral_s ctx data sort
        with Z3.Error _ -> (
          try
            Z3.FloatingPoint.mk_numeral_f ctx
              (Float.of_string_exn data)
              sort
          with Invalid_argument _ -> todo "%a" Llair.Exp.pp exp () )
      in
      Z3.FloatingPoint.mk_to_ieee_bv ctx num
  | Ap1 (Signed {bits= n}, Integer {bits= m}, e) ->
      Z3.BitVector.mk_sign_ext ctx (m - n)
        (Z3.BitVector.mk_extract ctx (n - 1) 0 (exp_to_z3 x time e))
  | Ap1 (Unsigned {bits= n}, Integer {bits= m}, e) ->
      Z3.BitVector.mk_zero_ext ctx (m - n)
        (Z3.BitVector.mk_extract ctx (n - 1) 0 (exp_to_z3 x time e))
  | Ap1 (Convert {src}, dst, e) when Llair.Typ.equivalent src dst ->
      exp_to_z3 x time e
  | Ap1 (Convert {src= Float _ as src}, Integer {bits}, e) ->
      let arg = exp_to_z3 x time e in
      (* arg is ieee_bv encoding of a float, decode to float, convert to
         bv *)
      let sort = typ_to_fp_sort ctx src in
      let mode = Z3.FloatingPoint.RoundingMode.mk_rne ctx in
      Z3.FloatingPoint.mk_to_sbv ctx mode
        (Z3.FloatingPoint.mk_to_fp_bv ctx arg sort)
        bits
  | Ap1 (Convert {src= Integer _}, (Float _ as dst), e) ->
      let arg = exp_to_z3 x time e in
      (* arg is bv encoding of an int, convert to float, encode to
         ieee_bv *)
      let sort = typ_to_fp_sort ctx dst in
      let mode = Z3.FloatingPoint.RoundingMode.mk_rne ctx in
      Z3.FloatingPoint.mk_to_ieee_bv ctx
        (Z3.FloatingPoint.mk_to_fp_signed ctx mode arg sort)
  (* | Ap1 (Convert {src= Float _}, Float _, e) -> 
   * | Ap1 (Convert {src}, dst, e) -> *)
  | Ap2 (Eq, _, d, e) -> boolbinop Z3.Boolean.mk_eq d e
  | Ap2 (Dq, _, d, e) ->
      let mk_dq ctx d e = Z3.Boolean.(mk_not ctx (mk_eq ctx d e)) in
      boolbinop mk_dq d e
  | Ap2 (Gt, _, d, e) -> boolbinop Z3.BitVector.mk_sgt d e
  | Ap2 (Lt, _, d, e) -> boolbinop Z3.BitVector.mk_slt d e
  | Ap2 (Ge, _, d, e) -> boolbinop Z3.BitVector.mk_sge d e
  | Ap2 (Le, _, d, e) -> boolbinop Z3.BitVector.mk_sle d e
  | Ap2 (Ugt, _, d, e) -> boolbinop Z3.BitVector.mk_ugt d e
  | Ap2 (Ult, _, d, e) -> boolbinop Z3.BitVector.mk_ult d e
  | Ap2 (Uge, _, d, e) -> boolbinop Z3.BitVector.mk_uge d e
  | Ap2 (Ule, _, d, e) -> boolbinop Z3.BitVector.mk_ule d e
  (* | Ap2 (Ord, _, d, e) ->
   * | Ap2 (Uno, _, d, e) -> *)
  | Ap2 (Add, _, d, e) -> binop Z3.BitVector.mk_add d e
  | Ap2 (Sub, _, d, e) -> binop Z3.BitVector.mk_sub d e
  | Ap2 (Mul, _, d, e) -> binop Z3.BitVector.mk_mul d e
  | Ap2 (Div, _, d, e) -> binop Z3.BitVector.mk_sdiv d e
  | Ap2 (Rem, _, d, e) -> binop Z3.BitVector.mk_srem d e
  | Ap2 (Udiv, _, d, e) -> binop Z3.BitVector.mk_udiv d e
  | Ap2 (Urem, _, d, e) -> binop Z3.BitVector.mk_urem d e
  | Ap2 (And, _, d, e) -> binop Z3.BitVector.mk_and d e
  | Ap2 (Or, _, d, e) -> binop Z3.BitVector.mk_or d e
  | Ap2 (Xor, _, d, e) -> binop Z3.BitVector.mk_xor d e
  | Ap2 (Shl, _, d, e) -> binop Z3.BitVector.mk_shl d e
  | Ap2 (Lshr, _, d, e) -> binop Z3.BitVector.mk_lshr d e
  | Ap2 (Ashr, _, d, e) -> binop Z3.BitVector.mk_ashr d e
  | Ap3 (Conditional, _, cnd, thn, els) ->
      Z3.Boolean.mk_ite ctx
        (bv2bool (exp_to_z3 x time cnd))
        (exp_to_z3 x time thn) (exp_to_z3 x time els)
  (* | Ap1 (Select idx, typ, rcd) ->
   * | Ap2 (Update idx, typ, rcd, elt) -> *)
  | ApN (Record, typ, elts) ->
      Iter.mapi (IArray.to_iter elts) ~f:(fun i elt ->
          let elt_size = Llair.Typ.size_of (Llair.Exp.typ_of elt) in
          let _, slot_size = Llair.Typ.offset_length_of_elt typ i in
          let padding = slot_size - elt_size in
          assert (padding >= 0) ;
          Z3.BitVector.mk_zero_ext ctx padding (exp_to_z3 x time elt) )
      |> Iter.reduce_exn ~f:(Z3.BitVector.mk_concat ctx)
  (* | Ap1 (Splat, _, byt) -> *)
  | _ -> todo "%a" Llair.Exp.pp exp ()

let preserve_memory x time =
  let {ctx; memory} = x in
  [ Z3.Boolean.mk_eq ctx
      (Z3.Expr.mk_app ctx memory [time_to_z3 x (time + 1)])
      (Z3.Expr.mk_app ctx memory [time_to_z3 x time]) ]

let inst_to_z3 : xcontext -> int -> Llair.inst -> Z3.Expr.expr list =
 fun x time inst ->
  [%dbg]
    ~call:(fun {pf} -> pf " %i %a" time Llair.Inst.pp inst)
    ~retn:(fun {pf} es -> pf "@[%a@]" pp_z3_exps es)
  @@ fun () ->
  let {ctx; addr_sort; memory; next_alloc} = x in
  match inst with
  | Move {reg_exps} ->
      IArray.iter reg_exps ~f:(fun (reg, exp) ->
          set_reg x reg (exp_to_z3 x time exp) ) ;
      preserve_memory x time
  | Load {reg; ptr; len} ->
      let len =
        match len with Integer {data} -> Z.to_int data | _ -> assert false
      in
      let mem = Z3.Expr.mk_app ctx memory [time_to_z3 x time] in
      let ptr = exp_to_z3 x time ptr in
      let get i =
        Z3.Z3Array.mk_select ctx mem
          (Z3.BitVector.mk_add ctx ptr
             (Z3.Expr.mk_numeral_int ctx i addr_sort) )
      in
      let exp =
        Iter.(0 -- (len - 1))
        |> Iter.map ~f:get
        |> Iter.reduce_exn ~f:(Z3.BitVector.mk_concat ctx)
      in
      set_reg x reg exp ;
      preserve_memory x time
  | Store {ptr; exp; len} ->
      let len =
        match len with Integer {data} -> Z.to_int data | _ -> assert false
      in
      let ptr = exp_to_z3 x time ptr in
      let exp = exp_to_z3 x time exp in
      let set i mem =
        let low = i * 8 in
        let high = low + 7 in
        Z3.Z3Array.mk_store ctx mem
          (Z3.BitVector.mk_add ctx ptr
             (Z3.Expr.mk_numeral_int ctx i addr_sort) )
          (Z3.BitVector.mk_extract ctx high low exp)
      in
      let mem = Z3.Expr.mk_app ctx memory [time_to_z3 x time] in
      let mem' = Iter.fold ~f:set Iter.(0 -- (len - 1)) mem in
      [ Z3.Boolean.mk_eq ctx
          (Z3.Expr.mk_app ctx memory [time_to_z3 x (time + 1)])
          mem' ]
  | AtomicRMW _ | AtomicCmpXchg _ -> todo "%a" Llair.Inst.pp inst ()
  | Alloc {reg; num; len} ->
      let num = exp_to_z3 x time num in
      let len = Z3.Expr.mk_numeral_int ctx len addr_sort in
      let size = Z3.BitVector.mk_mul ctx num len in
      set_reg x reg next_alloc ;
      x.next_alloc <- Z3.BitVector.mk_add ctx next_alloc size ;
      preserve_memory x time
  | Free _ -> preserve_memory x time
  | Nondet {reg; msg} ->
      Option.iter reg ~f:(fun reg ->
          let sort = typ_to_z3 ctx (Llair.Reg.typ reg) in
          set_reg x reg (Z3.Expr.mk_fresh_const ctx msg sort) ) ;
      preserve_memory x time
  | Builtin {reg; name; args} -> (
    match (reg, name, IArray.to_array args) with
    | None, `memset, [|dst; byt; len; _isvolatile|] ->
        let len =
          match len with
          | Integer {data} -> Z.to_int data
          | _ | (exception Z.Overflow) -> todo "%a" Llair.Inst.pp inst ()
        in
        let dst = exp_to_z3 x time dst in
        let byt = exp_to_z3 x time byt in
        let set i mem =
          Z3.Z3Array.mk_store ctx mem
            (Z3.BitVector.mk_add ctx dst
               (Z3.Expr.mk_numeral_int ctx i addr_sort) )
            byt
        in
        let mem = Z3.Expr.mk_app ctx memory [time_to_z3 x time] in
        let mem' = Iter.fold ~f:set Iter.(0 -- (len - 1)) mem in
        [ Z3.Boolean.mk_eq ctx
            (Z3.Expr.mk_app ctx memory [time_to_z3 x (time + 1)])
            mem' ]
    | None, (`memcpy | `memmove), [|dst; src; len; _isvolatile|] -> (
        let len = exp_to_z3 x time len in
        let src = exp_to_z3 x time src in
        let dst = exp_to_z3 x time dst in
        let mem0 = Z3.Expr.mk_app ctx memory [time_to_z3 x time] in
        let mem1 = Z3.Expr.mk_app ctx memory [time_to_z3 x (time + 1)] in
        let set i mem =
          let i = Z3.Expr.mk_numeral_int ctx i addr_sort in
          Z3.Z3Array.mk_store ctx mem
            (Z3.BitVector.mk_add ctx dst i)
            (Z3.Z3Array.mk_select ctx mem0 (Z3.BitVector.mk_add ctx src i))
        in
        let len_int =
          Z3.Expr.simplify (Z3.BitVector.mk_bv2int ctx len false) None
        in
        match Z.to_int (Z3.Arithmetic.Integer.get_big_int len_int) with
        | n ->
            [ Z3.Boolean.mk_eq ctx mem1
                (Iter.fold ~f:set Iter.(0 -- (n - 1)) mem0) ]
        | exception (Z3.Error _ | Z.Overflow) ->
            let adr = Z3.Quantifier.mk_bound ctx 0 addr_sort in
            let body =
              Z3.Boolean.mk_ite ctx
                (Z3.Boolean.mk_and ctx
                   [ Z3.BitVector.mk_ule ctx dst adr
                   ; Z3.BitVector.mk_ult ctx adr
                       (Z3.BitVector.mk_add ctx dst len) ] )
                (Z3.Boolean.mk_eq ctx
                   (Z3.Z3Array.mk_select ctx mem1 adr)
                   (Z3.Z3Array.mk_select ctx mem0
                      (Z3.BitVector.mk_add ctx src
                         (Z3.BitVector.mk_sub ctx adr dst) ) ) )
                (Z3.Boolean.mk_eq ctx
                   (Z3.Z3Array.mk_select ctx mem1 adr)
                   (Z3.Z3Array.mk_select ctx mem0 adr) )
            in
            [ Z3.Quantifier.expr_of_quantifier
                (Z3.Quantifier.mk_forall ctx [addr_sort]
                   [Z3.Symbol.mk_string ctx "adr"]
                   body None [] [] None None ) ] )
    | _ -> todo "%a" Llair.Inst.pp inst () )

let do_call ({stack} as x) time formals locals actuals call =
  let actuals = IArray.map ~f:(exp_to_z3 x time) actuals in
  IArray.iter2_exn formals actuals ~f:(fun formal actual ->
      push_reg x formal ;
      set_reg x formal actual ) ;
  Llair.Reg.Set.iter ~f:(push_reg x) locals ;
  Stack.push call stack

let term_to_z3 ({ctx; stack} as x) time term next =
  [%dbg]
    ~call:(fun {pf} ->
      pf " %i %a@ to %a" time Llair.Term.pp term Llair.IP.pp next )
    ~retn:(fun {pf} es -> pf "@[%a@]" pp_z3_exps es)
  @@ fun () ->
  preserve_memory x time
  @
  match (term : Llair.term) with
  | Switch {key; tbl; els} -> (
      let key = exp_to_z3 x time key in
      let next = Llair.IP.block next in
      match
        IArray.find_map tbl ~f:(fun (exp, {dst}) ->
            if Llair.Block.equal dst next then
              Some [Z3.Boolean.mk_eq ctx key (exp_to_z3 x time exp)]
            else None )
      with
      | Some cst -> cst
      | None ->
          assert (Llair.Block.equal els.dst next) ;
          Iter.map (IArray.to_iter tbl) ~f:(fun (exp, _) ->
              Z3.Boolean.(mk_not ctx (mk_eq ctx key (exp_to_z3 x time exp))) )
          |> Iter.to_list )
  | Iswitch _ -> todo "%a" Llair.Term.pp term ()
  | Call ({callee= Direct {func= {formals; locals}}; actuals} as call) ->
      do_call x time formals locals actuals call ;
      []
  | Call ({callee= Indirect _; actuals} as call) ->
      let callee = Llair.(IP.block next).parent in
      let {Llair.formals; locals} = callee in
      do_call x time formals locals actuals
        {call with callee= Llair.Direct {func= callee; recursive= true}} ;
      []
  | Call {callee= Intrinsic _} -> todo "%a" Llair.Term.pp term ()
  | Return {exp} ->
      let exp = Option.map ~f:(exp_to_z3 x time) exp in
      let {Llair.callee; areturn} = Stack.pop stack in
      ( match callee with
      | Direct {func= {formals; locals}} ->
          Llair.Reg.Set.iter ~f:(pop_reg x) locals ;
          IArray.iter ~f:(pop_reg x) formals
      | _ -> todo "%a" Llair.Term.pp term () ) ;
      ( match (areturn, exp) with
      | Some areturn, Some exp -> set_reg x areturn exp
      | None, None -> ()
      | _ -> todo "%a" Llair.Term.pp term () ) ;
      []
  | Throw _ -> todo "%a" Llair.Term.pp term ()
  | Abort _ | Unreachable -> todo "%a" Llair.Term.pp term ()

let ip_to_z3 ctx time curr next =
  match Llair.IP.inst curr with
  | Some inst -> inst_to_z3 ctx time inst
  | None -> term_to_z3 ctx time (Llair.IP.block curr).term next

let rec paths h : path iter =
  match h with
  | Init -> Iter.singleton Iter.empty
  | Goal_progress h -> paths h
  | Step {curr; preds} ->
      if Llair.IP.index curr <> 0 then
        Iter.flat_map ~f:paths (IArray.to_iter preds)
      else
        let block_ips = Llair.Block.iter (Llair.IP.block curr) in
        Iter.flat_map (IArray.to_iter preds) ~f:(fun pred ->
            let progress =
              match pred with Goal_progress _ -> true | _ -> false
            in
            Iter.map (paths pred) ~f:(fun path ->
                Iter.append path
                  (Iter.map block_ips ~f:(fun ip ->
                       (ip, progress && Llair.IP.index ip = 0) ) ) ) )

let dp_mem xcontext time model ppf =
  let {ctx; addr_sort; memory; next_alloc} = xcontext in
  let pp_mem_addr ppf addr =
    match
      Z3.Model.eval model
        (Z3.Z3Array.mk_select ctx
           (Z3.Expr.mk_app ctx memory [time_to_z3 xcontext time])
           (Z3.Expr.mk_numeral_int ctx addr addr_sort) )
        false
    with
    | Some byt -> Format.fprintf ppf "%i: %a" addr pp_z3_exp byt
    | None -> warn "eval memory failed" ()
  in
  match Z3.Model.eval model next_alloc false with
  | Some next_alloc -> (
    match Z3.BitVector.numeral_to_string next_alloc with
    | s -> (
      try
        Scanf.sscanf s "%x" (fun n ->
            Format.fprintf ppf "@[<v 2>Memory:@ %t@]" (fun fs ->
                Iter.(0 -- (n - 1))
                |> Iter.to_list
                |> List.chunks 8
                |> List.pp ~pre:"@[<v>" "@ " ~suf:"@]"
                     (List.pp ~pre:"@[<h>" "@ " ~suf:"@]" pp_mem_addr)
                     fs ) )
      with Scanf.Scan_failure _ ->
        warn "convert next_alloc to int failed: %s" s () )
    | exception Z3.Error _ -> warn "convert next_alloc to string failed" ()
    )
  | None -> warn "eval next_alloc failed" ()

let count = ref 0

let dp_root_call xcontext curr =
  match Llair.(IP.block curr).term with
  | Call {callee= Direct {func}; actuals} ->
      let regs =
        IArray.fold
          ~f:Llair.(Exp.fold_regs ~f:Reg.Set.add)
          actuals Llair.Reg.Set.empty
      in
      let pp_regs ppf regs =
        let pp_reg ppf reg =
          Format.fprintf ppf "%a = %a" Llair.Reg.pp reg pp_z3_exp
            (get_reg xcontext reg)
        in
        Llair.Reg.Set.pp_full ~sep:",@ " pp_reg ppf regs
      in
      Format.dprintf
        "@[<v>Called trace root function:@   %s@ Arguments: (@[%a@])@ \
         Registers: @[%a@]@]@\n"
        (Llair.FuncName.name func.name)
        (IArray.pp ",@ " Llair.Exp.pp)
        actuals pp_regs regs
  | term -> todo "root not a direct call: %a" Llair.Term.pp term ()

let validate_path ({ctx} as xcontext) path =
  [%dbg]
    ~call:(fun {pf} ->
      incr count ;
      pf " %i" !count )
    ~retn:(fun {pf} r -> pf "%b" (Option.is_some r))
  @@ fun () ->
  let solver = Z3.Solver.mk_solver ctx None in
  (* history does not include call of entry point, so look up parent
     function of first instruction and enter scope of its formals and
     locals *)
  let (head, _), rest = Iter.pop path |> Option.get_exn in
  let entry_func = (Llair.IP.block head).parent in
  IArray.iter ~f:(push_reg xcontext) entry_func.formals ;
  Llair.Reg.Set.iter ~f:(push_reg xcontext) entry_func.locals ;
  let _, root_time, dp_root =
    Iter.foldi rest
      (head, -1, fun _ -> ())
      ~f:(fun time (next, progress) (curr, root_time, dp_root) ->
        let root_time, dp_root =
          if progress && root_time < 0 then
            (time + 1, dp_root_call xcontext curr)
          else (root_time, dp_root)
        in
        Z3.Solver.add solver (ip_to_z3 xcontext time curr next) ;
        (next, root_time, dp_root) )
  in
  match Z3.Solver.check solver [] with
  | SATISFIABLE -> (
    match Z3.Solver.get_model solver with
    | Some model ->
        Some
          (fun fs ->
            Format.fprintf fs
              "@[<v>Witness Trace:@ @[<v>%a@]@ @ Validated: true@ @ %t@ \
               %t@ @ Full trace execution encoding:@ %s@\n\
               @]"
              (pp_path ~show_root:true) path dp_root
              (dp_mem xcontext root_time model)
              (Z3.Model.to_string model) )
    | None ->
        Some
          (Format.dprintf "@[<v 2>Witness Trace:%a@]"
             (pp_path ~show_root:true) path ) )
  | UNSATISFIABLE -> None
  | UNKNOWN ->
      let msg = Z3.Solver.get_reason_unknown solver in
      if String.starts_with ~prefix:"Sort mismatch" msg then
        fail " UNKNOWN: %s@\n%s" msg (Z3.Solver.to_string solver) () ;
      [%Dbg.info " UNKNOWN: %s" msg] ;
      None

exception Found of (Format.formatter -> unit)

let jobs = ref 1

let validate h =
  [%dbg]
    ~call:(fun {pf} -> pf " num paths: %i" (Iter.length (paths h)))
    ~retn:(fun {pf} dp -> pf "%t" dp)
  @@ fun () ->
  let paths = Iter.(to_array (shuffle (map ~f:to_array (paths h)))) in
  let gen_idx = Iter.(to_gen (0 -- (Array.length paths - 1))) in
  let demux () =
    match gen_idx () with Some i -> i | None -> raise Parany.End_of_input
  in
  let work i = validate_path (mk_xcontext ()) (Iter.of_array paths.(i)) in
  let mux r = match r with Some dp -> raise (Found dp) | None -> () in
  try
    Parany.run !jobs ~demux ~work ~mux ;
    Format.dprintf "Validated: false"
  with Found dp -> dp

let dump_witness = ref None

let validate history =
  match !dump_witness with
  | Some file ->
      Out_channel.with_open_bin file (fun oc ->
          Marshal.to_channel oc history [] ) ;
      Format.dprintf "Witness dumped to %s" file
  | None -> validate history
