(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** LLAIR (Low-Level Analysis Internal Representation) *)

[@@@warning "+9"]

module Loc = Loc
module Typ = Typ
module Reg = Reg
module Exp = Exp
module Function = Function
module Global = Global
module GlobalDefn = GlobalDefn

module Intrinsic = struct
  include Intrinsics
  module Intrinsic_to_String = Bijection.Make (Intrinsics) (String)

  let t_to_name =
    Iter.of_list all
    |> Iter.map ~f:(fun i -> (i, Variants.to_name i))
    |> Intrinsic_to_String.of_iter

  let to_string i = Intrinsic_to_String.find_left i t_to_name

  let of_name s =
    try Some (Intrinsic_to_String.find_right s t_to_name)
    with Not_found -> None

  let pp ppf i = Format.pp_print_string ppf (to_string i)
end

type inst =
  | Move of {reg_exps: (Reg.t * Exp.t) iarray; loc: Loc.t}
  | Load of {reg: Reg.t; ptr: Exp.t; len: Exp.t; loc: Loc.t}
  | Store of {ptr: Exp.t; exp: Exp.t; len: Exp.t; loc: Loc.t}
  | Alloc of {reg: Reg.t; num: Exp.t; len: int; loc: Loc.t}
  | Free of {ptr: Exp.t; loc: Loc.t}
  | Nondet of {reg: Reg.t option; msg: string; loc: Loc.t}
  | Abort of {loc: Loc.t}
  | Intrinsic of
      {reg: Reg.t option; name: Intrinsic.t; args: Exp.t iarray; loc: Loc.t}
[@@deriving compare, equal, hash, sexp]

type cmnd = inst iarray [@@deriving compare, equal, hash, sexp]
type label = string [@@deriving compare, equal, hash, sexp]

type jump = {mutable dst: block; mutable retreating: bool}

and 'a call =
  { mutable callee: 'a
  ; typ: Typ.t
  ; actuals: Exp.t iarray
  ; areturn: Reg.t option
  ; return: jump
  ; throw: jump option
  ; mutable recursive: bool
  ; loc: Loc.t }

and term =
  | Switch of {key: Exp.t; tbl: (Exp.t * jump) iarray; els: jump; loc: Loc.t}
  | Iswitch of {ptr: Exp.t; tbl: jump iarray; loc: Loc.t}
  | Call of func call
  | ICall of Exp.t call
  | Return of {exp: Exp.t option; loc: Loc.t}
  | Throw of {exc: Exp.t; loc: Loc.t}
  | Unreachable

and block =
  { lbl: label
  ; cmnd: cmnd
  ; term: term
  ; mutable parent: func
  ; mutable sort_index: int }

and func =
  { name: Function.t
  ; formals: Reg.t iarray
  ; freturn: Reg.t option
  ; fthrow: Reg.t
  ; locals: Reg.Set.t
  ; entry: block
  ; loc: Loc.t }

(* compare *)

(* functions are uniquely identified by [name] *)
let compare_func x y = if x == y then 0 else Function.compare x.name y.name
let equal_func x y = x == y || Function.equal x.name y.name
let hash_fold_func s x = Function.hash_fold_t s x.name

(* blocks in a [t] are uniquely identified by [sort_index] *)
let compare_block x y =
  if x == y then 0 else Int.compare x.sort_index y.sort_index

let equal_block x y = x == y || Int.equal x.sort_index y.sort_index
let hash_fold_block s x = Int.hash_fold_t s x.sort_index

module Compare : sig
  type nonrec jump = jump [@@deriving compare, equal]
  type nonrec 'a call = 'a call [@@deriving compare, equal]
  type nonrec term = term [@@deriving compare, equal]
end
with type jump := jump
 and type 'a call := 'a call
 and type term := term = struct
  type nonrec jump = jump = {mutable dst: block; mutable retreating: bool}
  [@@deriving compare, equal]

  type nonrec 'a call = 'a call =
    { mutable callee: 'a
    ; typ: Typ.t
    ; actuals: Exp.t iarray
    ; areturn: Reg.t option
    ; return: jump
    ; throw: jump option
    ; mutable recursive: bool
    ; loc: Loc.t }
  [@@deriving compare, equal]

  type nonrec term = term =
    | Switch of
        {key: Exp.t; tbl: (Exp.t * jump) iarray; els: jump; loc: Loc.t}
    | Iswitch of {ptr: Exp.t; tbl: jump iarray; loc: Loc.t}
    | Call of func call
    | ICall of Exp.t call
    | Return of {exp: Exp.t option; loc: Loc.t}
    | Throw of {exc: Exp.t; loc: Loc.t}
    | Unreachable
  [@@deriving compare, equal]
end

include Compare

(* hash *)

let hash_fold_jump s {dst; retreating} =
  let s = [%hash_fold: block] s dst in
  let s = [%hash_fold: bool] s retreating in
  s

let hash_fold_call (type callee) hash_fold_callee s
    {callee: callee; typ; actuals; areturn; return; throw; recursive; loc} =
  let s = [%hash_fold: int] s 3 in
  let s = [%hash_fold: callee] s callee in
  let s = [%hash_fold: Typ.t] s typ in
  let s = [%hash_fold: Exp.t iarray] s actuals in
  let s = [%hash_fold: Reg.t option] s areturn in
  let s = [%hash_fold: jump] s return in
  let s = [%hash_fold: jump option] s throw in
  let s = [%hash_fold: bool] s recursive in
  let s = [%hash_fold: Loc.t] s loc in
  s

let hash_fold_term s = function
  | Switch {key; tbl; els; loc} ->
      let s = [%hash_fold: int] s 1 in
      let s = [%hash_fold: Exp.t] s key in
      let s = [%hash_fold: (Exp.t * jump) iarray] s tbl in
      let s = [%hash_fold: jump] s els in
      let s = [%hash_fold: Loc.t] s loc in
      s
  | Iswitch {ptr; tbl; loc} ->
      let s = [%hash_fold: int] s 2 in
      let s = [%hash_fold: Exp.t] s ptr in
      let s = [%hash_fold: jump iarray] s tbl in
      let s = [%hash_fold: Loc.t] s loc in
      s
  | Call call ->
      let s = [%hash_fold: int] s 3 in
      let s = hash_fold_call hash_fold_func s call in
      s
  | ICall call ->
      let s = [%hash_fold: int] s 4 in
      let s = hash_fold_call Exp.hash_fold_t s call in
      s
  | Return {exp; loc} ->
      let s = [%hash_fold: int] s 5 in
      let s = [%hash_fold: Exp.t option] s exp in
      let s = [%hash_fold: Loc.t] s loc in
      s
  | Throw {exc; loc} ->
      let s = [%hash_fold: int] s 6 in
      let s = [%hash_fold: Exp.t] s exc in
      let s = [%hash_fold: Loc.t] s loc in
      s
  | Unreachable -> [%hash_fold: int] s 7

let hash_func = Hash.of_fold hash_fold_func
let hash_block = Hash.of_fold hash_fold_block
let hash_jump = Hash.of_fold hash_fold_jump
let hash_term = Hash.of_fold hash_fold_term

(* sexp *)

let sexp_cons (hd : Sexp.t) (tl : Sexp.t) =
  match tl with
  | List xs -> Sexp.List (hd :: xs)
  | Atom _ -> Sexp.List [hd; tl]

let sexp_ctor label args = sexp_cons (Sexp.Atom label) args

let sexp_of_jump {dst; retreating} =
  [%sexp {dst: label = dst.lbl; retreating: bool}]

let sexp_of_call (type callee) tag sexp_of_callee
    {callee: callee; typ; actuals; areturn; return; throw; recursive; loc} =
  sexp_ctor tag
    [%sexp
      { callee: callee
      ; typ: Typ.t
      ; actuals: Exp.t iarray
      ; areturn: Reg.t option
      ; return: jump
      ; throw: jump option
      ; recursive: bool
      ; loc: Loc.t }]

let sexp_of_term = function
  | Switch {key; tbl; els; loc} ->
      sexp_ctor "Switch"
        [%sexp
          {key: Exp.t; tbl: (Exp.t * jump) iarray; els: jump; loc: Loc.t}]
  | Iswitch {ptr; tbl; loc} ->
      sexp_ctor "Iswitch" [%sexp {ptr: Exp.t; tbl: jump iarray; loc: Loc.t}]
  | Call call ->
      sexp_of_call "Call" (fun f -> Function.sexp_of_t f.name) call
  | ICall call -> sexp_of_call "ICall" Exp.sexp_of_t call
  | Return {exp; loc} ->
      sexp_ctor "Return" [%sexp {exp: Exp.t option; loc: Loc.t}]
  | Throw {exc; loc} -> sexp_ctor "Throw" [%sexp {exc: Exp.t; loc: Loc.t}]
  | Unreachable -> Sexp.Atom "Unreachable"

let sexp_of_block {lbl; cmnd; term; parent; sort_index} =
  [%sexp
    { lbl: label
    ; cmnd: cmnd
    ; term: term
    ; parent: Function.t = parent.name
    ; sort_index: int }]

let sexp_of_func {name; formals; freturn; fthrow; locals; entry; loc} =
  [%sexp
    { name: Function.t
    ; formals: Reg.t iarray
    ; freturn: Reg.t option
    ; fthrow: Reg.t
    ; locals: Reg.Set.t
    ; entry: block
    ; loc: Loc.t }]

type functions = func Function.Map.t [@@deriving sexp_of]

type program = {globals: GlobalDefn.t iarray; functions: functions}
[@@deriving sexp_of]

(* pp *)

let pp_inst fs inst =
  let pf pp = Format.fprintf fs pp in
  match inst with
  | Move {reg_exps; loc} ->
      let regs, exps = IArray.split reg_exps in
      pf "@[<2>@[%a@]@ := @[%a@];@]\t%a" (IArray.pp ",@ " Reg.pp) regs
        (IArray.pp ",@ " Exp.pp) exps Loc.pp loc
  | Load {reg; ptr; len; loc} ->
      pf "@[<2>%a@ := load %a@ %a;@]\t%a" Reg.pp reg Exp.pp len Exp.pp ptr
        Loc.pp loc
  | Store {ptr; exp; len; loc} ->
      pf "@[<2>store %a@ %a@ %a;@]\t%a" Exp.pp len Exp.pp ptr Exp.pp exp
        Loc.pp loc
  | Alloc {reg; num; len; loc} ->
      pf "@[<2>%a@ := alloc [%a x %i];@]\t%a" Reg.pp reg Exp.pp num len
        Loc.pp loc
  | Free {ptr; loc} -> pf "@[<2>free %a;@]\t%a" Exp.pp ptr Loc.pp loc
  | Nondet {reg; msg; loc} ->
      pf "@[<2>%anondet \"%s\";@]\t%a"
        (Option.pp "%a := " Reg.pp)
        reg msg Loc.pp loc
  | Abort {loc} -> pf "@[<2>abort;@]\t%a" Loc.pp loc
  | Intrinsic {reg; name; args; loc} ->
      pf "@[<2>%aintrinsic@ %a(@,@[<hv>%a@]);@]\t%a"
        (Option.pp "%a := " Reg.pp)
        reg Intrinsic.pp name (IArray.pp ",@ " Exp.pp) args Loc.pp loc

let pp_actuals pp_actual fs actuals =
  Format.fprintf fs "@ (@[%a@])" (IArray.pp ",@ " pp_actual) actuals

let pp_formal fs reg = Reg.pp fs reg

let pp_jump fs {dst; retreating} =
  Format.fprintf fs "@[<2>%s%%%s@]"
    (if retreating then "↑" else "")
    dst.lbl

let pp_call tag pp_callee fs
    {callee; actuals; areturn; return; throw; recursive; loc; _} =
  Format.fprintf fs
    "@[<2>@[<7>%a%s @[<2>%s%a%a@]@]@ @[returnto %a%a;@]@]\t%a"
    (Option.pp "%a := " Reg.pp)
    areturn tag
    (if recursive then "↑" else "")
    pp_callee callee (pp_actuals Exp.pp) actuals pp_jump return
    (Option.pp "@ throwto %a" pp_jump)
    throw Loc.pp loc

let pp_term fs term =
  let pf pp = Format.fprintf fs pp in
  let pp_goto fs jmp = Format.fprintf fs "goto %a;" pp_jump jmp in
  match term with
  | Switch {key; tbl; els; loc} -> (
    match IArray.to_array tbl with
    | [||] -> pf "@[%a@]\t%a" pp_goto els Loc.pp loc
    | [|(z, jmp)|] when Exp.is_false z ->
        pf "@[if %a@;<1 2>%a@ @[else@;<1 2>%a@]@]\t%a" Exp.pp key pp_goto
          els pp_goto jmp Loc.pp loc
    | _ ->
        pf "@[<2>switch %a@ @[%a@ else: %a@]@]\t%a" Exp.pp key
          (IArray.pp "@ " (fun fs (case, jmp) ->
               Format.fprintf fs "%a: %a" Exp.pp case pp_goto jmp ))
          tbl pp_goto els Loc.pp loc )
  | Iswitch {ptr; tbl; loc} ->
      pf "@[<2>iswitch %a@ @[<hv>%a@]@]\t%a" Exp.pp ptr
        (IArray.pp "@ " (fun fs jmp ->
             Format.fprintf fs "%s: %a" jmp.dst.lbl pp_goto jmp ))
        tbl Loc.pp loc
  | Call call -> pp_call "call" (fun fs f -> Function.pp fs f.name) fs call
  | ICall call -> pp_call "icall" Exp.pp fs call
  | Return {exp; loc} ->
      pf "@[<2>return%a@]\t%a" (Option.pp " %a" Exp.pp) exp Loc.pp loc
  | Throw {exc; loc} -> pf "@[<2>throw %a@]\t%a" Exp.pp exc Loc.pp loc
  | Unreachable -> pf "unreachable"

let pp_cmnd = IArray.pp "@ " pp_inst

let pp_block fs {lbl; cmnd; term; parent= _; sort_index} =
  Format.fprintf fs "@[<v 2>%%%s: #%i@ @[<v>%a%t%a@]@]" lbl sort_index
    pp_cmnd cmnd
    (fun fs -> if IArray.is_empty cmnd then () else Format.fprintf fs "@ ")
    pp_term term

(** Initial cyclic values *)

let rec dummy_block =
  { lbl= "dummy"
  ; cmnd= IArray.empty
  ; term= Unreachable
  ; parent= dummy_func
  ; sort_index= 0 }

and dummy_func =
  { name=
      Function.mk
        (Typ.pointer ~elt:(Typ.function_ ~args:IArray.empty ~return:None))
        "dummy"
  ; formals= IArray.empty
  ; freturn= None
  ; fthrow= Reg.mk Typ.ptr 0 "dummy"
  ; locals= Reg.Set.empty
  ; entry= dummy_block
  ; loc= Loc.none }

(** Instructions *)

module Inst = struct
  type t = inst [@@deriving compare, equal, hash, sexp]

  module Tbl = HashTable.Make (struct
    type t = block * inst [@@deriving equal, hash]
  end)

  let pp = pp_inst
  let move ~reg_exps ~loc = Move {reg_exps; loc}
  let load ~reg ~ptr ~len ~loc = Load {reg; ptr; len; loc}
  let store ~ptr ~exp ~len ~loc = Store {ptr; exp; len; loc}
  let alloc ~reg ~num ~len ~loc = Alloc {reg; num; len; loc}
  let free ~ptr ~loc = Free {ptr; loc}
  let nondet ~reg ~msg ~loc = Nondet {reg; msg; loc}
  let abort ~loc = Abort {loc}
  let intrinsic ~reg ~name ~args ~loc = Intrinsic {reg; name; args; loc}

  let loc = function
    | Move {loc; _}
     |Load {loc; _}
     |Store {loc; _}
     |Alloc {loc; _}
     |Free {loc; _}
     |Nondet {loc; _}
     |Abort {loc; _}
     |Intrinsic {loc; _} ->
        loc

  let union_locals inst vs =
    match inst with
    | Move {reg_exps; _} ->
        IArray.fold ~f:(fun (reg, _) vs -> Reg.Set.add reg vs) reg_exps vs
    | Load {reg; _}
     |Alloc {reg; _}
     |Nondet {reg= Some reg; _}
     |Intrinsic {reg= Some reg; _} ->
        Reg.Set.add reg vs
    | Store _ | Free _
     |Nondet {reg= None; _}
     |Abort _
     |Intrinsic {reg= None; _} ->
        vs

  let locals inst = union_locals inst Reg.Set.empty

  let fold_exps inst s ~f =
    match inst with
    | Move {reg_exps; loc= _} ->
        IArray.fold ~f:(fun (_reg, exp) -> f exp) reg_exps s
    | Load {reg= _; ptr; len; loc= _} -> f len (f ptr s)
    | Store {ptr; exp; len; loc= _} -> f len (f exp (f ptr s))
    | Alloc {reg= _; num; len= _; loc= _} -> f num s
    | Free {ptr; loc= _} -> f ptr s
    | Nondet {reg= _; msg= _; loc= _} -> s
    | Abort {loc= _} -> s
    | Intrinsic {reg= _; name= _; args; loc= _} -> IArray.fold ~f args s
end

(** Jumps *)

module Jump = struct
  type t = jump [@@deriving compare, equal, hash, sexp_of]

  let compare x y = compare_block x.dst y.dst
  let equal x y = equal_block x.dst y.dst
  let pp = pp_jump
  let mk lbl = {dst= {dummy_block with lbl}; retreating= false}
end

(** Basic-Block Terminators *)

module Term = struct
  type t = term [@@deriving compare, equal, hash, sexp_of]

  let pp = pp_term

  let invariant ?parent term =
    let@ () = Invariant.invariant [%here] term [%sexp_of: t] in
    match term with
    | Switch _ | Iswitch _ -> assert true
    | Call {typ; actuals; areturn; _} | ICall {typ; actuals; areturn; _}
      -> (
      match typ with
      | Pointer {elt= Function {args; return= retn_typ; _}} ->
          assert (IArray.length args = IArray.length actuals) ;
          assert (Option.is_some retn_typ || Option.is_none areturn)
      | _ -> assert false )
    | Return {exp; _} -> (
      match parent with
      | Some parent ->
          assert (
            Bool.equal (Option.is_some exp) (Option.is_some parent.freturn)
          )
      | None -> assert true )
    | Throw _ | Unreachable -> assert true

  let goto ~dst ~loc =
    Switch {key= Exp.false_; tbl= IArray.empty; els= dst; loc}
    |> check invariant

  let branch ~key ~nzero ~zero ~loc =
    let tbl = IArray.of_array [|(Exp.false_, zero)|] in
    let els = nzero in
    Switch {key; tbl; els; loc} |> check invariant

  let switch ~key ~tbl ~els ~loc =
    Switch {key; tbl; els; loc} |> check invariant

  let iswitch ~ptr ~tbl ~loc = Iswitch {ptr; tbl; loc} |> check invariant

  let call ~name ~typ ~actuals ~areturn ~return ~throw ~loc =
    let cal =
      { callee= {dummy_func with name= Function.mk typ name}
      ; typ
      ; actuals
      ; areturn
      ; return
      ; throw
      ; recursive= false
      ; loc }
    in
    let k = Call cal in
    (k |> check invariant, fun ~callee -> cal.callee <- callee)

  let icall ~callee ~typ ~actuals ~areturn ~return ~throw ~loc =
    ICall
      {callee; typ; actuals; areturn; return; throw; recursive= false; loc}
    |> check invariant

  let return ~exp ~loc = Return {exp; loc} |> check invariant
  let throw ~exc ~loc = Throw {exc; loc} |> check invariant
  let unreachable = Unreachable |> check invariant

  let loc = function
    | Switch {loc; _}
     |Iswitch {loc; _}
     |Call {loc; _}
     |ICall {loc; _}
     |Return {loc; _}
     |Throw {loc; _} ->
        loc
    | Unreachable -> Loc.none

  let union_locals term vs =
    match term with
    | Call {areturn; _} | ICall {areturn; _} ->
        Reg.Set.add_option areturn vs
    | Switch _ | Iswitch _ | Return _ | Throw _ | Unreachable -> vs
end

(** Basic-Blocks *)

module Block = struct
  module T = struct
    type t = block [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  module Map = Map.Make (T)
  module Tbl = HashTable.Make (T)

  let pp = pp_block

  let mk ~lbl ~cmnd ~term =
    { lbl
    ; cmnd
    ; term
    ; parent= dummy_block.parent
    ; sort_index= dummy_block.sort_index }
end

(* Blocks compared by label, which are unique within a function, used to
   compute unique sort_index ids *)
module Block_label = struct
  module T = struct
    type t = block [@@deriving sexp_of]

    let compare x y =
      [%compare: string * Function.t] (x.lbl, x.parent.name)
        (y.lbl, y.parent.name)

    let equal x y =
      [%equal: string * Function.t] (x.lbl, x.parent.name)
        (y.lbl, y.parent.name)

    let hash b = [%hash: string * Function.t] (b.lbl, b.parent.name)
  end

  include T
  module Set = Set.Make (T)
end

module BlockS = HashSet.Make (Block_label)
module BlockQ = HashQueue.Make (Block_label)

(** Functions *)

module Func = struct
  type t = func [@@deriving compare, equal, hash, sexp_of]

  let undefined_entry =
    Block.mk ~lbl:"undefined" ~cmnd:IArray.empty ~term:Term.unreachable

  let mk_undefined ~name ~formals ~freturn ~fthrow ~loc =
    let locals = Reg.Set.empty in
    {name; formals; freturn; fthrow; locals; entry= undefined_entry; loc}

  let is_undefined func = func.entry == undefined_entry

  let fold_cfg func s ~f =
    let seen = BlockS.create 0 in
    let rec fold_cfg_ blk s =
      if not (BlockS.add seen blk) then s
      else
        let s =
          let f j s = fold_cfg_ j.dst s in
          match blk.term with
          | Switch {tbl; els; _} ->
              let s = IArray.fold ~f:(fun (_, j) -> f j) tbl s in
              f els s
          | Iswitch {tbl; _} -> IArray.fold ~f tbl s
          | Call {return; throw; _} | ICall {return; throw; _} ->
              Option.fold ~f throw (f return s)
          | Return _ | Throw _ | Unreachable -> s
        in
        f blk s
    in
    fold_cfg_ func.entry s

  let iter_term func ~f = fold_cfg ~f:(fun blk () -> f blk.term) func ()
  let entry_cfg func = fold_cfg ~f:(fun blk cfg -> blk :: cfg) func []

  let pp_call fs call =
    let {callee; actuals; areturn; _} = call in
    let {name; formals; freturn; _} = callee in
    let pp_arg fs (actual, formal) =
      Format.fprintf fs "@[%a / %a@]" Exp.pp actual Reg.pp formal
    in
    let pp_args fs (actuals, formals) =
      IArray.pp ",@ " pp_arg fs (IArray.combine_exn actuals formals)
    in
    Format.fprintf fs "%a%a(@[%a@])"
      (Option.pp "%a := " pp_arg)
      (Option.map2 (fun a f -> (Exp.reg a, f)) areturn freturn)
      Function.pp name pp_args (actuals, formals)

  let pp fs func =
    let {name; formals; freturn; entry; loc; _} = func in
    let {cmnd; term; sort_index; _} = entry in
    let pp_if cnd str fs = if cnd then Format.fprintf fs str in
    Format.fprintf fs "@[<v>@[<v>%a%a@[<2>%a%a@]%t@]"
      (Option.pp "%a " Typ.pp)
      ( match Function.typ name with
      | Pointer {elt= Function {return; _}} -> return
      | _ -> None )
      (Option.pp " %a := " Reg.pp)
      freturn Function.pp name (pp_actuals pp_formal) formals
      (fun fs ->
        if is_undefined func then Format.fprintf fs " #%i@]" sort_index
        else
          let cfg =
            List.sort ~cmp:Block.compare (List.tl_exn (entry_cfg func))
          in
          Format.fprintf fs " { #%i %a@;<1 4>@[<v>%a@ %a@]%t%a@]@ }"
            sort_index Loc.pp loc pp_cmnd cmnd Term.pp term
            (pp_if (not (List.is_empty cfg)) "@ @   ")
            (List.pp "@\n@\n  " Block.pp)
            cfg )

  let invariant func =
    assert (func == func.entry.parent) ;
    let@ () = Invariant.invariant [%here] func [%sexp_of: t] in
    try
      match Function.typ func.name with
      | Pointer {elt= Function {return; _}; _} ->
          assert (
            not
              (Iter.contains_dup
                 (Iter.of_list (entry_cfg func))
                 ~cmp:(fun b1 b2 -> String.compare b1.lbl b2.lbl)) ) ;
          assert (
            Bool.equal (Option.is_some return) (Option.is_some func.freturn)
          ) ;
          iter_term func ~f:(fun term -> Term.invariant ~parent:func term)
      | _ -> assert false
    with exc ->
      let bt = Printexc.get_raw_backtrace () in
      [%Trace.info "%a" pp func] ;
      Printexc.raise_with_backtrace exc bt

  let find name functions =
    Function.Map.find (Function.counterfeit name) functions

  let mk ~name ~formals ~freturn ~fthrow ~entry ~cfg ~loc =
    let locals =
      let locals_cmnd locals cmnd =
        IArray.fold_right ~f:Inst.union_locals cmnd locals
      in
      let locals_block block locals =
        locals_cmnd (Term.union_locals block.term locals) block.cmnd
      in
      IArray.fold ~f:locals_block cfg (locals_block entry Reg.Set.empty)
    in
    let func = {name; formals; freturn; fthrow; locals; entry; loc} in
    let resolve_parent_and_jumps block =
      block.parent <- func ;
      let lookup cfg lbl : block =
        Iter.find_exn (IArray.to_iter cfg) ~f:(fun k ->
            String.equal lbl k.lbl )
      in
      let set_dst jmp = jmp.dst <- lookup cfg jmp.dst.lbl in
      match block.term with
      | Switch {tbl; els; _} ->
          IArray.iter tbl ~f:(fun (_, jmp) -> set_dst jmp) ;
          set_dst els
      | Iswitch {tbl; _} -> IArray.iter tbl ~f:set_dst
      | Call {return; throw; _} | ICall {return; throw; _} ->
          set_dst return ;
          Option.iter throw ~f:set_dst
      | Return _ | Throw _ | Unreachable -> ()
    in
    let elim_jumps_to_jumps block =
      let rec find_dst retreating jmp =
        match jmp.dst.term with
        | Switch {tbl; els; _}
          when IArray.is_empty tbl && IArray.is_empty jmp.dst.cmnd ->
            find_dst (retreating || els.retreating) els
        | _ -> jmp
      in
      let set_dst jmp =
        let tgt = find_dst jmp.retreating jmp in
        if tgt != jmp then (
          jmp.dst <- tgt.dst ;
          jmp.retreating <- tgt.retreating )
      in
      match block.term with
      | Switch {tbl; els; _} ->
          IArray.iter tbl ~f:(fun (_, jmp) -> set_dst jmp) ;
          set_dst els
      | Iswitch {tbl; _} -> IArray.iter tbl ~f:set_dst
      | Call {return; throw; _} | ICall {return; throw; _} ->
          set_dst return ;
          Option.iter throw ~f:set_dst
      | Return _ | Throw _ | Unreachable -> ()
    in
    resolve_parent_and_jumps entry ;
    IArray.iter cfg ~f:resolve_parent_and_jumps ;
    elim_jumps_to_jumps entry ;
    IArray.iter cfg ~f:elim_jumps_to_jumps ;
    func |> check invariant
end

(** Derived meta-data *)

module FuncQ = HashQueue.Make (Func)

let set_derived_metadata functions =
  let compute_roots functions =
    let roots = FuncQ.create () in
    Function.Map.iter functions ~f:(fun func ->
        FuncQ.enqueue_back_exn roots func func ) ;
    Function.Map.iter functions ~f:(fun func ->
        Func.iter_term func ~f:(fun term ->
            match term with
            | Call {callee; _} ->
                FuncQ.remove roots callee |> (ignore : [> ] -> unit)
            | _ -> () ) ) ;
    roots
  in
  let topsort roots =
    let tips_to_roots = BlockQ.create () in
    let rec visit ancestors func src =
      if BlockQ.mem tips_to_roots src then ()
      else
        let ancestors = Block_label.Set.add src ancestors in
        let jump jmp =
          if Block_label.Set.mem jmp.dst ancestors then
            jmp.retreating <- true
          else visit ancestors func jmp.dst
        in
        ( match src.term with
        | Switch {tbl; els; _} ->
            IArray.iter tbl ~f:(fun (_, jmp) -> jump jmp) ;
            jump els
        | Iswitch {tbl; _} -> IArray.iter tbl ~f:jump
        | Call ({callee; return; throw; _} as cal) ->
            if Block_label.Set.mem callee.entry ancestors then
              cal.recursive <- true
            else visit ancestors func callee.entry ;
            jump return ;
            Option.iter ~f:jump throw
        | ICall ({return; throw; _} as call) ->
            (* conservatively assume all indirect calls are recursive *)
            call.recursive <- true ;
            jump return ;
            Option.iter ~f:jump throw
        | Return _ | Throw _ | Unreachable -> () ) ;
        BlockQ.enqueue_back_exn tips_to_roots src ()
    in
    FuncQ.iter roots ~f:(fun root ->
        visit Block_label.Set.empty root root.entry ) ;
    tips_to_roots
  in
  let set_sort_indices tips_to_roots =
    let index = ref (BlockQ.length tips_to_roots) in
    BlockQ.iteri tips_to_roots ~f:(fun ~key:block ~data:_ ->
        block.sort_index <- !index ;
        index := !index - 1 )
  in
  let functions =
    List.fold functions Function.Map.empty ~f:(fun func m ->
        Function.Map.add_exn ~key:func.name ~data:func m )
  in
  let roots = compute_roots functions in
  let tips_to_roots = topsort roots in
  set_sort_indices tips_to_roots ;
  functions

module Program = struct
  type t = program

  let invariant pgm =
    let@ () = Invariant.invariant [%here] pgm [%sexp_of: program] in
    assert (
      not
        (Iter.contains_dup (IArray.to_iter pgm.globals) ~cmp:(fun g1 g2 ->
             Global.compare g1.name g2.name )) )

  let mk ~globals ~functions =
    { globals= IArray.of_list_rev globals
    ; functions= set_derived_metadata functions }
    |> check invariant

  let pp fs {globals; functions} =
    Format.fprintf fs "@[<v>@[%a@]@ @ @ @[%a@]@]"
      (IArray.pp "@\n@\n" GlobalDefn.pp)
      globals
      (List.pp "@\n@\n" Func.pp)
      ( Function.Map.values functions
      |> Iter.to_list
      |> List.sort ~cmp:(fun x y -> compare_block x.entry y.entry) )
end
