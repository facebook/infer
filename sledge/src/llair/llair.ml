(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** LLAIR (Low-Level Analysis Internal Representation) *)

[@@@warning "+missing-record-field-pattern"]

module Loc = Loc
module Typ = Typ
module Reg = Reg
module Exp = Exp
module Function = Function
module Global = Global
module GlobalDefn = GlobalDefn

let cct_schedule_points = ref false

module Bij (I : sig
  type t [@@deriving compare, equal, sexp_of, enumerate]
end) =
struct
  include I
  module B = Bijection.Make (I) (String)

  let bij =
    Iter.of_list I.all
    |> Iter.map ~f:(fun i -> (i, Sexp.to_string (I.sexp_of_t i)))
    |> B.of_iter

  let to_name i = B.find_left i bij
  let of_name s = try Some (B.find_right s bij) with Not_found -> None
  let pp ppf i = Format.pp_print_string ppf (to_name i)
end

module Builtin = Bij (Builtins)
module Intrinsic = Bij (Intrinsics)

type inst =
  | Move of {reg_exps: (Reg.t * Exp.t) iarray; loc: Loc.t}
  | Load of {reg: Reg.t; ptr: Exp.t; len: Exp.t; loc: Loc.t}
  | Store of {ptr: Exp.t; exp: Exp.t; len: Exp.t; loc: Loc.t}
  | AtomicRMW of {reg: Reg.t; ptr: Exp.t; exp: Exp.t; len: Exp.t; loc: Loc.t}
  | AtomicCmpXchg of
      { reg: Reg.t
      ; ptr: Exp.t
      ; cmp: Exp.t
      ; exp: Exp.t
      ; len: Exp.t
      ; len1: Exp.t
      ; loc: Loc.t }
  | Alloc of {reg: Reg.t; num: Exp.t; len: int; loc: Loc.t}
  | Free of {ptr: Exp.t; loc: Loc.t}
  | Nondet of {reg: Reg.t option; msg: string; loc: Loc.t}
  | Builtin of
      {reg: Reg.t option; name: Builtin.t; args: Exp.t iarray; loc: Loc.t}
[@@deriving compare, equal, sexp_of]

type cmnd = inst iarray [@@deriving compare, equal, sexp_of]
type label = string [@@deriving compare, equal, sexp]

type jump = {mutable dst: block; mutable retreating: bool}

and callee =
  | Direct of func
  | Indirect of {ptr: Exp.t; candidates: func iarray}
  | Intrinsic of Intrinsic.t

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
  | Call of callee call
  | Return of {exp: Exp.t option; loc: Loc.t}
  | Throw of {exc: Exp.t; loc: Loc.t}
  | Abort of {loc: Loc.t}
  | Unreachable

and block =
  { lbl: label
  ; cmnd: cmnd
  ; term: term
  ; mutable parent: func
  ; mutable sort_index: int
  ; mutable checkpoint_dists: int Int.Map.t }

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

(* blocks in a [t] are uniquely identified by [sort_index] *)
let compare_block x y =
  if x == y then 0 else Int.compare x.sort_index y.sort_index

let equal_block x y = x == y || Int.equal x.sort_index y.sort_index

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

  type nonrec callee = callee =
    | Direct of func
    | Indirect of {ptr: Exp.t; candidates: func iarray}
    | Intrinsic of Intrinsic.t
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
    | Call of callee call
    | Return of {exp: Exp.t option; loc: Loc.t}
    | Throw of {exc: Exp.t; loc: Loc.t}
    | Abort of {loc: Loc.t}
    | Unreachable
  [@@deriving compare, equal]
end

include Compare

(* sexp *)

let sexp_cons (hd : Sexp.t) (tl : Sexp.t) =
  match tl with
  | List xs -> Sexp.List (hd :: xs)
  | Atom _ -> Sexp.List [hd; tl]

let sexp_ctor label args = sexp_cons (Sexp.Atom label) args

let sexp_of_jump {dst; retreating} =
  [%sexp {dst: label = dst.lbl; retreating: bool}]

let sexp_of_callee = function
  | Direct func -> sexp_ctor "Direct" [%sexp (func.name : Function.t)]
  | Indirect {ptr; candidates= _} ->
      sexp_ctor "Indirect" [%sexp (ptr : Exp.t)]
  | Intrinsic intr -> sexp_ctor "Intrinsic" [%sexp (intr : Intrinsic.t)]

let sexp_of_call
    {callee; typ; actuals; areturn; return; throw; recursive; loc} =
  sexp_ctor "Call"
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
  | Call call -> sexp_of_call call
  | Return {exp; loc} ->
      sexp_ctor "Return" [%sexp {exp: Exp.t option; loc: Loc.t}]
  | Throw {exc; loc} -> sexp_ctor "Throw" [%sexp {exc: Exp.t; loc: Loc.t}]
  | Abort {loc} -> sexp_ctor "Abort" [%sexp {loc: Loc.t}]
  | Unreachable -> Sexp.Atom "Unreachable"

let sexp_of_block {lbl; cmnd; term; parent; sort_index; checkpoint_dists} =
  [%sexp
    { lbl: label
    ; cmnd: cmnd
    ; term: term
    ; parent: Function.t = parent.name
    ; sort_index: int
    ; checkpoint_dists: int Int.Map.t }]

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
  | AtomicRMW {reg; ptr; exp; len; loc} ->
      pf "@[<2>%a@ := atomic_rmw %a@ %a@ %a;@]\t%a" Reg.pp reg Exp.pp len
        Exp.pp ptr Exp.pp exp Loc.pp loc
  | AtomicCmpXchg {reg; ptr; cmp; exp; len; len1; loc} ->
      pf "@[<2>%a@ := atomic_cmpxchg %a,%a@ %a@ %a@ %a;@]\t%a" Reg.pp reg
        Exp.pp len Exp.pp len1 Exp.pp ptr Exp.pp cmp Exp.pp exp Loc.pp loc
  | Alloc {reg; num; len; loc} ->
      pf "@[<2>%a@ := alloc [%a x %i];@]\t%a" Reg.pp reg Exp.pp num len
        Loc.pp loc
  | Free {ptr; loc} -> pf "@[<2>free %a;@]\t%a" Exp.pp ptr Loc.pp loc
  | Nondet {reg; msg; loc} ->
      pf "@[<2>%anondet \"%s\";@]\t%a"
        (Option.pp "%a := " Reg.pp)
        reg msg Loc.pp loc
  | Builtin {reg; name; args; loc} ->
      pf "@[<2>%abuiltin@ %a(@,@[<hv>%a@]);@]\t%a"
        (Option.pp "%a := " Reg.pp)
        reg Builtin.pp name (IArray.pp ",@ " Exp.pp) args Loc.pp loc

let pp_actuals pp_actual fs actuals =
  Format.fprintf fs "@ (@[%a@])" (IArray.pp ",@ " pp_actual) actuals

let pp_formal fs reg = Reg.pp fs reg

let pp_jump fs {dst; retreating} =
  Format.fprintf fs "@[<2>%s%%%s@]" (if retreating then "↑" else "") dst.lbl

let pp_callee fs = function
  | Direct f -> Function.pp fs f.name
  | Indirect {ptr; candidates= _} -> Exp.pp fs ptr
  | Intrinsic i -> Intrinsic.pp fs i

let pp_call fs
    {callee; typ= _; actuals; areturn; return; throw; recursive; loc} =
  Format.fprintf fs
    "@[<2>@[<7>%acall @[<2>%s%a%a@]@]@ @[returnto %a%a;@]@]\t%a"
    (Option.pp "%a := " Reg.pp)
    areturn
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
               Format.fprintf fs "%a: %a" Exp.pp case pp_goto jmp ) )
          tbl pp_goto els Loc.pp loc )
  | Iswitch {ptr; tbl; loc} ->
      pf "@[<2>iswitch %a@ @[<hv>%a@]@]\t%a" Exp.pp ptr
        (IArray.pp "@ " (fun fs jmp ->
             Format.fprintf fs "%s: %a" jmp.dst.lbl pp_goto jmp ) )
        tbl Loc.pp loc
  | Call call -> pp_call fs call
  | Return {exp; loc} ->
      pf "@[<2>return%a@]\t%a" (Option.pp " %a" Exp.pp) exp Loc.pp loc
  | Throw {exc; loc} -> pf "@[<2>throw %a@]\t%a" Exp.pp exc Loc.pp loc
  | Abort {loc} -> pf "@[<2>abort@]\t%a" Loc.pp loc
  | Unreachable -> pf "unreachable"

let pp_cmnd = IArray.pp "@ " pp_inst

let pp_block fs {lbl; cmnd; term; parent= _; sort_index; checkpoint_dists= _}
    =
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
  ; sort_index= 0
  ; checkpoint_dists= Int.Map.empty }

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
  type t = inst [@@deriving compare, equal, sexp_of]

  let pp = pp_inst
  let move ~reg_exps ~loc = Move {reg_exps; loc}
  let load ~reg ~ptr ~len ~loc = Load {reg; ptr; len; loc}
  let store ~ptr ~exp ~len ~loc = Store {ptr; exp; len; loc}

  let atomic_rmw ~reg ~ptr ~exp ~len ~loc =
    AtomicRMW {reg; ptr; exp; len; loc}

  let atomic_cmpxchg ~reg ~ptr ~cmp ~exp ~len ~len1 ~loc =
    AtomicCmpXchg {reg; ptr; cmp; exp; len; len1; loc}

  let alloc ~reg ~num ~len ~loc = Alloc {reg; num; len; loc}
  let free ~ptr ~loc = Free {ptr; loc}
  let nondet ~reg ~msg ~loc = Nondet {reg; msg; loc}
  let builtin ~reg ~name ~args ~loc = Builtin {reg; name; args; loc}

  let loc = function
    | Move {loc; _}
     |Load {loc; _}
     |Store {loc; _}
     |AtomicRMW {loc; _}
     |AtomicCmpXchg {loc; _}
     |Alloc {loc; _}
     |Free {loc; _}
     |Nondet {loc; _}
     |Builtin {loc; _} ->
        loc

  let union_locals inst vs =
    match inst with
    | Move {reg_exps; _} ->
        IArray.fold ~f:(fun (reg, _) vs -> Reg.Set.add reg vs) reg_exps vs
    | Load {reg; _}
     |AtomicRMW {reg; _}
     |AtomicCmpXchg {reg; _}
     |Alloc {reg; _}
     |Nondet {reg= Some reg; _}
     |Builtin {reg= Some reg; _} ->
        Reg.Set.add reg vs
    | Store _ | Free _ | Nondet {reg= None; _} | Builtin {reg= None; _} ->
        vs

  let locals inst = union_locals inst Reg.Set.empty

  let fold_exps inst s ~f =
    match inst with
    | Move {reg_exps; loc= _} ->
        IArray.fold ~f:(fun (_reg, exp) -> f exp) reg_exps s
    | Load {reg= _; ptr; len; loc= _} -> f len (f ptr s)
    | Store {ptr; exp; len; loc= _} -> f len (f exp (f ptr s))
    | AtomicRMW {reg= _; ptr; exp; len; loc= _} -> f len (f exp (f ptr s))
    | AtomicCmpXchg {reg= _; ptr; cmp; exp; len; len1; loc= _} ->
        f len1 (f len (f exp (f cmp (f ptr s))))
    | Alloc {reg= _; num; len= _; loc= _} -> f num s
    | Free {ptr; loc= _} -> f ptr s
    | Nondet {reg= _; msg= _; loc= _} -> s
    | Builtin {reg= _; name= _; args; loc= _} -> IArray.fold ~f args s
end

(** Jumps *)

module Jump = struct
  type t = jump [@@deriving compare, equal, sexp_of]

  let compare x y = compare_block x.dst y.dst
  let equal x y = equal_block x.dst y.dst
  let pp = pp_jump
  let mk lbl = {dst= {dummy_block with lbl}; retreating= false}
end

(** Basic-Block Terminators *)

module Term = struct
  type t = term [@@deriving compare, equal, sexp_of]

  let pp = pp_term
  let pp_callee = pp_callee

  let invariant ?parent term =
    let@ () = Invariant.invariant [%here] term [%sexp_of: t] in
    match term with
    | Switch _ | Iswitch _ -> assert true
    | Call {callee; typ; actuals; areturn; _} -> (
        ( match typ with
        | Pointer {elt= Function {args; return= retn_typ; _}} ->
            assert (IArray.length args = IArray.length actuals) ;
            assert (Option.is_some retn_typ || Option.is_none areturn)
        | _ -> assert false ) ;
        match callee with
        | Intrinsic `sledge_thread_create ->
            assert (IArray.length actuals = 2) ;
            assert (Option.is_some areturn)
        | Intrinsic `sledge_thread_resume ->
            assert (IArray.length actuals = 1) ;
            assert (Option.is_none areturn)
        | Intrinsic `sledge_thread_join ->
            assert (IArray.length actuals = 1) ;
            assert (Option.is_some areturn)
        | Direct _ | Indirect _ -> assert true )
    | Return {exp; _} -> (
      match parent with
      | Some parent ->
          assert (
            Bool.equal (Option.is_some exp) (Option.is_some parent.freturn) )
      | None -> assert true )
    | Throw _ | Abort _ | Unreachable -> assert true

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
      { callee= Direct {dummy_func with name= Function.mk typ name}
      ; typ
      ; actuals
      ; areturn
      ; return
      ; throw
      ; recursive= false
      ; loc }
    in
    let k = Call cal in
    (k |> check invariant, fun ~callee -> cal.callee <- Direct callee)

  let icall ~callee ~typ ~actuals ~areturn ~return ~throw ~loc =
    let cal =
      { callee= Indirect {ptr= callee; candidates= IArray.empty}
      ; typ
      ; actuals
      ; areturn
      ; return
      ; throw
      ; recursive= false
      ; loc }
    in
    let k = Call cal in
    ( k |> check invariant
    , fun ~candidates -> cal.callee <- Indirect {ptr= callee; candidates} )

  let intrinsic ~callee ~typ ~actuals ~areturn ~return ~throw ~loc =
    Call
      { callee= Intrinsic callee
      ; typ
      ; actuals
      ; areturn
      ; return
      ; throw
      ; recursive= false
      ; loc }
    |> check invariant

  let return ~exp ~loc = Return {exp; loc} |> check invariant
  let throw ~exc ~loc = Throw {exc; loc} |> check invariant
  let abort ~loc = Abort {loc} |> check invariant
  let unreachable = Unreachable |> check invariant

  let loc = function
    | Switch {loc; _}
     |Iswitch {loc; _}
     |Call {loc; _}
     |Return {loc; _}
     |Throw {loc; _}
     |Abort {loc; _} ->
        loc
    | Unreachable -> Loc.none

  let union_locals term vs =
    match term with
    | Call {areturn; _} -> Reg.Set.add_option areturn vs
    | Switch _ | Iswitch _ | Return _ | Throw _ | Abort _ | Unreachable ->
        vs
end

(** Basic-Blocks *)

module Block = struct
  module T = struct
    type t = block [@@deriving compare, equal, sexp_of]

    let hash = Poly.hash
  end

  include T
  module Map = Map.Make (T)
  module Tbl = HashTable.Make (T)

  let pp = pp_block

  let pp_ident fs b =
    Format.fprintf fs "%a#%s" Function.pp b.parent.name b.lbl

  let mk ~lbl ~cmnd ~term = {dummy_block with lbl; cmnd; term}
end

type ip = {block: block; index: int} [@@deriving compare, equal, sexp_of]

module IP = struct
  type t = ip [@@deriving compare, equal, sexp_of]

  let mk block = {block; index= 0}
  let succ {block; index} = {block; index= index + 1}

  let inst {block; index} =
    if index < IArray.length block.cmnd then
      Some (IArray.get block.cmnd index)
    else None

  let block ip = ip.block
  let index ip = ip.index

  let loc ip =
    match inst ip with
    | Some i -> Inst.loc i
    | None -> Term.loc (block ip).term

  let is_schedule_point ip =
    if !cct_schedule_points then
      match inst ip with
      | Some (Builtin {name= `cct_point; _}) -> true
      | _ -> false
    else
      match inst ip with
      | Some (Load _ | Store _ | AtomicRMW _ | AtomicCmpXchg _ | Free _) ->
          true
      | Some (Move _ | Alloc _ | Nondet _) -> false
      | Some (Builtin {name; _}) -> (
        match name with
        | `calloc | `malloc | `mallocx | `nallocx | `cct_point -> false
        | `_ZN5folly13usingJEMallocEv | `aligned_alloc | `dallocx
         |`mallctl | `mallctlbymib | `mallctlnametomib
         |`malloc_stats_print | `malloc_usable_size | `memcpy | `memmove
         |`memset | `posix_memalign | `rallocx | `realloc | `sallocx
         |`sdallocx | `strlen | `xallocx ->
            true )
      | None -> (
        match ip.block.term with
        | Call {callee= Direct f; _} -> (
          match Function.name f.name with
          | "sledge_thread_join" -> true
          | _ -> false )
        | _ -> false )

  let pp ppf {block; index} =
    Format.fprintf ppf "#%i%t" block.sort_index (fun ppf ->
        if index <> 0 then Format.fprintf ppf "+%i" index )

  module Tbl = HashTable.Make (struct
    type t = ip [@@deriving equal]

    let hash = Poly.hash
  end)
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

    let hash b = Poly.hash (b.lbl, b.parent.name)
  end

  include T
  module Set = Set.Make (T)
end

module BlockS = HashSet.Make (Block_label)
module BlockQ = HashQueue.Make (Block_label)

(** Functions *)

module Func = struct
  type t = func [@@deriving compare, equal, sexp_of]

  let hash f = Poly.hash f.name

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
          | Call {return; throw; _} -> Option.fold ~f throw (f return s)
          | Return _ | Throw _ | Abort _ | Unreachable -> s
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
      IArray.pp ",@ " pp_arg fs (IArray.combine actuals formals)
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
                 ~cmp:(fun b1 b2 -> String.compare b1.lbl b2.lbl) ) ) ;
          assert (
            Bool.equal (Option.is_some return) (Option.is_some func.freturn) ) ;
          iter_term func ~f:(fun term -> Term.invariant ~parent:func term)
      | _ -> assert false
    with exc ->
      let bt = Printexc.get_raw_backtrace () in
      [%Dbg.info " %a" pp func] ;
      Printexc.raise_with_backtrace exc bt

  let find name functions =
    Function.Map.find (Function.counterfeit name) functions

  let lookup cfg lbl =
    Iter.find_exn (IArray.to_iter cfg) ~f:(fun k -> String.equal lbl k.lbl)

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
    let seen = BlockS.create (IArray.length cfg) in
    let rec resolve_parent_and_jumps ancestors src =
      src.parent <- func ;
      BlockS.add seen src |> ignore ;
      let ancestors = Block_label.Set.add src ancestors in
      let jump jmp =
        let dst = lookup cfg jmp.dst.lbl in
        if Block_label.Set.mem dst ancestors then (
          jmp.dst <- dst ;
          jmp.retreating <- true ;
          jmp )
        else if BlockS.mem seen dst then (
          jmp.dst <- dst ;
          jmp )
        else
          match resolve_parent_and_jumps ancestors dst with
          | None ->
              jmp.dst <- dst ;
              jmp
          | Some tgt ->
              jmp.dst <- tgt.dst ;
              jmp.retreating <- tgt.retreating ;
              tgt
      in
      let jump' jmp = jump jmp |> ignore in
      match src.term with
      | Switch {tbl; els; _} ->
          IArray.iter ~f:(fun (_, jmp) -> jump' jmp) tbl ;
          let tgt = jump els in
          if IArray.is_empty tbl && IArray.is_empty src.cmnd then Some tgt
          else None
      | Iswitch {tbl; _} ->
          IArray.iter ~f:jump' tbl ;
          None
      | Call {return; throw; _} ->
          jump' return ;
          Option.iter ~f:jump' throw ;
          None
      | Return _ | Throw _ | Abort _ | Unreachable -> None
    in
    resolve_parent_and_jumps Block_label.Set.empty entry |> ignore ;
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
            | Call {callee= Direct f; _} -> FuncQ.remove roots f |> ignore
            | _ -> () ) ) ;
    roots
  in
  let topsort roots =
    let tips_to_roots = BlockQ.create () in
    let rec visit ancestors src =
      if BlockQ.mem tips_to_roots src then ()
      else
        let ancestors = Block_label.Set.add src ancestors in
        let jump jmp =
          if jmp.retreating then () else visit ancestors jmp.dst
        in
        ( match src.term with
        | Switch {tbl; els; _} ->
            IArray.iter tbl ~f:(fun (_, jmp) -> jump jmp) ;
            jump els
        | Iswitch {tbl; _} -> IArray.iter tbl ~f:jump
        | Call ({callee; return; throw; _} as call) ->
            ( match callee with
            | Direct f when not (Block_label.Set.mem f.entry ancestors) ->
                visit ancestors f.entry
            | Direct _ | Indirect _ ->
                (* conservatively assume all indirect calls are recursive *)
                call.recursive <- true
            | Intrinsic _ -> () ) ;
            jump return ;
            Option.iter ~f:jump throw
        | Return _ | Throw _ | Abort _ | Unreachable -> () ) ;
        BlockQ.enqueue_back_exn tips_to_roots src ()
    in
    FuncQ.iter roots ~f:(fun root ->
        visit Block_label.Set.empty root.entry ) ;
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
             Global.compare g1.name g2.name ) ) )

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

  (** An [Abstr_stack.t] abstracts some concrete stack(s) for the CFL
      reachability problem of the distance computations below. It is
      conceptually a set of "spare" close parentheses ([Abstr_stack.T.t])
      corresponding to some unmatched open parentheses, but is implemented
      as a hash table from functions to sets of return sites (i.e. of calls
      to that function) to support quick/easy lookup *)
  module Abstr_stack = struct
    let empty () : BlockS.t Function.Tbl.t = Function.Tbl.create ()

    let add k callee =
      Function.Tbl.find_or_add k callee ~default:(fun () -> BlockS.create 0)
      |> BlockS.insert

    let return_sites k callee =
      match Function.Tbl.find k callee with
      | None -> Iter.empty
      | Some rets -> BlockS.to_iter rets
  end

  (** Join some [new_dists] map (from blocks to ints) onto an [old_dists]
      map, if one is given. Note that [merge_endo] is important here,
      ensuring that [y == join_dists x (Some y)] holds whenever the old
      distances [y] subsume the new distances [x]. *)
  let join_dists new_dists old_dists_opt =
    match old_dists_opt with
    | None -> new_dists
    | Some old_dists ->
        Block.Map.merge_endo old_dists new_dists ~f:(fun _ -> function
          | `Left d | `Right d -> Some d | `Both (d, d') -> Some (min d d') )

  let reachable_dists ~dp_path ?(preceding_stack = Abstr_stack.empty ())
      init predicate =
    [%dbg]
      ~call:(fun {pf} -> pf "%t" dp_path)
      ~retn:(fun {pf} (dists, _ctx) ->
        pf "@[<v 2>%t@]@\n@\n" (fun ppf ->
            let pp = Format.fprintf ppf "@ %a (%i)" Block.pp_ident in
            Block.Map.iteri dists ~f:(fun ~key ~data -> pp key data) ) )
    @@ fun () ->
    (* [intermediate_dists] maps [dst] blocks to [src] blocks to integers,
       such that [intermediate_dists(dst)(src)] is the distance from src to
       dst *)
    let intermediate_dists = Block.Tbl.create () in
    let open_calls = Abstr_stack.empty () in
    let rec visit ~predicate ~curr_block dists stack =
      let old_dists = Block.Tbl.find intermediate_dists curr_block in
      let new_dists =
        Block.Map.(map ~f:succ dists |> add ~key:curr_block ~data:0)
      in
      let joined_dists = join_dists new_dists old_dists in
      if Option.exists old_dists ~f:(( == ) joined_dists) then ()
      else (
        Block.Tbl.set intermediate_dists ~key:curr_block ~data:joined_dists ;
        if predicate curr_block then
          List.iter stack ~f:(fun (return_site, callee) ->
              Abstr_stack.add open_calls callee return_site )
          (* search has completed, so we return with an abstract stack
             containing all as-yet-unmatched call frames on the stack *)
        else
          let dists = Block.Tbl.find_exn intermediate_dists curr_block in
          let visit_target k tgt =
            visit ~predicate ~curr_block:tgt dists k
          in
          let jump jmp = visit_target stack jmp.dst in
          match curr_block.term with
          | Switch {tbl; els; _} ->
              IArray.iter tbl ~f:(snd >> jump) ;
              jump els
          | Iswitch {tbl; _} -> IArray.iter tbl ~f:jump
          | Call {callee; return; _} -> (
            match callee with
            | Direct f ->
                visit_target ((return.dst, f.name) :: stack) f.entry
            | Intrinsic _ -> jump return
            | Indirect {candidates; _} ->
                IArray.iter candidates ~f:(fun f ->
                    visit_target ((return.dst, f.name) :: stack) f.entry ) )
          | Return _ -> (
            match stack with
            | (ret, _) :: stack -> visit_target stack ret
            | [] ->
                Abstr_stack.return_sites preceding_stack
                  curr_block.parent.name (visit_target [])
                (* empty stack indicates we're returning from one checkpoint
                   function without having reached the next -- no
                   sparse-trace-compatible executions down that path *) )
          | Throw _ | Abort _ | Unreachable -> () )
    in
    visit ~predicate ~curr_block:init Block.Map.empty [] ;
    Block.Tbl.fold intermediate_dists None ~f:(fun ~key ~data dists ->
        if predicate key then Some (join_dists data dists) else dists )
    |> Option.value ~default:Block.Map.empty
    |> fun x -> (x, open_calls)

  let record_dists ~idx dists ~dp_path =
    if Block.Map.is_empty dists then Error dp_path
    else
      Ok
        (Block.Map.iteri dists ~f:(fun ~key:blk ~data ->
             blk.checkpoint_dists <-
               Int.Map.add blk.checkpoint_dists ~key:idx ~data ) )

  (** Compute and record distances between two blocks. *)
  let record_block_dists ~idx prev next =
    let dp_path ppf =
      Format.fprintf ppf "from %a to %a" Block.pp_ident prev Block.pp_ident
        next
    in
    let dists, stack = reachable_dists ~dp_path prev (Block.equal next) in
    Result.map (fun _ -> stack) (record_dists ~idx ~dp_path dists)

  (** Compute and record distances between return sites of one function and
      any location in another. *)
  let record_fn_dists ~idx ~preceding_stack prev next =
    let dp_path ppf =
      Format.fprintf ppf "from retn(%a) to %a" Function.pp prev Function.pp
        next
    in
    Iter.fold (Abstr_stack.return_sites preceding_stack prev) None
      ~f:(fun ret_site dists ->
        let pred b = Function.(equal next b.parent.name) in
        let dists', _ =
          reachable_dists ~preceding_stack ~dp_path ret_site pred
        in
        Some (join_dists dists' dists) )
    |> Option.value ~default:Block.Map.empty
    |> record_dists ~dp_path ~idx

  (** Compute and record distances from a block to any return site of that
      block's procedure *)
  let record_dists_to_exit ~idx ~preceding_stack block =
    let dp_path ppf =
      Format.fprintf ppf "from %a to exit(%a)" Block.pp_ident block
        Function.pp block.parent.name
    in
    let rets =
      Abstr_stack.return_sites preceding_stack block.parent.name
      |> IArray.of_iter
    in
    let dists, _ =
      reachable_dists ~preceding_stack ~dp_path block (fun b ->
          IArray.mem b rets ~eq:Block_label.equal )
    in
    record_dists ~idx ~dp_path dists

  (** Compute static distance heuristics for each step along some
      [src_trace] and [snk_trace], starting from [entry]. Traces are as
      defined in [Goal.Sparse_trace], so we compute distances between: (1)
      successive calls along the [src_trace]; (2) the last call of
      [src_trace] and its exit; (3) successive exits along the [src_trace],
      in reverse; (4) the last exit of the [src_trace] and the [snk_trace]
      entry; (5) successive calls along the [snk_trace]. Each step is
      computed using [reachable_dists], labelled inline by the numbers given
      here. If [snk_trace] is empty, then only (1) is needed. *)
  let compute_distances ~entry ~src_trace ~snk_trace pgm =
    [%dbg]
      ~call:(fun {pf} ->
        let pp_trace =
          IArray.pp "@ -> " (fun ppf -> Function.name >> String.pp ppf)
        in
        pf "wrt trace: @[<v 2>src:@ %a@]@\n@[<v 2>sink:@ %a@]" pp_trace
          src_trace pp_trace snk_trace ;
        assert (not (IArray.is_empty src_trace)) )
      ~retn:(fun {pf} _ -> pf "")
    @@ fun () ->
    let open Containers.Result.Infix in
    let checkpoint_idx = ref 0 in
    let ret_idx call_idx = (2 * IArray.length src_trace) - call_idx in
    let root_to_src_ctx = ref None in
    let* last_entry, preceding_stack =
      IArray.fold src_trace
        (Ok (entry, Abstr_stack.empty ()))
        ~f:(fun next curr ->
          let* curr, _ = curr in
          let next_entry =
            Function.Map.(find_exn next pgm.functions).entry
          in
          let idx = Int.post_incr checkpoint_idx in
          (* (1): successive calls along [src_trace] *)
          let* preceding_stack = record_block_dists ~idx curr next_entry in
          if idx = 1 then
            root_to_src_ctx := Some (next_entry, preceding_stack) ;
          let+ () =
            if idx > 0 && not (IArray.is_empty snk_trace) then
              (* (3): successive returns along [src_trace] *)
              record_fn_dists ~idx:(ret_idx idx) ~preceding_stack next
                curr.parent.name
            else Ok ()
          in
          (next_entry, preceding_stack) )
    in
    match Array.to_list (IArray.to_array snk_trace) with
    | [] -> Ok ()
    | snk_head :: snk_tail ->
        (* (2): from the entry to exit of the innermost [src_trace] fn *)
        let* () =
          record_dists_to_exit ~idx:(IArray.length src_trace)
            ~preceding_stack last_entry
        in
        let src_trace_root, root_ctx = Option.get_exn !root_to_src_ctx in
        checkpoint_idx := ret_idx 0 ;
        (* (4): from exit of [src_trace] to entry of [snk_trace] *)
        let idx = Int.post_incr checkpoint_idx in
        let* () =
          record_fn_dists ~idx ~preceding_stack:root_ctx
            src_trace_root.parent.name snk_head
        in
        List.fold snk_tail
          (Ok Function.Map.(find_exn snk_head pgm.functions).entry)
          ~f:(fun next curr_res ->
            let* curr = curr_res in
            let next_entry =
              Function.Map.(find_exn next pgm.functions).entry
            in
            let idx = Int.post_incr checkpoint_idx in
            (* (5): successive calls along [snk_trace] *)
            let+ _ = record_block_dists ~idx curr next_entry in
            next_entry )
        >|= ignore
end
