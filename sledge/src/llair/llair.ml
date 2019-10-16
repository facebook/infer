(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** LLAIR (Low-Level Analysis Internal Representation) *)

[@@@warning "+9"]

type inst =
  | Move of {reg_exps: (Reg.t * Exp.t) vector; loc: Loc.t}
  | Load of {reg: Reg.t; ptr: Exp.t; len: Exp.t; loc: Loc.t}
  | Store of {ptr: Exp.t; exp: Exp.t; len: Exp.t; loc: Loc.t}
  | Memset of {dst: Exp.t; byt: Exp.t; len: Exp.t; loc: Loc.t}
  | Memcpy of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
  | Memmov of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
  | Alloc of {reg: Reg.t; num: Exp.t; len: Exp.t; loc: Loc.t}
  | Free of {ptr: Exp.t; loc: Loc.t}
  | Nondet of {reg: Reg.t option; msg: string; loc: Loc.t}
  | Abort of {loc: Loc.t}
[@@deriving sexp]

type cmnd = inst vector [@@deriving sexp]
type label = string [@@deriving sexp]

type jump = {mutable dst: block; mutable retreating: bool}

and 'a call =
  { callee: 'a
  ; typ: Typ.t
  ; actuals: Exp.t list
  ; areturn: Reg.t option
  ; return: jump
  ; throw: jump option
  ; mutable recursive: bool
  ; loc: Loc.t }

and term =
  | Switch of {key: Exp.t; tbl: (Exp.t * jump) vector; els: jump; loc: Loc.t}
  | Iswitch of {ptr: Exp.t; tbl: jump vector; loc: Loc.t}
  | Call of Exp.t call
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
  { name: Global.t
  ; formals: Reg.t list
  ; freturn: Reg.t option
  ; fthrow: Reg.t
  ; locals: Reg.Set.t
  ; entry: block }

let sexp_cons (hd : Sexp.t) (tl : Sexp.t) =
  match tl with
  | List xs -> Sexp.List (hd :: xs)
  | Atom _ -> Sexp.List [hd; tl]

let sexp_ctor label args = sexp_cons (Sexp.Atom label) args

let sexp_of_jump {dst; retreating} =
  [%sexp {dst: label = dst.lbl; retreating: bool}]

let sexp_of_term = function
  | Switch {key; tbl; els; loc} ->
      sexp_ctor "Switch"
        [%sexp
          {key: Exp.t; tbl: (Exp.t * jump) vector; els: jump; loc: Loc.t}]
  | Iswitch {ptr; tbl; loc} ->
      sexp_ctor "Iswitch" [%sexp {ptr: Exp.t; tbl: jump vector; loc: Loc.t}]
  | Call {callee; typ; actuals; areturn; return; throw; recursive; loc} ->
      sexp_ctor "Call"
        [%sexp
          { callee: Exp.t
          ; typ: Typ.t
          ; actuals: Exp.t list
          ; areturn: Reg.t option
          ; return: jump
          ; throw: jump option
          ; recursive: bool
          ; loc: Loc.t }]
  | Return {exp; loc} ->
      sexp_ctor "Return" [%sexp {exp: Exp.t option; loc: Loc.t}]
  | Throw {exc; loc} -> sexp_ctor "Throw" [%sexp {exc: Exp.t; loc: Loc.t}]
  | Unreachable -> Sexp.Atom "Unreachable"

let sexp_of_block {lbl; cmnd; term; parent; sort_index} =
  [%sexp
    { lbl: label
    ; cmnd: cmnd
    ; term: term
    ; parent: Reg.t = parent.name.reg
    ; sort_index: int }]

let sexp_of_func {name; formals; freturn; fthrow; locals; entry} =
  [%sexp
    { name: Global.t
    ; formals: Reg.t list
    ; freturn: Reg.t option
    ; fthrow: Reg.t
    ; locals: Reg.Set.t
    ; entry: block }]

(* blocks in a [t] are uniquely identified by [sort_index] *)
let compare_block x y = Int.compare x.sort_index y.sort_index
let equal_block x y = Int.equal x.sort_index y.sort_index

type functions = func Map.M(String).t [@@deriving sexp_of]

type t = {globals: Global.t vector; functions: functions}
[@@deriving sexp_of]

let pp_inst fs inst =
  let pf pp = Format.fprintf fs pp in
  match inst with
  | Move {reg_exps; loc} ->
      let regs, exps = Vector.unzip reg_exps in
      pf "@[<2>@[%a@]@ := @[%a@];@]\t%a" (Vector.pp ",@ " Reg.pp) regs
        (Vector.pp ",@ " Exp.pp) exps Loc.pp loc
  | Load {reg; ptr; len; loc} ->
      pf "@[<2>%a@ := load %a@ %a;@]\t%a" Reg.pp reg Exp.pp len Exp.pp ptr
        Loc.pp loc
  | Store {ptr; exp; len; loc} ->
      pf "@[<2>store %a@ %a@ %a;@]\t%a" Exp.pp len Exp.pp ptr Exp.pp exp
        Loc.pp loc
  | Memset {dst; byt; len; loc} ->
      pf "@[<2>memset %a %a %a;@]\t%a" Exp.pp len Exp.pp dst Exp.pp byt
        Loc.pp loc
  | Memcpy {dst; src; len; loc} ->
      pf "@[<2>memcpy %a %a %a;@]\t%a" Exp.pp len Exp.pp dst Exp.pp src
        Loc.pp loc
  | Memmov {dst; src; len; loc} ->
      pf "@[<2>memmov %a %a %a;@]\t%a" Exp.pp len Exp.pp dst Exp.pp src
        Loc.pp loc
  | Alloc {reg; num; len; loc} ->
      pf "@[<2>%a@ := alloc [%a x %a];@]\t%a" Reg.pp reg Exp.pp num Exp.pp
        len Loc.pp loc
  | Free {ptr; loc} -> pf "@[<2>free %a;@]\t%a" Exp.pp ptr Loc.pp loc
  | Nondet {reg; msg; loc} ->
      pf "@[<2>%anondet \"%s\";@]\t%a"
        (Option.pp "%a := " Reg.pp)
        reg msg Loc.pp loc
  | Abort {loc} -> pf "@[<2>abort;@]\t%a" Loc.pp loc

let pp_actuals pp_actual fs actuals =
  Format.fprintf fs "@ (@[%a@])" (List.pp ",@ " pp_actual)
    (List.rev actuals)

let pp_formal fs reg = Reg.pp fs reg

let pp_jump fs {dst; retreating} =
  Format.fprintf fs "@[<2>%s%%%s@]"
    (if retreating then "↑" else "")
    dst.lbl

let pp_term fs term =
  let pf pp = Format.fprintf fs pp in
  let pp_goto fs jmp = Format.fprintf fs "goto %a;" pp_jump jmp in
  match term with
  | Switch {key; tbl; els; loc} -> (
    match Vector.to_array tbl with
    | [||] -> pf "@[%a@]\t%a" pp_goto els Loc.pp loc
    | [|(z, jmp)|] when Exp.is_false z ->
        pf "@[if %a@;<1 2>%a@ @[else@;<1 2>%a@]@]\t%a" Exp.pp key pp_goto
          els pp_goto jmp Loc.pp loc
    | _ ->
        pf "@[<2>switch %a@ @[%a@ else: %a@]@]\t%a" Exp.pp key
          (Vector.pp "@ " (fun fs (case, jmp) ->
               Format.fprintf fs "%a: %a" Exp.pp case pp_goto jmp ))
          tbl pp_goto els Loc.pp loc )
  | Iswitch {ptr; tbl; loc} ->
      pf "@[<2>iswitch %a@ @[<hv>%a@]@]\t%a" Exp.pp ptr
        (Vector.pp "@ " (fun fs jmp ->
             Format.fprintf fs "%s: %a" jmp.dst.lbl pp_goto jmp ))
        tbl Loc.pp loc
  | Call {callee; actuals; areturn; return; throw; recursive; loc; _} ->
      pf "@[<2>@[<7>%acall @[<2>%s%a%a@]@]@ @[returnto %a%a;@]@]\t%a"
        (Option.pp "%a := " Reg.pp)
        areturn
        (if recursive then "↑" else "")
        Exp.pp callee (pp_actuals Exp.pp) actuals pp_jump return
        (Option.pp "@ throwto %a" pp_jump)
        throw Loc.pp loc
  | Return {exp; loc} ->
      pf "@[<2>return%a@]\t%a" (Option.pp " %a" Exp.pp) exp Loc.pp loc
  | Throw {exc; loc} -> pf "@[<2>throw %a@]\t%a" Exp.pp exc Loc.pp loc
  | Unreachable -> pf "unreachable"

let pp_cmnd = Vector.pp "@ " pp_inst

let pp_block fs {lbl; cmnd; term; parent= _; sort_index} =
  Format.fprintf fs "@[<v 2>%s: #%i@ @[<v>%a%t%a@]@]" lbl sort_index pp_cmnd
    cmnd
    (fun fs -> if Vector.is_empty cmnd then () else Format.fprintf fs "@ ")
    pp_term term

(** Initial cyclic values *)

let rec dummy_block =
  { lbl= "dummy"
  ; cmnd= Vector.empty
  ; term= Unreachable
  ; parent= dummy_func
  ; sort_index= 0 }

and dummy_func =
  let dummy_reg = Reg.program ~global:() Typ.ptr "dummy" in
  { name= Global.mk dummy_reg Typ.ptr Loc.none
  ; formals= []
  ; freturn= None
  ; fthrow= dummy_reg
  ; locals= Reg.Set.empty
  ; entry= dummy_block }

(** Instructions *)

module Inst = struct
  type t = inst [@@deriving sexp]

  let pp = pp_inst
  let move ~reg_exps ~loc = Move {reg_exps; loc}
  let load ~reg ~ptr ~len ~loc = Load {reg; ptr; len; loc}
  let store ~ptr ~exp ~len ~loc = Store {ptr; exp; len; loc}
  let memset ~dst ~byt ~len ~loc = Memset {dst; byt; len; loc}
  let memcpy ~dst ~src ~len ~loc = Memcpy {dst; src; len; loc}
  let memmov ~dst ~src ~len ~loc = Memmov {dst; src; len; loc}
  let alloc ~reg ~num ~len ~loc = Alloc {reg; num; len; loc}
  let free ~ptr ~loc = Free {ptr; loc}
  let nondet ~reg ~msg ~loc = Nondet {reg; msg; loc}
  let abort ~loc = Abort {loc}

  let loc = function
    | Move {loc; _}
     |Load {loc; _}
     |Store {loc; _}
     |Memset {loc; _}
     |Memcpy {loc; _}
     |Memmov {loc; _}
     |Alloc {loc; _}
     |Free {loc; _}
     |Nondet {loc; _}
     |Abort {loc; _} ->
        loc

  let union_locals inst vs =
    match inst with
    | Move {reg_exps; _} ->
        Vector.fold ~f:(fun vs (reg, _) -> Set.add vs reg) ~init:vs reg_exps
    | Load {reg; _} | Alloc {reg; _} | Nondet {reg= Some reg; _} ->
        Set.add vs reg
    | Store _ | Memcpy _ | Memmov _ | Memset _ | Free _
     |Nondet {reg= None; _}
     |Abort _ ->
        vs

  let locals inst = union_locals inst Reg.Set.empty

  let fold_exps inst ~init ~f =
    match inst with
    | Move {reg_exps; loc= _} ->
        Vector.fold reg_exps ~init ~f:(fun acc (_reg, exp) -> f acc exp)
    | Load {reg= _; ptr; len; loc= _} -> f (f init ptr) len
    | Store {ptr; exp; len; loc= _} -> f (f (f init ptr) exp) len
    | Memset {dst; byt; len; loc= _} -> f (f (f init dst) byt) len
    | Memcpy {dst; src; len; loc= _} | Memmov {dst; src; len; loc= _} ->
        f (f (f init dst) src) len
    | Alloc {reg= _; num; len; loc= _} -> f (f init num) len
    | Free {ptr; loc= _} -> f init ptr
    | Nondet {reg= _; msg= _; loc= _} -> init
    | Abort {loc= _} -> init
end

(** Jumps *)

module Jump = struct
  type t = jump [@@deriving sexp_of]

  let compare x y = compare_block x.dst y.dst
  let equal x y = equal_block x.dst y.dst
  let pp = pp_jump
  let mk lbl = {dst= {dummy_block with lbl}; retreating= false}
end

(** Basic-Block Terminators *)

module Term = struct
  type t = term [@@deriving sexp_of]

  let pp = pp_term

  let invariant ?parent term =
    Invariant.invariant [%here] term [%sexp_of: t]
    @@ fun () ->
    match term with
    | Switch _ | Iswitch _ -> assert true
    | Call {typ; actuals; areturn; _} -> (
      match typ with
      | Pointer {elt= Function {args; return= retn_typ; _}} ->
          assert (Vector.length args = List.length actuals) ;
          assert (Option.is_some retn_typ || Option.is_none areturn)
      | _ -> assert false )
    | Return {exp; _} -> (
      match parent with
      | Some parent ->
          assert (Bool.(Option.is_some exp = Option.is_some parent.freturn))
      | None -> assert true )
    | Throw _ | Unreachable -> assert true

  let goto ~dst ~loc =
    Switch {key= Exp.false_; tbl= Vector.empty; els= dst; loc}
    |> check invariant

  let branch ~key ~nzero ~zero ~loc =
    let tbl = Vector.of_array [|(Exp.false_, zero)|] in
    let els = nzero in
    Switch {key; tbl; els; loc} |> check invariant

  let switch ~key ~tbl ~els ~loc =
    Switch {key; tbl; els; loc} |> check invariant

  let iswitch ~ptr ~tbl ~loc = Iswitch {ptr; tbl; loc} |> check invariant

  let call ~callee ~typ ~actuals ~areturn ~return ~throw ~loc =
    Call
      {callee; typ; actuals; areturn; return; throw; recursive= false; loc}
    |> check invariant

  let return ~exp ~loc = Return {exp; loc} |> check invariant
  let throw ~exc ~loc = Throw {exc; loc} |> check invariant
  let unreachable = Unreachable |> check invariant

  let loc = function
    | Switch {loc; _}
     |Iswitch {loc; _}
     |Call {loc; _}
     |Return {loc; _}
     |Throw {loc; _} ->
        loc
    | Unreachable -> Loc.none

  let union_locals term vs =
    match term with
    | Call {areturn; _} -> Set.add_option areturn vs
    | _ -> vs
end

(** Basic-Blocks *)

module Block = struct
  module T = struct type t = block [@@deriving compare, equal, sexp_of] end
  include T
  include Comparator.Make (T)

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
    module T0 = struct
      type t = block [@@deriving sexp_of]

      let compare x y =
        [%compare: string * Global.t] (x.lbl, x.parent.name)
          (y.lbl, y.parent.name)

      let hash b = [%hash: string * Global.t] (b.lbl, b.parent.name)
    end

    include T0
    include Comparator.Make (T0)
  end

  include T

  let empty_set = Set.empty (module T)
end

module BlockQ = Hash_queue.Make (Block_label)
module FuncQ = Hash_queue.Make (Reg)

(** Functions *)

module Func = struct
  type t = func [@@deriving sexp_of]

  let is_undefined = function
    | {entry= {cmnd; term= Unreachable; _}; _} -> Vector.is_empty cmnd
    | _ -> false

  let fold_cfg ~init ~f func =
    let seen = Hash_set.create (module Block_label) in
    let rec fold_cfg_ s blk =
      if Result.is_error (Hash_set.strict_add seen blk) then s
      else
        let s =
          let f s j = fold_cfg_ s j.dst in
          match blk.term with
          | Switch {tbl; els; _} ->
              let s = Vector.fold ~f:(fun s (_, j) -> f s j) ~init:s tbl in
              f s els
          | Iswitch {tbl; _} -> Vector.fold ~f ~init:s tbl
          | Call {return; throw; _} ->
              let s = f s return in
              Option.fold ~f ~init:s throw
          | Return _ | Throw _ | Unreachable -> s
        in
        f s blk
    in
    fold_cfg_ init func.entry

  let fold_term func ~init ~f =
    fold_cfg func ~init ~f:(fun s blk -> f s blk.term)

  let iter_term func ~f =
    fold_cfg func ~init:() ~f:(fun () blk -> f blk.term)

  let entry_cfg func = fold_cfg ~init:[] ~f:(fun cfg blk -> blk :: cfg) func

  let pp fs func =
    let {name; formals; freturn; entry; _} = func in
    let {cmnd; term; sort_index; _} = entry in
    let pp_if cnd str fs = if cnd then Format.fprintf fs str in
    Format.fprintf fs "@[<v>@[<v>%a%a@[<2>%a%a@]%t@]"
      (Option.pp "%a " Typ.pp)
      ( match name.typ with
      | Pointer {elt= Function {return; _}} -> return
      | _ -> None )
      (Option.pp " %a := " Reg.pp)
      freturn Global.pp name (pp_actuals pp_formal) formals
      (fun fs ->
        if is_undefined func then Format.fprintf fs " #%i@]" sort_index
        else
          let cfg =
            List.sort ~compare:Block.compare (List.tl_exn (entry_cfg func))
          in
          Format.fprintf fs " { #%i %a@;<1 4>@[<v>%a@ %a@]%t%a@]@ }"
            sort_index Loc.pp name.loc pp_cmnd cmnd Term.pp term
            (pp_if (not (List.is_empty cfg)) "@ @   ")
            (List.pp "@\n@\n  " Block.pp)
            cfg )

  let invariant func =
    Invariant.invariant [%here] func [%sexp_of: t]
    @@ fun () ->
    assert (func == func.entry.parent) ;
    match func.name.typ with
    | Pointer {elt= Function {return; _}; _} ->
        assert (
          not
            (List.contains_dup (entry_cfg func) ~compare:(fun b1 b2 ->
                 String.compare b1.lbl b2.lbl )) ) ;
        assert (Bool.(Option.is_some return = Option.is_some func.freturn)) ;
        iter_term func ~f:(fun term -> Term.invariant ~parent:func term)
    | _ -> assert false

  let find functions name = Map.find functions name

  let mk ~(name : Global.t) ~formals ~freturn ~fthrow ~entry ~cfg =
    let locals =
      let locals_cmnd locals cmnd =
        Vector.fold_right ~f:Inst.union_locals cmnd ~init:locals
      in
      let locals_block locals block =
        locals_cmnd (Term.union_locals block.term locals) block.cmnd
      in
      let init = locals_block Reg.Set.empty entry in
      Vector.fold ~f:locals_block cfg ~init
    in
    let func = {name; formals; freturn; fthrow; locals; entry} in
    let resolve_parent_and_jumps block =
      block.parent <- func ;
      let lookup cfg lbl : block =
        Vector.find_exn cfg ~f:(fun k -> String.equal lbl k.lbl)
      in
      let set_dst jmp = jmp.dst <- lookup cfg jmp.dst.lbl in
      match block.term with
      | Switch {tbl; els; _} ->
          Vector.iter tbl ~f:(fun (_, jmp) -> set_dst jmp) ;
          set_dst els
      | Iswitch {tbl; _} -> Vector.iter tbl ~f:set_dst
      | Call {return; throw; _} ->
          set_dst return ;
          Option.iter throw ~f:set_dst
      | Return _ | Throw _ | Unreachable -> ()
    in
    resolve_parent_and_jumps entry ;
    Vector.iter cfg ~f:resolve_parent_and_jumps ;
    func |> check invariant

  let mk_undefined ~name ~formals ~freturn ~fthrow =
    let entry =
      Block.mk ~lbl:"" ~cmnd:Vector.empty ~term:Term.unreachable
    in
    let cfg = Vector.empty in
    mk ~name ~entry ~formals ~freturn ~fthrow ~cfg
end

(** Derived meta-data *)

let set_derived_metadata functions =
  let compute_roots functions =
    let roots = FuncQ.create () in
    Map.iter functions ~f:(fun func ->
        FuncQ.enqueue_back_exn roots func.name.reg func ) ;
    Map.iter functions ~f:(fun func ->
        Func.fold_term func ~init:() ~f:(fun () -> function
          | Call {callee; _} -> (
            match Reg.of_exp callee with
            | Some callee ->
                FuncQ.remove roots callee |> (ignore : [> ] -> unit)
            | None -> () )
          | _ -> () ) ) ;
    roots
  in
  let topsort functions roots =
    let tips_to_roots = BlockQ.create () in
    let rec visit ancestors func src =
      if BlockQ.mem tips_to_roots src then ()
      else
        let ancestors = Set.add ancestors src in
        let jump jmp =
          if Set.mem ancestors jmp.dst then jmp.retreating <- true
          else visit ancestors func jmp.dst
        in
        ( match src.term with
        | Switch {tbl; els; _} ->
            Vector.iter tbl ~f:(fun (_, jmp) -> jump jmp) ;
            jump els
        | Iswitch {tbl; _} -> Vector.iter tbl ~f:jump
        | Call ({callee; return; throw; _} as call) ->
            ( match
                Reg.of_exp callee >>| Reg.name >>= Func.find functions
              with
            | Some func ->
                if Set.mem ancestors func.entry then call.recursive <- true
                else visit ancestors func func.entry
            | None ->
                (* conservatively assume all virtual calls are recursive *)
                call.recursive <- true ) ;
            jump return ; Option.iter ~f:jump throw
        | Return _ | Throw _ | Unreachable -> () ) ;
        BlockQ.enqueue_back_exn tips_to_roots src ()
    in
    FuncQ.iter roots ~f:(fun root ->
        visit Block_label.empty_set root root.entry ) ;
    tips_to_roots
  in
  let set_sort_indices tips_to_roots =
    let index = ref (BlockQ.length tips_to_roots) in
    BlockQ.iteri tips_to_roots ~f:(fun ~key:block ~data:_ ->
        block.sort_index <- !index ;
        index := !index - 1 )
  in
  let functions =
    List.fold functions
      ~init:(Map.empty (module String))
      ~f:(fun m func ->
        Map.add_exn m ~key:(Reg.name func.name.reg) ~data:func )
  in
  let roots = compute_roots functions in
  let tips_to_roots = topsort functions roots in
  set_sort_indices tips_to_roots ;
  functions

let invariant pgm =
  Invariant.invariant [%here] pgm [%sexp_of: t]
  @@ fun () ->
  assert (
    not
      (Vector.contains_dup pgm.globals ~compare:(fun g1 g2 ->
           Reg.compare g1.Global.reg g2.Global.reg )) )

let mk ~globals ~functions =
  { globals= Vector.of_list_rev globals
  ; functions= set_derived_metadata functions }
  |> check invariant

let pp fs {globals; functions} =
  Format.fprintf fs "@[<v>@[%a@]@ @ @ @[%a@]@]"
    (Vector.pp "@\n@\n" Global.pp_defn)
    globals
    (List.pp "@\n@\n" Func.pp)
    ( Map.data functions
    |> List.sort ~compare:(fun x y -> compare_block x.entry y.entry) )
