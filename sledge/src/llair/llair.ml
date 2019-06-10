(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Translation units *)

type inst =
  | Load of {reg: Var.t; ptr: Exp.t; len: Exp.t; loc: Loc.t}
  | Store of {ptr: Exp.t; exp: Exp.t; len: Exp.t; loc: Loc.t}
  | Memset of {dst: Exp.t; byt: Exp.t; len: Exp.t; loc: Loc.t}
  | Memcpy of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
  | Memmov of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
  | Alloc of {reg: Var.t; num: Exp.t; len: Exp.t; loc: Loc.t}
  | Free of {ptr: Exp.t; loc: Loc.t}
  | Nondet of {reg: Var.t option; msg: string; loc: Loc.t}
  | Abort of {loc: Loc.t}
[@@deriving sexp]

type cmnd = inst vector [@@deriving sexp]
type label = string [@@deriving sexp]

type 'a control_transfer =
  {mutable dst: 'a; args: Exp.t list; mutable retreating: bool}
[@@deriving compare, equal, sexp_of]

type jump = block control_transfer

and term =
  | Switch of {key: Exp.t; tbl: (Exp.t * jump) vector; els: jump; loc: Loc.t}
  | Iswitch of {ptr: Exp.t; tbl: jump vector; loc: Loc.t}
  | Call of
      { call: Exp.t control_transfer
      ; typ: Typ.t
      ; return: jump
      ; throw: jump option
      ; ignore_result: bool
      ; loc: Loc.t }
  | Return of {exp: Exp.t option; loc: Loc.t}
  | Throw of {exc: Exp.t; loc: Loc.t}
  | Unreachable

and block =
  { lbl: label
  ; params: Var.t list
  ; locals: Var.Set.t
  ; cmnd: cmnd
  ; term: term
  ; mutable parent: func
  ; mutable sort_index: int }

and cfg = block vector

(* [entry] is not part of [cfg] since it is special in several ways: its
   params are the func params, its locals are all the locals in it plus the
   cfg, and it cannot be jumped to, only called. *)
and func =
  { name: Global.t
  ; entry: block
  ; cfg: cfg
  ; freturn: Var.t option
  ; fthrow: Var.t }

let sexp_cons (hd : Sexp.t) (tl : Sexp.t) =
  match tl with
  | List xs -> Sexp.List (hd :: xs)
  | Atom _ -> Sexp.List [hd; tl]

let sexp_ctor label args = sexp_cons (Sexp.Atom label) args

let sexp_of_jump {dst; args; retreating} =
  [%sexp {dst: label = dst.lbl; args: Exp.t list; retreating: bool}]

let sexp_of_term = function
  | Switch {key; tbl; els; loc} ->
      sexp_ctor "Switch"
        [%sexp
          {key: Exp.t; tbl: (Exp.t * jump) vector; els: jump; loc: Loc.t}]
  | Iswitch {ptr; tbl; loc} ->
      sexp_ctor "Iswitch" [%sexp {ptr: Exp.t; tbl: jump vector; loc: Loc.t}]
  | Call {call; typ; return; throw; ignore_result; loc} ->
      sexp_ctor "Call"
        [%sexp
          { call: Exp.t control_transfer
          ; typ: Typ.t
          ; return: jump
          ; throw: jump option
          ; ignore_result: bool
          ; loc: Loc.t }]
  | Return {exp; loc} ->
      sexp_ctor "Return" [%sexp {exp: Exp.t option; loc: Loc.t}]
  | Throw {exc; loc} -> sexp_ctor "Throw" [%sexp {exc: Exp.t; loc: Loc.t}]
  | Unreachable -> Sexp.Atom "Unreachable"

let sexp_of_block {lbl; params; locals; cmnd; term; parent; sort_index} =
  [%sexp
    { lbl: label
    ; params: Var.t list
    ; locals: Var.Set.t
    ; cmnd: cmnd
    ; term: term
    ; parent: Var.t = parent.name.var
    ; sort_index: int }]

let sexp_of_cfg v = [%sexp_of: block vector] v

let sexp_of_func {name; entry; cfg} =
  [%sexp {name: Global.t; entry: block; cfg: cfg}]

(* blocks in a [t] are uniquely identified by [sort_index] *)
let compare_block x y = Int.compare x.sort_index y.sort_index
let equal_block x y = Int.equal x.sort_index y.sort_index

type functions = func Var.Map.t [@@deriving sexp_of]

type t = {globals: Global.t vector; functions: functions}
[@@deriving sexp_of]

let pp_inst fs inst =
  let pf pp = Format.fprintf fs pp in
  match inst with
  | Load {reg; ptr; len; loc} ->
      pf "@[<2>load %a@ %a@ %a;@]\t%a" Exp.pp len Var.pp reg Exp.pp ptr
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
      pf "@[<2>alloc %a@ [%a x %a];@]\t%a" Var.pp reg Exp.pp num Exp.pp len
        Loc.pp loc
  | Free {ptr; loc} -> pf "@[<2>free %a;@]\t%a" Exp.pp ptr Loc.pp loc
  | Nondet {reg; msg; loc} ->
      pf "@[<2>nondet %a\"%s\";@]\t%a" (Option.pp "%a " Var.pp) reg msg
        Loc.pp loc
  | Abort {loc} -> pf "@[<2>abort;@]\t%a" Loc.pp loc

let pp_args pp_arg fs args =
  Format.fprintf fs "@ (@[%a@])" (List.pp ",@ " pp_arg) (List.rev args)

let pp_param fs var = Var.pp fs var

let pp_jump fs {dst= {lbl}; args; retreating} =
  Format.fprintf fs "@[<2>%s%%%s%a@]"
    (if retreating then "↑" else "")
    lbl (pp_args Exp.pp) args

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
        (Vector.pp "@ " (fun fs ({dst= {lbl}; _} as jmp) ->
             Format.fprintf fs "%s: %a" lbl pp_goto jmp ))
        tbl Loc.pp loc
  | Call {call= {dst; args; retreating}; return; throw; loc} ->
      pf "@[<2>@[<7>call @[<2>%s%a%a@]@]@ @[returnto %a%a;@]@]\t%a"
        (if retreating then "↑" else "")
        Exp.pp dst (pp_args Exp.pp) args pp_jump return
        (Option.pp "@ throwto %a" pp_jump)
        throw Loc.pp loc
  | Return {exp; loc} ->
      pf "@[<2>return%a@]\t%a" (Option.pp " %a" Exp.pp) exp Loc.pp loc
  | Throw {exc; loc} -> pf "@[<2>throw %a@]\t%a" Exp.pp exc Loc.pp loc
  | Unreachable -> pf "unreachable"

let pp_cmnd = Vector.pp "@ " pp_inst

let pp_block fs {lbl; params; cmnd; term; sort_index} =
  Format.fprintf fs "@[<v 2>@[<4>%s%a@]: #%i@ @[<v>%a%t%a@]@]" lbl
    (pp_args pp_param) params sort_index pp_cmnd cmnd
    (fun fs -> if Vector.is_empty cmnd then () else Format.fprintf fs "@ ")
    pp_term term

(** Initial cyclic values *)

let rec dummy_block =
  { lbl= "dummy"
  ; params= []
  ; locals= Var.Set.empty
  ; cmnd= Vector.empty
  ; term= Unreachable
  ; parent= dummy_func
  ; sort_index= 0 }

and dummy_func =
  let dummy_var = Var.program "dummy" in
  let dummy_ptr_typ = Typ.pointer ~elt:(Typ.opaque ~name:"dummy") in
  { name= Global.mk dummy_var dummy_ptr_typ Loc.none
  ; entry= dummy_block
  ; cfg= Vector.empty
  ; freturn= None
  ; fthrow= dummy_var }

(** Instructions *)

module Inst = struct
  type t = inst [@@deriving sexp]

  let pp = pp_inst
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
    | Load {loc}
     |Store {loc}
     |Memset {loc}
     |Memcpy {loc}
     |Memmov {loc}
     |Alloc {loc}
     |Free {loc}
     |Nondet {loc}
     |Abort {loc} ->
        loc

  let union_locals inst vs =
    match inst with
    | Load {reg} | Alloc {reg} | Nondet {reg= Some reg} -> Set.add vs reg
    | Store _ | Memcpy _ | Memmov _ | Memset _ | Free _
     |Nondet {reg= None}
     |Abort _ ->
        vs

  let locals inst = union_locals inst Var.Set.empty
end

(** Jumps *)

module Jump = struct
  type t = jump [@@deriving sexp_of]

  let compare = compare_control_transfer compare_block
  let equal = equal_control_transfer equal_block
  let pp = pp_jump

  let invariant ?(accept_return = false) jmp =
    Invariant.invariant [%here] (jmp, accept_return) [%sexp_of: t * bool]
    @@ fun () ->
    let {dst= {params; parent}; args} = jmp in
    if parent == dummy_func then
      (* jmp not yet backpatched by Func.mk *)
      assert true
    else
      assert (
        List.length params = List.length args + Bool.to_int accept_return )

  let mk lbl args =
    {dst= {dummy_block with lbl}; args; retreating= false}
    |> check invariant
end

(** Basic-Block Terminators *)

module Term = struct
  type t = term [@@deriving sexp_of]

  let pp = pp_term

  let invariant term =
    Invariant.invariant [%here] term [%sexp_of: t]
    @@ fun () ->
    match term with
    | Switch {tbl; els} ->
        Vector.iter tbl ~f:(fun (_, jmp) -> Jump.invariant jmp) ;
        Jump.invariant els
    | Iswitch {tbl} -> Vector.iter tbl ~f:Jump.invariant
    | Call {call= {args= actls}; typ; return; throw; ignore_result} -> (
      match typ with
      | Pointer {elt= Function {args= frmls; return= retn_typ}} ->
          assert (Vector.length frmls = List.length actls) ;
          Jump.invariant return
            ~accept_return:(Option.is_some retn_typ && not ignore_result) ;
          Option.iter throw ~f:(Jump.invariant ~accept_return:true)
      | _ -> assert false )
    | Return _ | Throw _ | Unreachable -> assert true

  let goto ~dst ~loc =
    Switch {key= Exp.bool false; tbl= Vector.empty; els= dst; loc}
    |> check invariant

  let branch ~key ~nzero ~zero ~loc =
    let tbl = Vector.of_array [|(Exp.bool false, zero)|] in
    let els = nzero in
    Switch {key; tbl; els; loc} |> check invariant

  let switch ~key ~tbl ~els ~loc =
    Switch {key; tbl; els; loc} |> check invariant

  let iswitch ~ptr ~tbl ~loc = Iswitch {ptr; tbl; loc} |> check invariant

  let call ~func ~typ ~args ~return ~throw ~ignore_result ~loc =
    Call
      { call= {dst= func; args; retreating= false}
      ; typ
      ; return
      ; throw
      ; ignore_result
      ; loc }
    |> check invariant

  let return ~exp ~loc = Return {exp; loc} |> check invariant
  let throw ~exc ~loc = Throw {exc; loc} |> check invariant
  let unreachable = Unreachable |> check invariant

  let loc = function
    | Switch {loc} | Iswitch {loc} | Call {loc} | Return {loc} | Throw {loc}
      ->
        loc
    | Unreachable -> Loc.none
end

(** Basic-Blocks *)

module Block = struct
  module T = struct type t = block [@@deriving compare, equal, sexp_of] end
  include T
  include Comparator.Make (T)

  let pp = pp_block

  let invariant blk =
    Invariant.invariant [%here] blk [%sexp_of: t]
    @@ fun () ->
    assert (not (List.contains_dup blk.params ~compare:Var.compare))

  let mk ~lbl ~params ~cmnd ~term =
    let locals =
      let locals_cmnd cmnd vs =
        Vector.fold_right cmnd ~init:vs ~f:Inst.union_locals
      in
      let locals_params params vs = List.fold params ~init:vs ~f:Set.add in
      locals_params params (locals_cmnd cmnd Var.Set.empty)
    in
    { lbl
    ; params
    ; locals
    ; cmnd
    ; term
    ; parent= dummy_block.parent
    ; sort_index= dummy_block.sort_index }
    |> check invariant
end

(** Functions *)

module Func = struct
  type t = func [@@deriving sexp_of]

  let is_undefined = function
    | {entry= {cmnd; term= Unreachable}} -> Vector.is_empty cmnd
    | _ -> false

  let pp fs ({name; entry= {params; cmnd; term; sort_index}; cfg} as func) =
    let pp_if cnd str fs = if cnd then Format.fprintf fs str in
    Format.fprintf fs "@[<v>@[<v>%a@[<2>%a%a@]%t@]" (Option.pp "%a " Typ.pp)
      ( match name.typ with
      | Pointer {elt= Function {return}} -> return
      | _ -> None )
      Global.pp name (pp_args pp_param) params
      (fun fs ->
        if is_undefined func then Format.fprintf fs " #%i@]" sort_index
        else
          Format.fprintf fs " { #%i %a@;<1 4>@[<v>%a@ %a@]%t%a@]@ }"
            sort_index Loc.pp name.loc pp_cmnd cmnd Term.pp term
            (pp_if (not (Vector.is_empty cfg)) "@ @   ")
            (Vector.pp "@\n@\n  " Block.pp)
            cfg )

  let fold_term {entry= {term}; cfg} ~init ~f =
    Vector.fold cfg ~init:(f init term) ~f:(fun a {term} -> f a term)

  let iter_term {entry= {term}; cfg} ~f =
    f term ;
    Vector.iter cfg ~f:(fun {term} -> f term)

  let invariant func =
    Invariant.invariant [%here] func [%sexp_of: t]
    @@ fun () ->
    assert (func == func.entry.parent) ;
    let {name= {typ}; cfg} = func in
    match typ with
    | Pointer _ ->
        assert (
          not
            (Vector.contains_dup cfg ~compare:(fun b1 b2 ->
                 String.compare b1.lbl b2.lbl )) ) ;
        assert (
          not
            (List.contains_dup
               (List.concat_map (Vector.to_list cfg) ~f:(fun {params} ->
                    params ))
               ~compare:Var.compare) ) ;
        iter_term func ~f:(fun term -> Term.invariant term)
    | _ -> assert false

  let find functions name = Map.find functions name

  let mk ~(name : Global.t) ~entry ~cfg =
    let locals =
      Vector.fold ~init:entry.locals cfg ~f:(fun locals block ->
          Set.union locals block.locals )
    in
    let freturn, locals =
      match name.typ with
      | Pointer {elt= Function {return= Some _}} ->
          let freturn, locals =
            Var.fresh "freturn" ~wrt:(Set.add_list entry.params locals)
          in
          (Some freturn, locals)
      | _ -> (None, locals)
    in
    let fthrow, locals =
      Var.fresh "fthrow" ~wrt:(Set.add_list entry.params locals)
    in
    let entry = {entry with locals} in
    let func = {name; entry; cfg; freturn; fthrow} in
    let resolve_parent_and_jumps block =
      block.parent <- func ;
      let lookup cfg lbl : block =
        Vector.find_exn cfg ~f:(fun k -> String.equal lbl k.lbl)
      in
      let set_dst jmp = jmp.dst <- lookup cfg jmp.dst.lbl in
      match block.term with
      | Switch {tbl; els} ->
          Vector.iter tbl ~f:(fun (_, jmp) -> set_dst jmp) ;
          set_dst els
      | Iswitch {tbl} -> Vector.iter tbl ~f:set_dst
      | Call {return; throw} ->
          set_dst return ;
          Option.iter throw ~f:set_dst
      | Return _ | Throw _ | Unreachable -> ()
    in
    resolve_parent_and_jumps entry ;
    Vector.iter cfg ~f:resolve_parent_and_jumps ;
    func |> check invariant

  let mk_undefined ~name ~params =
    let entry =
      Block.mk ~lbl:"" ~params ~cmnd:Vector.empty ~term:Term.unreachable
    in
    let cfg = Vector.empty in
    mk ~name ~entry ~cfg
end

(** Derived meta-data *)

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
module FuncQ = Hash_queue.Make (Var)

let set_derived_metadata functions =
  let compute_roots functions =
    let roots = FuncQ.create () in
    Map.iter functions ~f:(fun func ->
        FuncQ.enqueue_back_exn roots func.name.var func ) ;
    Map.iter functions ~f:(fun func ->
        Func.fold_term func ~init:() ~f:(fun () -> function
          | Call {call= {dst}} -> (
            match Var.of_exp dst with
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
        | Switch {tbl; els} ->
            Vector.iter tbl ~f:(fun (_, jmp) -> jump jmp) ;
            jump els
        | Iswitch {tbl} -> Vector.iter tbl ~f:jump
        | Call {call= {dst} as call; return; throw} ->
            ( match Var.of_exp dst >>= Func.find functions with
            | Some func ->
                if Set.mem ancestors func.entry then call.retreating <- true
                else visit ancestors func func.entry
            | None ->
                (* conservatively assume all virtual calls are recursive *)
                call.retreating <- true ) ;
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
  let sort_cfgs functions =
    Map.iter functions ~f:(fun {cfg} ->
        Array.sort
          ~compare:(fun x y -> Int.compare x.sort_index y.sort_index)
          (Vector.to_array cfg) )
  in
  let functions =
    List.fold functions ~init:Var.Map.empty ~f:(fun m func ->
        Map.add_exn m ~key:func.name.var ~data:func )
  in
  let roots = compute_roots functions in
  let tips_to_roots = topsort functions roots in
  set_sort_indices tips_to_roots ;
  sort_cfgs functions ;
  functions

let invariant pgm =
  Invariant.invariant [%here] pgm [%sexp_of: t]
  @@ fun () ->
  assert (
    not
      (Vector.contains_dup pgm.globals ~compare:(fun g1 g2 ->
           Var.compare g1.Global.var g2.Global.var )) )

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
