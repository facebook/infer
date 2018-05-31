(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Programs / translation units *)

type inst =
  | Load of {reg: Var.t; ptr: Exp.t; loc: Loc.t}
  | Store of {ptr: Exp.t; exp: Exp.t; loc: Loc.t}
  | Memcpy of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
  | Memmov of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
  | Memset of {dst: Exp.t; byt: Exp.t; len: Exp.t; loc: Loc.t}
  | Alloc of {reg: Var.t; num: Exp.t; loc: Loc.t}
  | Free of {ptr: Exp.t; loc: Loc.t}
  | Nondet of {reg: Var.t option; msg: string; loc: Loc.t}
[@@deriving sexp]

type cmnd = inst vector [@@deriving sexp]

type label = string [@@deriving sexp]

type 'a control_transfer =
  {mutable dst: 'a; args: Exp.t vector; mutable retreating: bool}
[@@deriving sexp]

type jump = block control_transfer

and term =
  | Switch of {key: Exp.t; tbl: (Z.t * jump) vector; els: jump; loc: Loc.t}
  | ISwitch of {ptr: Exp.t; tbl: jump vector; loc: Loc.t}
  | Call of
      { call: Exp.t control_transfer
      ; return: jump
      ; throw: jump option
      ; ignore_result: bool
      ; loc: Loc.t }
  | Return of {exp: Exp.t option; loc: Loc.t}
  | Throw of {exc: Exp.t; loc: Loc.t}
  | Unreachable

and block =
  { lbl: label
  ; params: Var.t vector
  ; cmnd: cmnd
  ; term: term
  ; mutable parent: func
  ; mutable sort_index: int }

and cfg = block vector

and func = {name: Global.t; entry: block; cfg: cfg} [@@deriving sexp]

type t =
  {typ_defns: Typ.t list; globals: Global.t vector; functions: func vector}

(** Initial cyclic values *)

let rec dummy_block =
  { lbl= "dummy"
  ; params= Vector.empty
  ; cmnd= Vector.empty
  ; term= Unreachable
  ; parent= dummy_func
  ; sort_index= 0 }

and dummy_func =
  {name= Global.mk "dummy" Typ.i8p; entry= dummy_block; cfg= Vector.empty}

module Inst = struct
  type t = inst

  let mkLoad ~reg ~ptr ~loc =
    assert (
      match Exp.typ ptr with
      | Pointer {elt} -> Typ.equal elt (Var.typ reg) && Typ.is_sized elt
      | _ -> false ) ;
    Load {reg; ptr; loc}

  let mkStore ~ptr ~exp ~loc =
    assert (
      match Exp.typ ptr with
      | Pointer {elt} -> Typ.equal elt (Exp.typ exp) && Typ.is_sized elt
      | _ -> false ) ;
    Store {ptr; exp; loc}

  let mkMemcpy ~dst ~src ~len ~loc =
    assert (
      match (Exp.typ dst, Exp.typ src, Exp.typ len) with
      | (Pointer {elt} as ptr1), ptr2, Integer _ ->
          Typ.equal ptr1 ptr2 && Typ.is_sized elt
      | _ -> false ) ;
    Memcpy {dst; src; len; loc}

  let mkMemmov ~dst ~src ~len ~loc =
    assert (
      match (Exp.typ dst, Exp.typ src, Exp.typ len) with
      | (Pointer {elt} as ptr1), ptr2, Integer _ ->
          Typ.equal ptr1 ptr2 && Typ.is_sized elt
      | _ -> false ) ;
    Memmov {dst; src; len; loc}

  let mkMemset ~dst ~byt ~len ~loc =
    assert (
      match (Exp.typ dst, Exp.typ byt, Exp.typ len) with
      | Pointer {elt}, Integer {bits= 8}, Integer _ -> Typ.is_sized elt
      | _ -> false ) ;
    Memset {dst; byt; len; loc}

  let mkAlloc ~reg ~num ~loc =
    assert (
      match (Var.typ reg, Exp.typ num) with
      | Pointer {elt}, Integer _ -> Typ.is_sized elt
      | _ -> false ) ;
    Alloc {reg; num; loc}

  let mkFree ~ptr ~loc =
    assert (
      match Exp.typ ptr with
      | Pointer {elt} -> Typ.is_sized elt
      | _ -> false ) ;
    Free {ptr; loc}

  let mkNondet ~reg ~msg ~loc =
    assert (Option.for_all ~f:(Var.typ >> Typ.is_sized) reg) ;
    Nondet {reg; msg; loc}

  let fmt ff inst =
    let pf fmt = Format.fprintf ff fmt in
    match inst with
    | Load {reg; ptr} -> pf "load %a %a;" Var.fmt reg Exp.fmt ptr
    | Store {ptr; exp} -> pf "store %a %a;" Exp.fmt ptr Exp.fmt exp
    | Memcpy {dst; src; len} ->
        pf "memcpy %a %a %a;" Exp.fmt dst Exp.fmt src Exp.fmt len
    | Memmov {dst; src; len} ->
        pf "memmov %a %a %a;" Exp.fmt dst Exp.fmt src Exp.fmt len
    | Memset {dst; byt; len} ->
        pf "memset %a %a %a;" Exp.fmt dst Exp.fmt byt Exp.fmt len
    | Alloc {reg; num} ->
        let[@warning "p"] Typ.Pointer {elt} = Var.typ reg in
        pf "alloc %a [%a x %a];" Var.fmt reg Exp.fmt num Typ.fmt elt
    | Free {ptr} -> pf "free %a;" Exp.fmt ptr
    | Nondet {reg; msg} ->
        pf "nondet %a\"%s\";" (option_fmt "%a " Var.fmt) reg msg
end

let fmt_cmnd = vector_fmt "@ " Inst.fmt

let fmt_args fmt_arg ff args =
  Format.fprintf ff "@ (@[%a@])" (vector_fmt ",@ " fmt_arg) args

let fmt_param ff var =
  Format.fprintf ff "%a %a" Typ.fmt (Var.typ var) Var.fmt var

module Jump = struct
  type t = jump

  let mk lbl args = {dst= {dummy_block with lbl}; args; retreating= false}

  let fmt ff {dst= {lbl}; args; retreating} =
    Format.fprintf ff "@[<2>%s%%%s%a@]"
      (if retreating then "↑" else "")
      lbl (fmt_args Exp.fmt) args
end

module Term = struct
  type t = term

  let mkSwitch ~key ~tbl ~els ~loc =
    assert (match Exp.typ key with Integer _ -> true | _ -> false) ;
    Switch {key; tbl; els; loc}

  let mkISwitch ~ptr ~tbl ~loc =
    assert (
      match Exp.typ ptr with
      | Pointer {elt= Integer {bits= 8}} -> true
      | _ -> false ) ;
    ISwitch {ptr; tbl; loc}

  let mkCall ~func ~args ~return ~throw ~ignore_result ~loc =
    assert (
      match Exp.typ func with
      | Pointer {elt= Function {args= typs}} ->
          Vector.for_all2_exn typs args ~f:(fun typ arg ->
              Typ.equal typ (Exp.typ arg) )
      | _ -> false ) ;
    Call
      { call= {dst= func; args; retreating= false}
      ; return
      ; throw
      ; ignore_result
      ; loc }

  let mkReturn ~exp ~loc = Return {exp; loc}

  let mkThrow ~exc ~loc = Throw {exc; loc}

  let mkUnreachable = Unreachable

  let fmt ff term =
    let pf fmt = Format.fprintf ff fmt in
    let fmt_goto ff jmp = Format.fprintf ff "goto %a;" Jump.fmt jmp in
    match term with
    | Switch {key; tbl; els} -> (
      match Vector.to_array tbl with
      | [||] -> pf "%a" fmt_goto els
      | [|(z, jmp)|] when Z.equal Z.one z ->
          pf "@[if (%a)@;<1 2>%a@ @[else@;<1 2>%a@]@]" Exp.fmt key fmt_goto
            jmp fmt_goto els
      | _ ->
          pf "@[<2>switch %a@ @[%a@ else: %a@]@]" Exp.fmt key
            (vector_fmt "@ " (fun ff (z, jmp) ->
                 Format.fprintf ff "%a: %a" Z.pp_print z fmt_goto jmp ))
            tbl fmt_goto els )
    | ISwitch {ptr; tbl} ->
        pf "@[<2>iswitch %a@ @[<hv>%a@]@]" Exp.fmt ptr
          (vector_fmt "@ " (fun ff ({dst= {lbl}; _} as jmp) ->
               Format.fprintf ff "%s: %a" lbl fmt_goto jmp ))
          tbl
    | Call {call= {dst; args; retreating}; return; throw} ->
        pf "@[<2>@[<7>call @[<2>%s%a%a@]@]@ @[returnto %a%a;@]@]"
          (if retreating then "↑" else "")
          Exp.fmt dst (fmt_args Exp.fmt) args Jump.fmt return
          (option_fmt "@ throwto %a" Jump.fmt)
          throw
    | Return {exp} -> pf "return%a" (option_fmt " %a" Exp.fmt) exp
    | Throw {exc} -> pf "throw %a" Exp.fmt exc
    | Unreachable -> pf "unreachable"
end

module Block = struct
  type t = block [@@deriving sexp]

  let mk ~lbl ~params ~cmnd ~term =
    assert (
      not (List.contains_dup (Vector.to_list params) ~compare:Var.compare)
    ) ;
    {dummy_block with lbl; params; cmnd; term}

  (* blocks in a [t] are uniquely identified by [sort_index] *)
  let compare x y = Int.compare x.sort_index y.sort_index

  let hash {sort_index} = Int.hash sort_index

  let fmt ff {lbl; params; cmnd; term; sort_index} =
    Format.fprintf ff "@[<v 2>@[<4>%s%a@]: #%i@ @[<v>%a%t%a@]@]" lbl
      (fmt_args fmt_param) params sort_index fmt_cmnd cmnd
      (fun ff -> if Vector.is_empty cmnd then () else Format.fprintf ff "@ ")
      Term.fmt term
end

module Func = struct
  type t = func

  let find functions func =
    Vector.find functions ~f:(fun {name} -> Global.equal func name)

  let is_undefined = function
    | {entry= {cmnd; term= Unreachable}} -> Vector.is_empty cmnd
    | _ -> false

  let fold_term {entry; cfg} ~init ~f =
    let fold_block {term} ~init ~f = f init term in
    Vector.fold cfg ~init:(fold_block entry ~init ~f) ~f:(fun z k ->
        fold_block k ~init:z ~f )

  let mk ~name ~entry ~cfg =
    let func = {name; entry; cfg} in
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
      | ISwitch {tbl} -> Vector.iter tbl ~f:set_dst
      | Call {return; throw} ->
          set_dst return ;
          Option.iter throw ~f:set_dst
      | Return _ | Throw _ | Unreachable -> ()
    in
    resolve_parent_and_jumps entry ;
    Vector.iter cfg ~f:resolve_parent_and_jumps ;
    assert (
      not
        (List.contains_dup (Vector.to_list cfg) ~compare:(fun b1 b2 ->
             String.compare b1.lbl b2.lbl )) ) ;
    assert (
      let check_jump ?retn_typ {dst= {params}; args} =
        let rev_frml_typs =
          Vector.fold params ~init:[] ~f:(fun rev_frml_typs frml ->
              Var.typ frml :: rev_frml_typs )
        in
        let rev_actl_typs =
          Option.to_list retn_typ
          @ Vector.fold args ~init:[] ~f:(fun rev_actl_typs actl ->
                Exp.typ actl :: rev_actl_typs )
        in
        List.iter2_exn rev_frml_typs rev_actl_typs ~f:(fun frml actl ->
            assert (Typ.equal frml actl && Typ.is_sized frml) )
      in
      fold_term func ~init:() ~f:(fun () -> function
        | Switch {tbl; els} ->
            Vector.iter tbl ~f:(fun (_, jmp) -> check_jump jmp) ;
            check_jump els
        | ISwitch {tbl} -> Vector.iter tbl ~f:check_jump
        | Call {call= {dst; args= actls}; return; throw; ignore_result} -> (
          match Exp.typ dst with
          | Pointer {elt= Function {args= frmls; return= retn_typ}} ->
              Vector.iter2_exn frmls actls ~f:(fun frml actl ->
                  assert (Typ.equal frml (Exp.typ actl)) ) ;
              check_jump return
                ?retn_typ:(if ignore_result then None else retn_typ) ;
              Option.iter throw ~f:(fun throw ->
                  check_jump throw ~retn_typ:Typ.i8p )
          | _ -> assert false )
        | Return {exp} -> (
            let typ = Option.map exp ~f:Exp.typ in
            match Global.typ name with
            | Pointer {elt= Function {return}} ->
                assert (Option.equal Typ.equal typ return)
            | _ -> assert false )
        | Throw {exc} -> assert (Typ.equal Typ.i8p (Exp.typ exc))
        | Unreachable -> () ) ;
      true ) ;
    func

  let mk_undefined ~name ~params =
    let entry =
      Block.mk ~lbl:"" ~params ~cmnd:Vector.empty ~term:Term.mkUnreachable
    in
    let cfg = Vector.empty in
    mk ~name ~entry ~cfg

  let fmt ff ({name; entry= {params; cmnd; term; sort_index}; cfg} as func) =
    let fmt_if cnd str ff = if cnd then Format.fprintf ff str in
    let[@warning "p"] Typ.Pointer {elt= Function {return}} =
      Global.typ name
    in
    Format.fprintf ff "@[<v>@[<v>%a@[<2>%a%a@]%t@]"
      (option_fmt "%a " Typ.fmt)
      return Global.fmt name (fmt_args fmt_param) params (fun ff ->
        if is_undefined func then Format.fprintf ff " #%i@]" sort_index
        else
          Format.fprintf ff " { #%i @;<1 4>@[<v>%a@ %a@]%t%a@]@ }"
            sort_index fmt_cmnd cmnd Term.fmt term
            (fmt_if (not (Vector.is_empty cfg)) "@ @   ")
            (vector_fmt "@\n@\n  " Block.fmt)
            cfg )
end

module Block_id = struct
  type t = block [@@deriving sexp]

  (* block labels within a function are unique *)
  let compare x y =
    [%compare : string * Global.t]
      (x.lbl, x.parent.name) (y.lbl, y.parent.name)

  let hash b = Hashtbl.hash (b.lbl, b.parent.name)
end

module BS = Set.Make (Block_id)
module BQ = Hash_queue.Make (Block_id)
module FQ = Hash_queue.Make (Global)

let set_derived_metadata functions =
  let compute_roots functions =
    let roots = FQ.create () in
    Array.iter functions ~f:(fun func -> FQ.enqueue_exn roots func.name func) ;
    Array.iter functions ~f:(fun func ->
        Func.fold_term func ~init:() ~f:(fun () -> function
          | Call {call= {dst}} -> (
            match Global.of_exp dst with
            | Some callee -> FQ.remove roots callee |> ignore
            | None -> () )
          | _ -> () ) ) ;
    roots
  in
  let topsort functions roots =
    let tips_to_roots = BQ.create () in
    let rec visit ancestors func src =
      if not (BQ.mem tips_to_roots src) then (
        let ancestors = BS.add ancestors src in
        let jump jmp =
          if BS.mem ancestors jmp.dst then jmp.retreating <- true
          else visit ancestors func jmp.dst
        in
        ( match src.term with
        | Switch {tbl; els} ->
            Vector.iter tbl ~f:(fun (_, jmp) -> jump jmp) ;
            jump els
        | ISwitch {tbl} -> Vector.iter tbl ~f:jump
        | Call {call= {dst} as call; return; throw} ->
            ( match Global.of_exp dst with
            | Some name -> (
              match Func.find (Vector.of_array functions) name with
              | Some func ->
                  if BS.mem ancestors func.entry then
                    call.retreating <- true
                  else visit ancestors func func.entry
              | None ->
                  fail "Call to unknown function: %a" Global.fmt name () )
            | None ->
                (* conservatively assume all virtual calls are recursive *)
                call.retreating <- true ) ;
            jump return ; Option.iter ~f:jump throw
        | Return _ | Throw _ | Unreachable -> () ) ;
        BQ.enqueue_exn tips_to_roots src () )
    in
    FQ.iter roots ~f:(fun root -> visit BS.empty root root.entry) ;
    tips_to_roots
  in
  let set_sort_indices tips_to_roots =
    let index = ref (BQ.length tips_to_roots) in
    BQ.iteri tips_to_roots ~f:(fun ~key:block ~data:_ ->
        block.sort_index <- !index ;
        decr index )
  in
  let sort_cfgs functions =
    Array.iter functions ~f:(fun {cfg} ->
        Array.sort
          ~cmp:(fun x y -> Int.compare x.sort_index y.sort_index)
          (Vector.to_array cfg) )
  in
  let sort_functions functions =
    Array.sort
      ~cmp:(fun x y -> Int.compare x.entry.sort_index y.entry.sort_index)
      functions
  in
  let functions = Array.of_list functions in
  let roots = compute_roots functions in
  let tips_to_roots = topsort functions roots in
  set_sort_indices tips_to_roots ;
  sort_cfgs functions ;
  sort_functions functions ;
  Vector.of_array functions

let mk ~typ_defns ~globals ~functions =
  assert (
    not
      (List.contains_dup typ_defns ~compare:(fun (s: Typ.t) (t: Typ.t) ->
           match (s, t) with
           | ( (Struct {name= n1} | Opaque {name= n1})
             , (Struct {name= n2} | Opaque {name= n2}) ) ->
               String.compare n1 n2
           | _ -> Typ.compare s t ))
    && not
         (List.contains_dup globals ~compare:(fun g1 g2 ->
              String.compare (Global.name g1) (Global.name g2) ))
    && not
         (List.contains_dup functions ~compare:(fun f1 f2 ->
              String.compare (Global.name f1.name) (Global.name f2.name) ))
  ) ;
  { typ_defns
  ; globals= Vector.of_list_rev globals
  ; functions= set_derived_metadata functions }

let fmt ff {typ_defns; globals; functions} =
  Format.fprintf ff "@[<v>@[%a@]@ @ @ @[%a@]@ @ @ @[%a@]@]"
    (list_fmt "@\n@\n" Typ.fmt_defn)
    typ_defns
    (vector_fmt "@\n@\n" Global.fmt_defn)
    globals
    (vector_fmt "@\n@\n" Func.fmt)
    functions
