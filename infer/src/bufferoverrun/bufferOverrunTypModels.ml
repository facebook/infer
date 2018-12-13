(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
module BoUtils = BufferOverrunUtils
module Dom = BufferOverrunDomain

type model_env = BufferOverrunModels.model_env =
  { pname: Typ.Procname.t
  ; node_hash: int
  ; location: Location.t
  ; tenv: Tenv.t
  ; integer_type_widths: Typ.IntegerWidths.t }

type declare_local_fun =
     decl_local:BoUtils.Exec.decl_local
  -> model_env
  -> Loc.t
  -> inst_num:int
  -> represents_multiple_values:bool
  -> dimension:int
  -> Dom.Mem.t
  -> Dom.Mem.t * int

type declare_symbolic_fun =
     decl_sym_val:BoUtils.Exec.decl_sym_val
  -> Itv.SymbolPath.partial
  -> model_env
  -> depth:int
  -> Loc.t
  -> Dom.Mem.t
  -> Dom.Mem.t

type typ_model = {declare_local: declare_local_fun; declare_symbolic: declare_symbolic_fun}

let std_array typ length =
  let declare_local ~decl_local {pname; node_hash; location} loc ~inst_num
      ~represents_multiple_values ~dimension mem =
    (* should this be deferred to the constructor? *)
    let length = Some (IntLit.of_int64 length) in
    BoUtils.Exec.decl_local_array ~decl_local pname ~node_hash location loc typ ~length ~inst_num
      ~represents_multiple_values ~dimension mem
  in
  let declare_symbolic ~decl_sym_val path {pname; tenv; location} ~depth loc mem =
    let offset = Itv.zero in
    let size = Itv.of_int64 length in
    BoUtils.Exec.decl_sym_arr ~decl_sym_val Symb.SymbolPath.Deref_ArrayIndex pname path tenv
      location ~depth loc typ ~offset ~size mem
  in
  {declare_local; declare_symbolic}


(* Java's Collections are represented by their size. We don't care about the elements.
- when they are constructed, we set the size to 0
- each time we add an element, we increase the length of the array
- each time we delete an element, we decrease the length of the array *)

let collection =
  let declare_local ~decl_local:_ {pname; node_hash; location} loc ~inst_num
      ~represents_multiple_values ~dimension mem =
    BoUtils.Exec.decl_local_collection pname ~node_hash location loc ~inst_num
      ~represents_multiple_values ~dimension mem
  in
  let declare_symbolic ~decl_sym_val:_ path {location} ~depth:_ loc mem =
    BoUtils.Exec.decl_sym_collection path location loc mem
  in
  {declare_local; declare_symbolic}


let dispatch : (Tenv.t, typ_model) ProcnameDispatcher.TypName.dispatcher =
  let open ProcnameDispatcher.TypName in
  make_dispatcher
    [ -"std" &:: "array" < capt_typ `T &+ capt_int >--> std_array
    ; +PatternMatch.implements_collection &::.*--> collection
    ; +PatternMatch.implements_iterator &::.*--> collection ]
