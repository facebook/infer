(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Allocsite = struct
  type t =
    | Unknown
    | Known of
        { proc_name: string
        ; node_hash: int
        ; inst_num: int
        ; dimension: int
        ; path: Symb.SymbolPath.partial option }
  [@@deriving compare]

  let pp fmt = function
    | Unknown ->
        F.fprintf fmt "Unknown"
    | Known {proc_name : string; node_hash : int; inst_num : int; dimension : int} ->
        F.fprintf fmt "%s-%d-%d-%d" proc_name node_hash inst_num dimension


  let to_string x = F.asprintf "%a" pp x

  let make :
         Typ.Procname.t
      -> node_hash:int
      -> inst_num:int
      -> dimension:int
      -> path:Symb.SymbolPath.partial option
      -> t =
   fun proc_name ~node_hash ~inst_num ~dimension ~path ->
    Known {proc_name= Typ.Procname.to_string proc_name; node_hash; inst_num; dimension; path}


  let unknown = Unknown

  let get_path = function Unknown -> None | Known {path} -> path
end

module Loc = struct
  type t = Var of Var.t | Allocsite of Allocsite.t | Field of t * Typ.Fieldname.t
  [@@deriving compare]

  let equal = [%compare.equal: t]

  let unknown = Allocsite Allocsite.unknown

  let rec pp fmt = function
    | Var v ->
        Var.pp F.str_formatter v ;
        let s = F.flush_str_formatter () in
        if Char.equal s.[0] '&' then
          F.pp_print_string fmt (String.sub s ~pos:1 ~len:(String.length s - 1))
        else F.pp_print_string fmt s
    | Allocsite a ->
        Allocsite.pp fmt a
    | Field (l, f) ->
        F.fprintf fmt "%a.%a" pp l Typ.Fieldname.pp f


  let to_string x = F.asprintf "%a" pp x

  let is_var = function Var _ -> true | _ -> false

  let rec contains_allocsite = function
    | Var _ ->
        false
    | Allocsite _ ->
        true
    | Field (loc, _) ->
        contains_allocsite loc


  let of_var v = Var v

  let of_allocsite a = Allocsite a

  let of_pvar pvar = Var (Var.of_pvar pvar)

  let of_id id = Var (Var.of_id id)

  let append_field l ~fn = Field (l, fn)

  let is_return = function
    | Var (Var.ProgramVar x) ->
        Mangled.equal (Pvar.get_name x) Ident.name_return
    | _ ->
        false


  let is_field_of ~loc ~field_loc = match field_loc with Field (l, _) -> equal loc l | _ -> false

  let rec get_path = function
    | Var (LogicalVar _) ->
        None
    | Var (ProgramVar pvar) ->
        Some (Symb.SymbolPath.of_pvar pvar)
    | Allocsite allocsite ->
        Allocsite.get_path allocsite
    | Field (l, fn) ->
        Option.map (get_path l) ~f:(fun p -> Symb.SymbolPath.field p fn)
end

module PowLoc = struct
  include AbstractDomain.FiniteSet (Loc)

  let bot = empty

  let is_bot = is_empty

  let unknown = singleton Loc.unknown

  let append_field ploc ~fn =
    if is_bot ploc then singleton Loc.unknown
    else fold (fun l -> add (Loc.append_field l ~fn)) ploc empty
end

(** unsound but ok for bug catching *)
let always_strong_update = true

let can_strong_update : PowLoc.t -> bool =
 fun ploc ->
  if always_strong_update then true
  else
    match PowLoc.is_singleton_or_more ploc with
    | IContainer.Singleton loc ->
        Loc.is_var loc
    | _ ->
        false
