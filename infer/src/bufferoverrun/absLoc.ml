(*
 * Copyright (c) 2016 - present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format

module Allocsite =
struct
  include String
  let pp fmt s = Format.fprintf fmt "%s" s
  let make x = x
end

module Loc =
struct
  type t =
    | Var of Var.t
    | Allocsite of Allocsite.t
    | Field of t * Ident.fieldname
  [@@deriving compare]

  let rec pp fmt = function
    | Var v ->
        Var.pp F.str_formatter v;
        let s = F.flush_str_formatter () in
        if s.[0] = '&' then
          F.fprintf fmt "%s" (String.sub s 1 (String.length s - 1))
        else F.fprintf fmt "%s" s
    | Allocsite a -> Allocsite.pp fmt a
    | Field (l, f) -> F.fprintf fmt "%a.%a" pp l Ident.pp_fieldname f
  let is_var = function Var _ -> true | _ -> false
  let is_pvar_in_reg v =
    Var.pp F.str_formatter v;
    let s = F.flush_str_formatter () in
    s.[0] = '&'
  let is_logical_var = function
    | Var (Var.LogicalVar _) -> true
    | _ -> false
  let of_var v = Var v
  let of_allocsite a = Allocsite a
  let of_pvar pvar = Var (Var.of_pvar pvar)
  let of_id id = Var (Var.of_id id)
  let append_field l f = Field (l, f)

  let is_return = function
    | Var (Var.ProgramVar x) ->
        Mangled.equal (Pvar.get_name x) Ident.name_return
    | _ -> false
end

module PowLoc =
struct
  include AbstractDomain.FiniteSet
      (struct
        include Set.Make (struct type t = Loc.t [@@deriving compare] end)
        let pp_element fmt e = Loc.pp fmt e
        let pp fmt s =
          Format.fprintf fmt "{";
          iter (fun e -> Format.fprintf fmt "%a," pp_element e) s;
          Format.fprintf fmt "}"
      end)

  let bot = empty

  let of_pvar pvar = singleton (Loc.of_pvar pvar)
  let of_id id = singleton (Loc.of_id id)
  let append_field ploc fn = fold (fun l -> add (Loc.append_field l fn)) ploc empty
end
