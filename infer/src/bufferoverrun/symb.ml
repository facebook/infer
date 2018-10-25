(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module BoundEnd = struct
  type t = LowerBound | UpperBound

  let neg = function LowerBound -> UpperBound | UpperBound -> LowerBound

  let to_string = function LowerBound -> "lb" | UpperBound -> "ub"
end

module SymbolPath = struct
  type partial = Pvar of Pvar.t | Index of partial | Field of Typ.Fieldname.t * partial
  [@@deriving compare]

  type t = Normal of partial | Offset of partial | Length of partial [@@deriving compare]

  let equal = [%compare.equal: t]

  let of_pvar pvar = Pvar pvar

  let field p fn = Field (fn, p)

  let index p = Index p

  let normal p = Normal p

  let offset p = Offset p

  let length p = Length p

  let rec pp_partial fmt = function
    | Pvar pvar ->
        Pvar.pp_value fmt pvar
    | Index p ->
        F.fprintf fmt "%a[*]" pp_partial p
    | Field (fn, p) ->
        F.fprintf fmt "%a.%s" pp_partial p (Typ.Fieldname.to_flat_string fn)


  let pp fmt = function
    | Normal p ->
        pp_partial fmt p
    | Offset p ->
        F.fprintf fmt "%a.offset" pp_partial p
    | Length p ->
        F.fprintf fmt "%a.length" pp_partial p
end

module Symbol = struct
  type t =
    {id: int; pname: Typ.Procname.t; unsigned: bool; path: SymbolPath.t; bound_end: BoundEnd.t}

  let compare s1 s2 =
    (* Symbols only make sense within a given function, so shouldn't be compared across function boundaries. *)
    assert (phys_equal s1.pname s2.pname) ;
    Int.compare s1.id s2.id


  let equal = [%compare.equal: t]

  let paths_equal s1 s2 = SymbolPath.equal s1.path s2.path

  let make : unsigned:bool -> Typ.Procname.t -> SymbolPath.t -> BoundEnd.t -> int -> t =
   fun ~unsigned pname path bound_end id -> {id; pname; unsigned; path; bound_end}


  let pp : F.formatter -> t -> unit =
   fun fmt {pname; id; unsigned; path; bound_end} ->
    F.fprintf fmt "%a.%s" SymbolPath.pp path (BoundEnd.to_string bound_end) ;
    if Config.bo_debug > 1 then
      let symbol_name = if unsigned then 'u' else 's' in
      F.fprintf fmt "(%s-%c$%d)" (Typ.Procname.to_string pname) symbol_name id


  let is_unsigned : t -> bool = fun x -> x.unsigned

  let path {path} = path

  let bound_end {bound_end} = bound_end
end

module SymbolTable = struct
  module M = PrettyPrintable.MakePPMap (SymbolPath)

  type t = (Symbol.t * Symbol.t) M.t ref

  let empty () = ref M.empty

  let lookup ~unsigned pname path symbol_table new_sym_num =
    match M.find_opt path !symbol_table with
    | Some s ->
        s
    | None ->
        let s =
          ( Symbol.make ~unsigned pname path LowerBound (Counter.next new_sym_num)
          , Symbol.make ~unsigned pname path UpperBound (Counter.next new_sym_num) )
        in
        symbol_table := M.add path s !symbol_table ;
        s
end

module SymbolMap = struct
  include PrettyPrintable.MakePPMap (Symbol)

  let for_all2 : f:(key -> 'a option -> 'b option -> bool) -> 'a t -> 'b t -> bool =
   fun ~f x y ->
    match merge (fun k x y -> if f k x y then None else raise Exit) x y with
    | _ ->
        true
    | exception Exit ->
        false


  let is_singleton : 'a t -> (key * 'a) option =
   fun m ->
    if is_empty m then None
    else
      let ((kmin, _) as binding) = min_binding m in
      let kmax, _ = max_binding m in
      if Symbol.equal kmin kmax then Some binding else None
end
