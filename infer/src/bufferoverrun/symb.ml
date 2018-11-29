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
  type deref_kind = Deref_ArrayIndex | Deref_CPointer [@@deriving compare]

  type partial =
    | Pvar of Pvar.t
    | Deref of deref_kind * partial
    | Field of Typ.Fieldname.t * partial
  [@@deriving compare]

  type t = Normal of partial | Offset of partial | Length of partial [@@deriving compare]

  let equal = [%compare.equal: t]

  let equal_partial = [%compare.equal: partial]

  let of_pvar pvar = Pvar pvar

  let field p fn = Field (fn, p)

  let deref ~deref_kind p = Deref (deref_kind, p)

  let normal p = Normal p

  let offset p = Offset p

  let length p = Length p

  let rec pp_partial_paren ~paren fmt = function
    | Pvar pvar ->
        Pvar.pp_value fmt pvar
    | Deref (Deref_ArrayIndex, p) ->
        F.fprintf fmt "%a[*]" (pp_partial_paren ~paren:true) p
    | Deref (Deref_CPointer, p) ->
        if paren then F.fprintf fmt "(" ;
        F.fprintf fmt "*%a" (pp_partial_paren ~paren:false) p ;
        if paren then F.fprintf fmt ")"
    | Field (fn, Deref (Deref_CPointer, p)) ->
        F.fprintf fmt "%a->%s" (pp_partial_paren ~paren:true) p (Typ.Fieldname.to_flat_string fn)
    | Field (fn, p) ->
        F.fprintf fmt "%a.%s" (pp_partial_paren ~paren:true) p (Typ.Fieldname.to_flat_string fn)


  let pp_partial = pp_partial_paren ~paren:false

  let pp fmt = function
    | Normal p ->
        pp_partial fmt p
    | Offset p ->
        F.fprintf fmt "%a.offset" pp_partial p
    | Length p ->
        F.fprintf fmt "%a.length" pp_partial p


  let rec represents_multiple_values = function
    | Pvar _ ->
        false
    | Deref (Deref_ArrayIndex, _) ->
        true
    | Deref (Deref_CPointer, p)
    (* unsound but avoids many FPs for non-array pointers *)
    | Field (_, p) ->
        represents_multiple_values p


  let pp_mark ~markup = if markup then MarkupFormatter.wrap_monospaced pp else pp
end

module Symbol = struct
  type t =
    {id: int; pname: Typ.Procname.t; unsigned: bool; path: SymbolPath.t; bound_end: BoundEnd.t}

  type 'res eval = t -> 'res AbstractDomain.Types.bottom_lifted

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
    SymbolPath.pp fmt path ;
    if Config.developer_mode then Format.fprintf fmt ".%s" (BoundEnd.to_string bound_end) ;
    if Config.bo_debug > 1 then
      let symbol_name = if unsigned then 'u' else 's' in
      F.fprintf fmt "(%s-%c$%d)" (Typ.Procname.to_string pname) symbol_name id


  let pp_mark ~markup = if markup then MarkupFormatter.wrap_monospaced pp else pp

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

module SymbolSet = struct
  include PrettyPrintable.MakePPSet (Symbol)

  let union3 x y z = union (union x y) z
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
end
