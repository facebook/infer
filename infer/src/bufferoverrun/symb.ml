(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module BoundEnd = struct
  type t = LowerBound | UpperBound [@@deriving compare]

  let equal = [%compare.equal: t]

  let neg = function LowerBound -> UpperBound | UpperBound -> LowerBound

  let to_string = function LowerBound -> "lb" | UpperBound -> "ub"
end

module SymbolPath = struct
  type deref_kind =
    | Deref_ArrayIndex
    | Deref_COneValuePointer
    | Deref_CPointer
    | Deref_JavaPointer

  let compare_deref_kind _ _ = 0

  type partial =
    | Pvar of Pvar.t
    | Deref of deref_kind * partial
    | Field of Typ.Fieldname.t * partial
    | Callsite of {ret_typ: Typ.t; cs: CallSite.t}
  [@@deriving compare]

  type t = Normal of partial | Offset of partial | Length of partial [@@deriving compare]

  let equal = [%compare.equal: t]

  let equal_partial = [%compare.equal: partial]

  let of_pvar pvar = Pvar pvar

  let of_callsite ~ret_typ cs = Callsite {ret_typ; cs}

  let field p fn = Field (fn, p)

  let deref ~deref_kind p = Deref (deref_kind, p)

  let normal p = Normal p

  let offset p = Offset p

  let length p = Length p

  let is_this = function Pvar pvar -> Pvar.is_this pvar || Pvar.is_self pvar | _ -> false

  let rec get_pvar = function
    | Pvar pvar ->
        Some pvar
    | Deref (_, partial) | Field (_, partial) ->
        get_pvar partial
    | Callsite _ ->
        None


  let rec pp_partial_paren ~paren fmt = function
    | Pvar pvar ->
        Pvar.pp_value fmt pvar
    | Deref (Deref_JavaPointer, p) when Config.bo_debug < 3 ->
        pp_partial_paren ~paren fmt p
    | Deref (Deref_ArrayIndex, p) ->
        F.fprintf fmt "%a[*]" (pp_partial_paren ~paren:true) p
    | Deref ((Deref_COneValuePointer | Deref_CPointer | Deref_JavaPointer), p) ->
        pp_pointer ~paren fmt p
    | Field (fn, Deref ((Deref_COneValuePointer | Deref_CPointer), p)) ->
        BufferOverrunField.pp ~pp_lhs:(pp_partial_paren ~paren:true)
          ~pp_lhs_alone:(pp_pointer ~paren) ~sep:"->" fmt p fn
    | Field (fn, p) ->
        BufferOverrunField.pp ~pp_lhs:(pp_partial_paren ~paren:true)
          ~pp_lhs_alone:(pp_partial_paren ~paren) ~sep:"." fmt p fn
    | Callsite {cs} ->
        F.fprintf fmt "%s" (Typ.Procname.to_simplified_string ~withclass:true (CallSite.pname cs))


  and pp_pointer ~paren fmt p =
    if paren then F.fprintf fmt "(" ;
    F.fprintf fmt "*%a" (pp_partial_paren ~paren:false) p ;
    if paren then F.fprintf fmt ")"


  let pp_partial = pp_partial_paren ~paren:false

  let pp fmt = function
    | Normal p ->
        pp_partial fmt p
    | Offset p ->
        F.fprintf fmt "%a.offset" pp_partial p
    | Length p ->
        F.fprintf fmt "%a.length" pp_partial p


  let pp_mark ~markup = if markup then MarkupFormatter.wrap_monospaced pp else pp

  let rec represents_multiple_values = function
    (* TODO depending on the result, the call might represent multiple values *)
    | Callsite _ | Pvar _ ->
        false
    | Deref (Deref_ArrayIndex, _) ->
        true
    | Deref (Deref_CPointer, p)
    (* Deref_CPointer is unsound here but avoids many FPs for non-array pointers *)
    | Deref ((Deref_COneValuePointer | Deref_JavaPointer), p)
    | Field (_, p) ->
        represents_multiple_values p


  let rec represents_multiple_values_sound = function
    | Callsite _ ->
        true
    | Pvar _ ->
        false
    | Deref ((Deref_ArrayIndex | Deref_CPointer), _) ->
        true
    | Deref ((Deref_COneValuePointer | Deref_JavaPointer), p) | Field (_, p) ->
        represents_multiple_values_sound p


  let rec represents_callsite_sound_partial = function
    | Callsite _ ->
        true
    | Pvar _ ->
        false
    | Deref (_, p) | Field (_, p) ->
        represents_callsite_sound_partial p


  let rec exists_str_partial ~f = function
    | Pvar pvar ->
        f (Pvar.to_string pvar)
    | Deref (_, x) ->
        exists_str_partial ~f x
    | Field (fld, x) ->
        f (Typ.Fieldname.to_string fld) || exists_str_partial ~f x
    | Callsite _ ->
        false


  let exists_str ~f = function Normal p | Offset p | Length p -> exists_str_partial ~f p
end

module Symbol = struct
  type extra_bool = bool

  let compare_extra_bool _ _ = 0

  type t =
    | OneValue of {unsigned: extra_bool; path: SymbolPath.t}
    | BoundEnd of {unsigned: extra_bool; path: SymbolPath.t; bound_end: BoundEnd.t}
  [@@deriving compare]

  let compare s1 s2 =
    match (s1, s2) with
    | OneValue _, BoundEnd _ ->
        -1
    | BoundEnd _, OneValue _ ->
        1
    | OneValue {unsigned= unsigned1}, OneValue {unsigned= unsigned2}
    | BoundEnd {unsigned= unsigned1}, BoundEnd {unsigned= unsigned2} ->
        let r = compare s1 s2 in
        if Int.equal r 0 then assert (Bool.equal unsigned1 unsigned2) ;
        r


  type 'res eval = t -> BoundEnd.t -> 'res AbstractDomain.Types.bottom_lifted

  let equal = [%compare.equal: t]

  let paths_equal s1 s2 =
    match (s1, s2) with
    | OneValue _, BoundEnd _ | BoundEnd _, OneValue _ ->
        false
    | OneValue {path= path1}, OneValue {path= path2}
    | BoundEnd {path= path1}, BoundEnd {path= path2} ->
        SymbolPath.equal path1 path2


  let make_onevalue : unsigned:bool -> SymbolPath.t -> t =
   fun ~unsigned path -> OneValue {unsigned; path}


  let make_boundend : BoundEnd.t -> unsigned:bool -> SymbolPath.t -> t =
   fun bound_end ~unsigned path -> BoundEnd {unsigned; path; bound_end}


  let pp : F.formatter -> t -> unit =
   fun fmt s ->
    match s with
    | OneValue {unsigned; path} | BoundEnd {unsigned; path} ->
        SymbolPath.pp fmt path ;
        ( if Config.developer_mode then
          match s with
          | BoundEnd {bound_end} ->
              Format.fprintf fmt ".%s" (BoundEnd.to_string bound_end)
          | OneValue _ ->
              () ) ;
        if Config.bo_debug > 1 then F.fprintf fmt "(%c)" (if unsigned then 'u' else 's')


  let pp_mark ~markup = if markup then MarkupFormatter.wrap_monospaced pp else pp

  let is_unsigned : t -> bool = function OneValue {unsigned} | BoundEnd {unsigned} -> unsigned

  let path = function OneValue {path} | BoundEnd {path} -> path

  let assert_bound_end s be =
    match s with OneValue _ -> () | BoundEnd {bound_end} -> assert (BoundEnd.equal be bound_end)


  let exists_str ~f = function OneValue {path} | BoundEnd {path} -> SymbolPath.exists_str ~f path
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
