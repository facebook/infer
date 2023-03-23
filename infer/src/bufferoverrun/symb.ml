(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module BoField = BufferOverrunField

module BoundEnd = struct
  type t = LowerBound | UpperBound [@@deriving compare, equal]

  let neg = function LowerBound -> UpperBound | UpperBound -> LowerBound

  let to_string = function LowerBound -> "lb" | UpperBound -> "ub"
end

module SymbolPath = struct
  type deref_kind = Deref_ArrayIndex | Deref_COneValuePointer | Deref_CPointer | Deref_JavaPointer

  type callsite = CallSite.t

  let compare_callsite x y = Procname.compare (CallSite.pname x) (CallSite.pname y)

  let equal_callsite x y = Procname.equal (CallSite.pname x) (CallSite.pname y)

  type prim =
    | Pvar of Pvar.t
    | Deref of (deref_kind[@ignore]) * partial
    | Callsite of {ret_typ: Typ.t; cs: callsite; obj_path: partial option [@ignore]}

  and partial = prim BoField.t [@@deriving compare, equal]

  let of_pvar pvar = BoField.Prim (Pvar pvar)

  let of_callsite ?obj_path ~ret_typ cs = BoField.Prim (Callsite {ret_typ; cs; obj_path})

  let deref ~deref_kind p = BoField.Prim (Deref (deref_kind, p))

  let prim_append_field ?typ p0 fn aux depth = function
    | Pvar _ | Callsite _ ->
        BoField.Field {fn; prefix= p0; typ}
    | Deref (_, p) ->
        aux ~depth:(depth + 1) p


  let prim_append_star_field p0 fn aux = function
    | Pvar _ | Callsite _ ->
        BoField.StarField {prefix= p0; last_field= fn}
    | Deref (_, p) ->
        aux p


  let append_field = BoField.mk_append_field ~prim_append_field ~prim_append_star_field

  let append_star_field = BoField.mk_append_star_field ~prim_append_star_field

  type t =
    | Normal of partial
    | Offset of {p: partial; is_void: bool}
    | Length of {p: partial; is_void: bool}
    | Modeled of partial
  [@@deriving compare, equal]

  let normal p = Normal p

  let offset p ~is_void = Offset {p; is_void}

  let length p ~is_void = Length {p; is_void}

  let modeled p = Modeled p

  let is_this = function
    | BoField.Prim (Pvar pvar) ->
        Pvar.is_this pvar || Pvar.is_self pvar
    | _ ->
        false


  let rec get_pvar = function
    | BoField.Prim (Pvar pvar) ->
        Some pvar
    | BoField.(Prim (Deref (_, partial)) | Field {prefix= partial} | StarField {prefix= partial}) ->
        get_pvar partial
    | BoField.Prim (Callsite _) ->
        None


  let rec pp_partial_paren ~paren fmt = function
    | BoField.Prim (Pvar pvar) ->
        if Config.bo_debug >= 3 then Pvar.pp_value fmt pvar else Pvar.pp_value_non_verbose fmt pvar
    | BoField.Prim (Deref ((Deref_CPointer | Deref_JavaPointer), p)) when Config.bo_debug < 3 ->
        pp_partial_paren ~paren fmt p
    | BoField.Prim (Deref (Deref_ArrayIndex, p)) ->
        F.fprintf fmt "%a[*]" (pp_partial_paren ~paren:true) p
    | BoField.Prim (Deref ((Deref_COneValuePointer | Deref_CPointer | Deref_JavaPointer), p)) ->
        pp_pointer ~paren fmt p
    | BoField.Field {fn; prefix= Prim (Deref ((Deref_COneValuePointer | Deref_CPointer), p))} ->
        BufferOverrunField.pp ~pp_lhs:(pp_partial_paren ~paren:true) ~sep:"->" fmt p fn
    | BoField.Field {fn; prefix= p} ->
        BufferOverrunField.pp ~pp_lhs:(pp_partial_paren ~paren:true) ~sep:"." fmt p fn
    | BoField.Prim (Callsite {cs; obj_path= Some obj_path}) ->
        if paren then F.pp_print_string fmt "(" ;
        F.fprintf fmt "%a.%a" (pp_partial_paren ~paren:false) obj_path
          (Procname.pp_simplified_string ~withclass:false)
          (CallSite.pname cs) ;
        if paren then F.pp_print_string fmt ")"
    | BoField.Prim (Callsite {cs; obj_path= None}) ->
        Procname.pp_simplified_string ~withclass:true fmt (CallSite.pname cs)
    | BoField.StarField {last_field; prefix} ->
        BufferOverrunField.pp ~pp_lhs:(pp_star ~paren:true) ~sep:"." fmt prefix last_field


  and pp_pointer ~paren fmt p =
    if paren then F.fprintf fmt "(" ;
    F.fprintf fmt "*%a" (pp_partial_paren ~paren:false) p ;
    if paren then F.fprintf fmt ")"


  and pp_star ~paren fmt p =
    pp_partial_paren ~paren fmt p ;
    F.pp_print_string fmt ".*"


  let pp_partial = pp_partial_paren ~paren:false

  let pp_is_void fmt is_void = if is_void then F.fprintf fmt "(v)"

  let pp fmt = function
    | Modeled p ->
        F.fprintf fmt "%a.modeled" pp_partial p
    | Normal p ->
        pp_partial fmt p
    | Offset {p; is_void} ->
        F.fprintf fmt "%a.offset%a" pp_partial p pp_is_void is_void
    | Length {p= Field {fn; prefix= p}; is_void}
      when BufferOverrunField.is_java_collection_internal_array fn ->
        F.fprintf fmt "%a.length%a" pp_partial p pp_is_void is_void
    | Length {p= StarField {last_field= fn; prefix= p}; is_void}
      when BufferOverrunField.is_java_collection_internal_array fn ->
        F.fprintf fmt "%a.length%a" (pp_star ~paren:false) p pp_is_void is_void
    | Length {p; is_void} ->
        F.fprintf fmt "%a.length%a" pp_partial p pp_is_void is_void


  let pp_mark ~markup = if markup then MarkupFormatter.wrap_monospaced pp else pp

  let rec represents_multiple_values = function
    (* TODO depending on the result, the call might represent multiple values *)
    | BoField.Prim (Callsite _ | Pvar _) ->
        false
    | BoField.Prim (Deref (Deref_ArrayIndex, _)) | BoField.StarField _ ->
        true
    | BoField.Prim (Deref (Deref_CPointer, p))
    (* Deref_CPointer is unsound here but avoids many FPs for non-array pointers *)
    | BoField.Prim (Deref ((Deref_COneValuePointer | Deref_JavaPointer), p))
    | BoField.Field {prefix= p} ->
        represents_multiple_values p


  let rec represents_multiple_values_sound = function
    | BoField.Prim (Callsite _) | BoField.StarField _ ->
        true
    | BoField.Prim (Pvar _) ->
        false
    | BoField.Prim (Deref ((Deref_ArrayIndex | Deref_CPointer), _)) ->
        true
    | BoField.(Prim (Deref ((Deref_COneValuePointer | Deref_JavaPointer), p)) | Field {prefix= p})
      ->
        represents_multiple_values_sound p


  let rec represents_callsite_sound_partial = function
    | BoField.Prim (Callsite _) ->
        true
    | BoField.Prim (Pvar _) ->
        false
    | BoField.(Prim (Deref (_, p)) | Field {prefix= p} | StarField {prefix= p}) ->
        represents_callsite_sound_partial p


  let rec exists_pvar_partial ~f = function
    | BoField.Prim (Pvar pvar) ->
        f pvar
    | BoField.(Prim (Deref (_, p)) | Field {prefix= p} | StarField {prefix= p}) ->
        exists_pvar_partial ~f p
    | BoField.Prim (Callsite _) ->
        false


  let rec exists_str_partial ~f = function
    | BoField.Prim (Pvar pvar) ->
        f (Pvar.to_string pvar)
    | BoField.Prim (Deref (_, x)) ->
        exists_str_partial ~f x
    | BoField.(Field {fn= fld; prefix= x} | StarField {last_field= fld; prefix= x}) ->
        f (Fieldname.to_string fld) || exists_str_partial ~f x
    | BoField.Prim (Callsite _) ->
        false


  let exists_str ~f = function
    | Modeled p | Normal p | Offset {p} | Length {p} ->
        exists_str_partial ~f p


  let is_void_ptr_path = function
    | Offset {is_void} | Length {is_void} ->
        is_void
    | Normal _ | Modeled _ ->
        false


  let is_cpp_vector_elem = function
    | BoField.Field {fn} ->
        BufferOverrunField.is_cpp_vector_elem fn
    | _ ->
        false


  let rec is_global_partial = function
    | BoField.Prim (Pvar pvar) ->
        Pvar.is_global pvar
    | BoField.(Prim (Deref (_, x)) | Field {prefix= x} | StarField {prefix= x}) ->
        is_global_partial x
    | BoField.Prim (Callsite _) ->
        false


  let is_length = function Length _ -> true | _ -> false

  let is_global = function Normal p | Offset {p} | Length {p} | Modeled p -> is_global_partial p
end

module Symbol = struct
  (* NOTE: non_int represents the symbols that are not integer type,
     so that their ranges are not used in the cost checker. *)
  type t =
    | OneValue of {unsigned: bool [@ignore]; non_int: bool [@ignore]; path: SymbolPath.t}
    | BoundEnd of
        { unsigned: bool [@ignore]
        ; non_int: bool [@ignore]
        ; path: SymbolPath.t
        ; bound_end: BoundEnd.t }
  [@@deriving compare, equal]

  let pp : F.formatter -> t -> unit =
   fun fmt s ->
    match s with
    | OneValue {unsigned; non_int; path} | BoundEnd {unsigned; non_int; path} ->
        SymbolPath.pp fmt path ;
        ( if Config.developer_mode then
            match s with
            | BoundEnd {bound_end} ->
                Format.fprintf fmt ".%s" (BoundEnd.to_string bound_end)
            | OneValue _ ->
                () ) ;
        if Config.bo_debug > 1 then
          F.fprintf fmt "(%c%s)" (if unsigned then 'u' else 's') (if non_int then "n" else "")


  let compare s1 s2 =
    match (s1, s2) with
    | OneValue _, BoundEnd _ ->
        -1
    | BoundEnd _, OneValue _ ->
        1
    | OneValue {unsigned= unsigned1}, OneValue {unsigned= unsigned2}
    | BoundEnd {unsigned= unsigned1}, BoundEnd {unsigned= unsigned2} ->
        let r = compare s1 s2 in
        if Int.equal r 0 && not (Bool.equal unsigned1 unsigned2) then (
          L.d_printfln_escaped "values are equal but their signs are different: %a <> %a" pp s1 pp
            s2 ;
          Bool.compare unsigned1 unsigned2 )
        else r


  type 'res eval = t -> BoundEnd.t -> 'res AbstractDomain.Types.bottom_lifted

  let paths_equal s1 s2 =
    match (s1, s2) with
    | OneValue _, BoundEnd _ | BoundEnd _, OneValue _ ->
        false
    | OneValue {path= path1}, OneValue {path= path2} | BoundEnd {path= path1}, BoundEnd {path= path2}
      ->
        SymbolPath.equal path1 path2


  type make_t = unsigned:bool -> ?non_int:bool -> SymbolPath.t -> t

  let make_onevalue : make_t =
   fun ~unsigned ?(non_int = false) path -> OneValue {unsigned; non_int; path}


  let make_boundend : BoundEnd.t -> make_t =
   fun bound_end ~unsigned ?(non_int = false) path -> BoundEnd {unsigned; non_int; path; bound_end}


  let pp_mark ~markup = if markup then MarkupFormatter.wrap_monospaced pp else pp

  let is_unsigned : t -> bool = function OneValue {unsigned} | BoundEnd {unsigned} -> unsigned

  let is_non_int : t -> bool = function OneValue {non_int} | BoundEnd {non_int} -> non_int

  let is_global : t -> bool = function
    | OneValue {path} | BoundEnd {path} ->
        SymbolPath.is_global path


  (* This should be called on non-pulse bound as of now. *)
  let path = function OneValue {path} | BoundEnd {path} -> path

  let is_length = function OneValue {path} | BoundEnd {path} -> SymbolPath.is_length path

  (* NOTE: This may not be satisfied in the cost checker for simplifying its results. *)
  let check_bound_end s be =
    if Config.bo_debug >= 3 then
      match s with
      | OneValue _ ->
          ()
      | BoundEnd {bound_end} ->
          if not (BoundEnd.equal be bound_end) then
            L.d_printfln_escaped
              "Mismatch of symbol's boundend and its position: %a is in a place for %s." pp s
              (BoundEnd.to_string be)


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
