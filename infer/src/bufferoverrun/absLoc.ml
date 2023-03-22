(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module BoField = BufferOverrunField

module Allocsite = struct
  type t =
    | Unknown
    | Symbol of Symb.SymbolPath.partial
    | Known of
        { proc_name: string
        ; caller_pname: Procname.t option
        ; node_hash: int
        ; inst_num: int
        ; dimension: int
        ; represents_multiple_values: bool
        ; path: Symb.SymbolPath.partial option }
    | LiteralString of string
  [@@deriving compare, equal]

  let eq as1 as2 =
    match (as1, as2) with
    | Unknown, _ | _, Unknown ->
        Boolean.Top
    | Symbol _, Symbol _ ->
        (* parameters may alias *) Boolean.Top
    | Known {path= Some p1}, Known {path= Some p2} ->
        Boolean.of_bool (Symb.SymbolPath.equal_partial p1 p2)
    | Known {path= Some _}, Known {path= None} | Known {path= None}, Known {path= Some _} ->
        Boolean.False
    | Known {path= None}, Known {path= None} ->
        Boolean.of_bool (equal as1 as2)
    | LiteralString s1, LiteralString s2 ->
        Boolean.of_bool (String.equal s1 s2)
    | _, _ ->
        Boolean.False


  let pp_paren ~paren fmt = function
    | Unknown ->
        F.fprintf fmt "Unknown"
    | Symbol path ->
        Symb.SymbolPath.pp_partial_paren ~paren fmt path
    | Known {path= Some path} when Config.bo_debug < 1 ->
        Symb.SymbolPath.pp_partial_paren ~paren fmt path
    | Known {proc_name; caller_pname; node_hash; inst_num; dimension; path} ->
        let pp_opt_pname fmt optpname =
          Option.iter optpname ~f:(fun pname -> Procname.pp fmt pname)
        in
        F.fprintf fmt "%s-%a-%d-%d-%d" proc_name pp_opt_pname caller_pname node_hash inst_num
          dimension ;
        Option.iter path ~f:(fun path ->
            F.fprintf fmt "(%a)" (Symb.SymbolPath.pp_partial_paren ~paren:false) path )
    | LiteralString s ->
        F.fprintf fmt "%S" s


  let pp = pp_paren ~paren:false

  let is_pretty = function Symbol _ | Known {path= Some _} -> true | _ -> false

  let get_literal_string = function LiteralString s -> Some s | _ -> None

  let is_unknown = function Unknown -> true | Symbol _ | Known _ | LiteralString _ -> false

  let make :
         Procname.t
      -> caller_pname:Procname.t option
      -> node_hash:int
      -> inst_num:int
      -> dimension:int
      -> path:Symb.SymbolPath.partial option
      -> represents_multiple_values:bool
      -> t =
   fun proc_name ~caller_pname ~node_hash ~inst_num ~dimension ~path ~represents_multiple_values ->
    Known
      { proc_name= Procname.to_string proc_name
      ; caller_pname
      ; node_hash
      ; inst_num
      ; dimension
      ; path
      ; represents_multiple_values }


  let make_symbol path = Symbol path

  let unknown = Unknown

  let literal_string s = LiteralString s

  let get_path = function
    | Unknown | LiteralString _ ->
        None
    | Symbol path ->
        Some path
    | Known {path} ->
        path


  let get_param_path = function
    | Symbol path ->
        Option.some_if (not (Symb.SymbolPath.represents_callsite_sound_partial path)) path
    | Unknown | Known _ | LiteralString _ ->
        None


  let represents_multiple_values = function
    | Unknown ->
        false
    | Symbol path ->
        Symb.SymbolPath.represents_multiple_values path
    | Known {path; represents_multiple_values} ->
        represents_multiple_values
        || Option.exists path ~f:Symb.SymbolPath.represents_multiple_values
    | LiteralString _ ->
        true


  let exists_pvar ~f = function
    | Unknown | LiteralString _ | Known {path= None} ->
        false
    | Symbol path | Known {path= Some path} ->
        Symb.SymbolPath.exists_pvar_partial ~f path
end

module Loc = struct
  type prim = Var of Var.t | Allocsite of Allocsite.t [@@deriving compare, equal]

  type t = prim BoField.t [@@deriving compare, equal]

  let of_var v = BoField.Prim (Var v)

  let of_allocsite a = BoField.Prim (Allocsite a)

  let prim_append_field ?typ l0 fn _aux _depth = function
    | Allocsite a as l when Allocsite.is_unknown a ->
        BoField.Prim l
    | Var _ | Allocsite _ ->
        BoField.Field {prefix= l0; fn; typ}


  let prim_append_star_field l0 fn _aux = function
    | Allocsite a as l when Allocsite.is_unknown a ->
        BoField.Prim l
    | Var _ | Allocsite _ ->
        BoField.StarField {prefix= l0; last_field= fn}


  let append_field = BoField.mk_append_field ~prim_append_field ~prim_append_star_field

  let append_star_field = BoField.mk_append_star_field ~prim_append_star_field

  let eq l1 l2 =
    match (l1, l2) with
    | BoField.Prim (Allocsite as1), BoField.Prim (Allocsite as2) ->
        Allocsite.eq as1 as2
    | _ ->
        Boolean.Top


  let unknown = of_allocsite Allocsite.unknown

  let rec is_unknown = function
    | BoField.Prim (Var _) ->
        false
    | BoField.Prim (Allocsite a) ->
        Allocsite.is_unknown a
    | BoField.(Field {prefix= x} | StarField {prefix= x}) ->
        is_unknown x


  let rec pp_paren ~paren fmt =
    let module SP = Symb.SymbolPath in
    function
    | BoField.Prim (Var v) ->
        Var.pp F.str_formatter v ;
        let s = F.flush_str_formatter () in
        if Char.equal s.[0] '&' then
          F.pp_print_string fmt (String.sub s ~pos:1 ~len:(String.length s - 1))
        else F.pp_print_string fmt s
    | BoField.Prim (Allocsite a) ->
        Allocsite.pp_paren ~paren fmt a
    | BoField.Field
        { prefix=
            Prim
              (Allocsite
                (Allocsite.Symbol
                  (BoField.Prim (SP.Deref ((SP.Deref_COneValuePointer | SP.Deref_CPointer), p))) )
                )
        ; fn= f }
    | BoField.Field
        { prefix=
            Prim
              (Allocsite
                (Allocsite.Known
                  { path=
                      Some
                        (BoField.Prim
                          (SP.Deref ((SP.Deref_COneValuePointer | SP.Deref_CPointer), p)) ) } ) )
        ; fn= f } ->
        BoField.pp ~pp_lhs:(SP.pp_partial_paren ~paren:true) ~sep:"->" fmt p f
    | BoField.Field {prefix= l; fn= f} ->
        BoField.pp ~pp_lhs:(pp_paren ~paren:true) ~sep:"." fmt l f
    | BoField.StarField {prefix; last_field} ->
        BoField.pp ~pp_lhs:(pp_star ~paren:true) ~sep:"." fmt prefix last_field


  and pp_star ~paren fmt l =
    pp_paren ~paren fmt l ;
    F.pp_print_string fmt ".*"


  let pp = pp_paren ~paren:false

  let is_c_strlen = function
    | BoField.Field {fn} ->
        Fieldname.equal fn (BoField.c_strlen ())
    | _ ->
        false


  let is_java_collection_internal_array = function
    | BoField.Field {fn} ->
        Fieldname.equal fn BoField.java_collection_internal_array
    | _ ->
        false


  let is_objc_collection_internal_array = function
    | BoField.Field {fn} ->
        Fieldname.equal fn BoField.objc_collection_internal_array
    | _ ->
        false


  let is_objc_iterator_offset = function
    | BoField.Field {fn} ->
        Fieldname.equal fn BoField.objc_iterator_offset
    | _ ->
        false


  let is_frontend_tmp = function
    | BoField.Prim (Var x) ->
        not (Var.appears_in_source_code x)
    | _ ->
        false


  let rec is_pretty (field : _ BoField.t) =
    match field with
    | Prim (Var _) ->
        true
    | Prim (Allocsite a) ->
        Allocsite.is_pretty a
    | Field {prefix= loc} | StarField {prefix= loc} ->
        is_pretty loc


  let of_c_strlen loc = append_field loc (BoField.c_strlen ())

  let of_pvar pvar = of_var (Var.of_pvar pvar)

  let of_id id = of_var (Var.of_id id)

  let rec of_path path =
    match path with
    | BoField.Prim (Symb.SymbolPath.Pvar pvar) ->
        of_pvar pvar
    | BoField.Prim (Symb.SymbolPath.Deref _ | Symb.SymbolPath.Callsite _) ->
        of_allocsite (Allocsite.make_symbol path)
    | BoField.Field {fn; prefix= path} ->
        append_field (of_path path) fn
    | BoField.StarField {last_field= fn; prefix} ->
        append_star_field (of_path prefix) fn


  let is_return = function
    | BoField.Prim (Var (Var.ProgramVar x)) ->
        Mangled.equal (Pvar.get_name x) Ident.name_return
    | _ ->
        false


  let rec is_trans_field_of ~loc ~field_loc =
    match field_loc with
    | BoField.(Field {prefix= l} | StarField {prefix= l}) ->
        if equal loc l then true else is_trans_field_of ~loc ~field_loc:l
    | _ ->
        false


  let get_parent_field field_loc =
    match field_loc with BoField.(Field {prefix= l} | StarField {prefix= l}) -> l | _ -> field_loc


  let get_literal_string = function
    | BoField.Prim (Allocsite a) ->
        Allocsite.get_literal_string a
    | _ ->
        None


  let get_literal_string_strlen = function
    | BoField.Field {prefix= l; fn} when Fieldname.equal (BoField.c_strlen ()) fn ->
        get_literal_string l
    | _ ->
        None


  let rec is_global = function
    | BoField.Prim (Var (Var.ProgramVar pvar)) ->
        Pvar.is_global pvar
    | BoField.Prim (Var (Var.LogicalVar _) | Allocsite _) ->
        false
    | BoField.(Field {prefix= loc} | StarField {prefix= loc}) ->
        is_global loc


  let rec get_global_array_initializer =
    let initializer_of_pvar pvar =
      if Pvar.is_constant_array pvar then Pvar.get_initializer_pname pvar else None
    in
    function
    | BoField.Prim (Var (Var.ProgramVar pvar)) ->
        initializer_of_pvar pvar
    | BoField.Prim (Var (Var.LogicalVar _)) ->
        None
    | BoField.Prim (Allocsite allocsite) ->
        Allocsite.get_path allocsite
        |> Option.bind ~f:Symb.SymbolPath.get_pvar
        |> Option.bind ~f:initializer_of_pvar
    | BoField.(Field {prefix= loc} | StarField {prefix= loc}) ->
        get_global_array_initializer loc


  let rec get_path = function
    | BoField.Prim (Var (LogicalVar _)) ->
        None
    | BoField.Prim (Var (ProgramVar pvar)) ->
        Some (Symb.SymbolPath.of_pvar pvar)
    | BoField.Prim (Allocsite allocsite) ->
        Allocsite.get_path allocsite
    | BoField.Field {prefix= l; fn; typ} ->
        Option.map (get_path l) ~f:(fun p -> Symb.SymbolPath.append_field ?typ p fn)
    | BoField.StarField {prefix; last_field} ->
        get_path prefix |> Option.map ~f:(fun p -> Symb.SymbolPath.append_star_field p last_field)


  let rec get_param_path = function
    | BoField.Prim (Var _) ->
        None
    | BoField.Prim (Allocsite allocsite) ->
        Allocsite.get_param_path allocsite
    | BoField.Field {prefix= l; fn} ->
        Option.map (get_param_path l) ~f:(fun p -> Symb.SymbolPath.append_field p fn)
    | BoField.StarField {prefix; last_field} ->
        get_param_path prefix
        |> Option.map ~f:(fun p -> Symb.SymbolPath.append_star_field p last_field)


  let rec represents_multiple_values = function
    | BoField.Prim (Var _) ->
        false
    | BoField.Prim (Allocsite allocsite) ->
        Allocsite.represents_multiple_values allocsite
    | BoField.Field _ as x
      when is_c_strlen x
           || is_java_collection_internal_array x
           || is_objc_iterator_offset x
           || is_objc_collection_internal_array x ->
        false
    | BoField.Field {prefix= l} ->
        represents_multiple_values l
    | BoField.StarField _ ->
        true


  let rec exists_pvar ~f = function
    | BoField.Prim (Var (LogicalVar _)) ->
        false
    | BoField.Prim (Var (ProgramVar pvar)) ->
        f pvar
    | BoField.Prim (Allocsite allocsite) ->
        Allocsite.exists_pvar ~f allocsite
    | BoField.(Field {prefix= l} | StarField {prefix= l}) ->
        exists_pvar ~f l


  let exists_str ~f l =
    Option.exists (get_path l) ~f:(fun path -> Symb.SymbolPath.exists_str_partial ~f path)


  let cast typ x =
    match x with
    | BoField.Field {prefix= l; fn} ->
        append_field l fn ~typ
    | BoField.(StarField _ | Prim (Var _ | Allocsite _)) ->
        x


  let get_linked_list_next ~lhs ~rhs =
    match (get_path lhs, get_path rhs) with
    | ( Some lhs_path
      , Some
          ( Field {prefix= rhs_path}
          | Prim
              (Deref (Deref_JavaPointer, Field {prefix= Prim (Deref (Deref_JavaPointer, rhs_path))}))
            ) )
      when Symb.SymbolPath.equal_partial lhs_path rhs_path ->
        Some lhs
    | _, _ ->
        None
end

module LocSet = PrettyPrintable.MakePPSet (Loc)

module PowLoc = struct
  (* The known set of locations should not be empty and not include the unknown location.
     Every constructors in this module should be defined carefully to keep that constraint.
     Unknown set of locations always includes unknown location.

     The other locations than unknown location in the unknown set mean that there is an
     evidence that these locations can be in the set.
     This is useful to not completely throw away the set of known possible locations when
     performing a weak update of a set of locations.
     That is:
        (Known L1) weakly updated with (Unknown L2) -> Unknown (L1 union L2)
        (Unknown L1) weakly updated with (Unknown L2) -> Unknown (L1 union L2)
     This makes it possible to deploy the following heuristics when updating a dereference
     of an unknown pointer: update all tracked unknown targets of the pointer and keep the
     other possible targets of the pointer unchanged. *)
  type t = Bottom | Unknown of LocSet.t | Known of LocSet.t [@@deriving compare]

  let mk_known ploc =
    assert ((not (LocSet.is_empty ploc)) && not (LocSet.exists (fun l -> Loc.is_unknown l) ploc)) ;
    Known ploc


  let mk_unknown ploc = Unknown ploc

  let pp f = function
    | Bottom ->
        F.pp_print_string f SpecialChars.up_tack
    | Known locs | Unknown locs ->
        LocSet.pp f locs


  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | Bottom, _ ->
        true
    | _, Bottom ->
        false
    | Unknown lhs, Unknown rhs | Known lhs, Known rhs ->
        LocSet.subset lhs rhs
    | Unknown _, _ ->
        true
    | _, Unknown _ ->
        false


  let join x y =
    match (x, y) with
    | Bottom, _ ->
        y
    | _, Bottom ->
        x
    | Unknown x, Unknown y ->
        mk_unknown (LocSet.union x y)
    | (Known x, Unknown y | Unknown x, Known y) when Config.bo_sound_unknown_sets_join ->
        mk_unknown (LocSet.union x y)
    | Known x, Unknown _ | Unknown _, Known x ->
        mk_known x
    | Known x, Known y ->
        mk_known (LocSet.union x y)


  let widen ~prev ~next ~num_iters:_ = join prev next

  let bot = Bottom

  let is_bot = function Bottom -> true | Unknown _ | Known _ -> false

  let is_unknown = function Unknown _ -> true | Bottom | Known _ -> false

  let unknown = mk_unknown (LocSet.singleton Loc.unknown)

  let singleton l =
    if Loc.is_unknown l then mk_unknown (LocSet.singleton l) else mk_known (LocSet.singleton l)


  let fold f ploc init =
    match ploc with Bottom -> init | Known ploc | Unknown ploc -> LocSet.fold f ploc init


  let exists f ploc =
    match ploc with Bottom -> false | Known ploc | Unknown ploc -> LocSet.exists f ploc


  let normalize ploc =
    if LocSet.is_empty ploc then Bottom
    else if LocSet.exists (fun l -> Loc.is_unknown l) ploc then mk_unknown ploc
    else mk_known ploc


  let map f ploc =
    match ploc with Bottom -> Bottom | Known ploc | Unknown ploc -> normalize (LocSet.map f ploc)


  let is_singleton_or_more = function
    | Bottom ->
        IContainer.Empty
    | Unknown ploc | Known ploc ->
        LocSet.is_singleton_or_more ploc


  let min_elt_opt = function Bottom -> None | Unknown ploc | Known ploc -> LocSet.min_elt_opt ploc

  let add l ploc =
    match ploc with
    | Bottom ->
        singleton l
    | (Known _ | Unknown _) when Loc.is_unknown l ->
        ploc
    | Known ploc ->
        mk_known (LocSet.add l ploc)
    | Unknown ploc ->
        mk_known (LocSet.add l (LocSet.filter (fun l -> not (Loc.is_unknown l)) ploc))


  let of_list locs = List.fold locs ~init:bot ~f:(fun acc loc -> add loc acc)

  let mem l = function Bottom -> false | Unknown ploc | Known ploc -> LocSet.mem l ploc

  let get_parent_field ploc =
    match ploc with
    | Bottom ->
        (* Return the unknown location to avoid unintended unreachable nodes *)
        mk_unknown (LocSet.singleton Loc.unknown)
    | Unknown ploc ->
        mk_unknown (LocSet.fold (fun l -> LocSet.add (Loc.get_parent_field l)) ploc LocSet.empty)
    | Known ploc ->
        mk_known (LocSet.fold (fun l -> LocSet.add (Loc.get_parent_field l)) ploc LocSet.empty)


  let append_field ?typ ploc ~fn =
    match ploc with
    | Bottom ->
        (* Return the unknown location to avoid unintended unreachable nodes *)
        mk_unknown (LocSet.singleton Loc.unknown)
    | Unknown ploc ->
        mk_unknown
          (LocSet.fold (fun l -> LocSet.add (Loc.append_field ?typ l fn)) ploc LocSet.empty)
    | Known ploc ->
        mk_known (LocSet.fold (fun l -> LocSet.add (Loc.append_field ?typ l fn)) ploc LocSet.empty)


  let append_star_field ploc ~fn =
    match ploc with
    | Bottom ->
        (* Return the unknown location to avoid unintended unreachable nodes *)
        mk_unknown (LocSet.singleton Loc.unknown)
    | Unknown ploc ->
        mk_unknown
          (LocSet.fold (fun l -> LocSet.add (Loc.append_star_field l fn)) ploc LocSet.empty)
    | Known ploc ->
        mk_known (LocSet.fold (fun l -> LocSet.add (Loc.append_star_field l fn)) ploc LocSet.empty)


  let lift_cmp cmp_loc ploc1 ploc2 =
    match (ploc1, ploc2) with
    | Known ploc1, Known ploc2 -> (
      match (LocSet.is_singleton_or_more ploc1, LocSet.is_singleton_or_more ploc2) with
      | IContainer.Singleton loc1, IContainer.Singleton loc2 ->
          Boolean.EqualOrder.of_equal cmp_loc (Loc.eq loc1 loc2)
      | _ ->
          Boolean.Top )
    | _, _ ->
        Boolean.Top


  type eval_locpath = Symb.SymbolPath.partial -> t

  let subst_loc l (eval_locpath : eval_locpath) =
    match Loc.get_param_path l with
    | None ->
        singleton l
    | Some path when Language.curr_language_is Java && Symb.SymbolPath.is_global_partial path ->
        singleton l
    | Some path ->
        eval_locpath path


  let subst x (eval_locpath : eval_locpath) =
    fold (fun l acc -> join acc (subst_loc l eval_locpath)) x bot


  let exists_str ~f x = exists (fun l -> Loc.exists_str ~f l) x

  let of_c_strlen x = map Loc.of_c_strlen x

  let cast typ x = map (Loc.cast typ) x

  let to_set = function Bottom -> LocSet.empty | Unknown ploc | Known ploc -> ploc

  let get_linked_list_next ~lhs ~rhs =
    match (is_singleton_or_more lhs, is_singleton_or_more rhs) with
    | Singleton lhs, Singleton rhs ->
        Loc.get_linked_list_next ~lhs ~rhs
    | _, _ ->
        None


  let is_single_known_loc = function
    | Bottom | Unknown _ ->
        false
    | Known ploc ->
        Int.equal (LocSet.cardinal ploc) 1
end

let always_strong_update = false

let can_strong_update : PowLoc.t -> bool =
 fun ploc ->
  if always_strong_update then true
  else
    match PowLoc.is_singleton_or_more ploc with
    | IContainer.Singleton loc ->
        (not (Loc.represents_multiple_values loc)) || Loc.is_c_strlen loc
    | _ ->
        false
