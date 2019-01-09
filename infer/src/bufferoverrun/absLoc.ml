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
    | Symbol of Symb.SymbolPath.partial
    | Known of
        { proc_name: string
        ; node_hash: int
        ; inst_num: int
        ; dimension: int
        ; represents_multiple_values: bool
        ; path: Symb.SymbolPath.partial option }
  [@@deriving compare]

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
        Boolean.of_bool ([%compare.equal: t] as1 as2)
    | Known _, Symbol _ | Symbol _, Known _ ->
        Boolean.False


  let pp_paren ~paren fmt = function
    | Unknown ->
        F.fprintf fmt "Unknown"
    | Symbol path ->
        Symb.SymbolPath.pp_partial_paren ~paren fmt path
    | Known {path= Some path} when Config.bo_debug < 1 ->
        Symb.SymbolPath.pp_partial_paren ~paren fmt path
    | Known {proc_name; node_hash; inst_num; dimension; path} ->
        F.fprintf fmt "%s-%d-%d-%d" proc_name node_hash inst_num dimension ;
        Option.iter path ~f:(fun path ->
            F.fprintf fmt "(%a)" (Symb.SymbolPath.pp_partial_paren ~paren:false) path )


  let pp = pp_paren ~paren:false

  let is_pretty = function Symbol _ | Known {path= Some _} -> true | _ -> false

  let to_string x = F.asprintf "%a" pp x

  let make :
         Typ.Procname.t
      -> node_hash:int
      -> inst_num:int
      -> dimension:int
      -> path:Symb.SymbolPath.partial option
      -> represents_multiple_values:bool
      -> t =
   fun proc_name ~node_hash ~inst_num ~dimension ~path ~represents_multiple_values ->
    Known
      { proc_name= Typ.Procname.to_string proc_name
      ; node_hash
      ; inst_num
      ; dimension
      ; path
      ; represents_multiple_values }


  let make_symbol path = Symbol path

  let unknown = Unknown

  let get_path = function Unknown -> None | Symbol path -> Some path | Known {path} -> path

  let get_param_path = function
    | Symbol path ->
        Option.some_if (not (Symb.SymbolPath.represents_callsite_sound_partial path)) path
    | Unknown | Known _ ->
        None


  let represents_multiple_values = function
    | Unknown ->
        false
    | Symbol path ->
        Symb.SymbolPath.represents_multiple_values path
    | Known {path; represents_multiple_values} ->
        represents_multiple_values
        || Option.value_map path ~default:false ~f:Symb.SymbolPath.represents_multiple_values
end

module Loc = struct
  type t = Var of Var.t | Allocsite of Allocsite.t | Field of t * Typ.Fieldname.t
  [@@deriving compare]

  let equal = [%compare.equal: t]

  let eq l1 l2 =
    match (l1, l2) with Allocsite as1, Allocsite as2 -> Allocsite.eq as1 as2 | _ -> Boolean.Top


  let unknown = Allocsite Allocsite.unknown

  let rec pp_paren ~paren fmt =
    let module SP = Symb.SymbolPath in
    function
    | Var v ->
        Var.pp F.str_formatter v ;
        let s = F.flush_str_formatter () in
        if Char.equal s.[0] '&' then
          F.pp_print_string fmt (String.sub s ~pos:1 ~len:(String.length s - 1))
        else F.pp_print_string fmt s
    | Allocsite a ->
        Allocsite.pp_paren ~paren fmt a
    | Field
        ( Allocsite
            (Allocsite.Symbol (SP.Deref ((SP.Deref_COneValuePointer | SP.Deref_CPointer), p)))
        , f )
    | Field
        ( Allocsite
            (Allocsite.Known
              {path= Some (SP.Deref ((SP.Deref_COneValuePointer | SP.Deref_CPointer), p))})
        , f ) ->
        BufferOverrunField.pp ~pp_lhs:(SP.pp_partial_paren ~paren:true)
          ~pp_lhs_alone:(SP.pp_pointer ~paren) ~sep:"->" fmt p f
    | Field (l, f) ->
        BufferOverrunField.pp ~pp_lhs:(pp_paren ~paren:true) ~pp_lhs_alone:(pp_paren ~paren)
          ~sep:"." fmt l f


  let pp = pp_paren ~paren:false

  let to_string x = F.asprintf "%a" pp x

  let is_var = function Var _ -> true | _ -> false

  let rec is_pretty = function
    | Var _ ->
        true
    | Allocsite a ->
        Allocsite.is_pretty a
    | Field (loc, _) ->
        is_pretty loc


  let of_var v = Var v

  let of_allocsite a = Allocsite a

  let of_pvar pvar = Var (Var.of_pvar pvar)

  let of_id id = Var (Var.of_id id)

  let rec of_path path =
    match path with
    | Symb.SymbolPath.Pvar pvar ->
        of_pvar pvar
    | Symb.SymbolPath.Deref _ | Symb.SymbolPath.Callsite _ ->
        Allocsite (Allocsite.make_symbol path)
    | Symb.SymbolPath.Field (fn, path) ->
        Field (of_path path, fn)


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


  let rec get_param_path = function
    | Var _ ->
        None
    | Allocsite allocsite ->
        Allocsite.get_param_path allocsite
    | Field (l, fn) ->
        Option.map (get_param_path l) ~f:(fun p -> Symb.SymbolPath.field p fn)


  let has_param_path formals x =
    Option.value_map (get_path x) ~default:false ~f:(fun x ->
        Option.value_map (Symb.SymbolPath.get_pvar x) ~default:false ~f:(fun x ->
            List.exists formals ~f:(fun (formal, _) -> Pvar.equal x formal) ) )


  let rec represents_multiple_values = function
    | Var _ ->
        false
    | Allocsite allocsite ->
        Allocsite.represents_multiple_values allocsite
    | Field (l, _) ->
        represents_multiple_values l


  let exists_str ~f l =
    Option.exists (get_path l) ~f:(fun path -> Symb.SymbolPath.exists_str_partial ~f path)
end

module PowLoc = struct
  include AbstractDomain.FiniteSet (Loc)

  let bot = empty

  let is_bot = is_empty

  let unknown = singleton Loc.unknown

  let append_field ploc ~fn =
    if is_bot ploc then singleton Loc.unknown
    else fold (fun l -> add (Loc.append_field l ~fn)) ploc empty


  let lift_cmp cmp_loc ploc1 ploc2 =
    match (is_singleton_or_more ploc1, is_singleton_or_more ploc2) with
    | IContainer.Singleton loc1, IContainer.Singleton loc2 ->
        Boolean.EqualOrder.of_equal cmp_loc (Loc.eq loc1 loc2)
    | _ ->
        Boolean.Top


  type eval_locpath = Symb.SymbolPath.partial -> t

  let subst_loc l (eval_locpath : eval_locpath) =
    match Loc.get_param_path l with None -> singleton l | Some path -> eval_locpath path


  let subst x (eval_locpath : eval_locpath) =
    fold (fun l acc -> join acc (subst_loc l eval_locpath)) x empty


  let exists_str ~f x = exists (fun l -> Loc.exists_str ~f l) x
end

let always_strong_update = false

let can_strong_update : PowLoc.t -> bool =
 fun ploc ->
  if always_strong_update then true
  else
    match PowLoc.is_singleton_or_more ploc with
    | IContainer.Singleton loc ->
        Loc.is_var loc
    | _ ->
        false
