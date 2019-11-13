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
    | LiteralString of string
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
    | Known {proc_name; node_hash; inst_num; dimension; path} ->
        F.fprintf fmt "%s-%d-%d-%d" proc_name node_hash inst_num dimension ;
        Option.iter path ~f:(fun path ->
            F.fprintf fmt "(%a)" (Symb.SymbolPath.pp_partial_paren ~paren:false) path )
    | LiteralString s ->
        F.fprintf fmt "%S" s


  let pp = pp_paren ~paren:false

  let is_pretty = function Symbol _ | Known {path= Some _} -> true | _ -> false

  let is_literal_string = function LiteralString s -> Some s | _ -> None

  let is_unknown = function Unknown -> true | Symbol _ | Known _ | LiteralString _ -> false

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
        || Option.value_map path ~default:false ~f:Symb.SymbolPath.represents_multiple_values
    | LiteralString _ ->
        true


  let exists_pvar ~f = function
    | Unknown | LiteralString _ | Known {path= None} ->
        false
    | Symbol path | Known {path= Some path} ->
        Symb.SymbolPath.exists_pvar_partial ~f path
end

module Loc = struct
  type field_typ = Typ.t option

  let compare_field_typ _ _ = 0

  include (* Enforce invariants on Field and StarField, see Symb.mli *) (
    struct
      type t =
        | Var of Var.t
        | Allocsite of Allocsite.t
        | Field of {prefix: t; fn: Typ.Fieldname.t; typ: field_typ}
        | StarField of {prefix: t; last_field: Typ.Fieldname.t}
      [@@deriving compare]

      let of_var v = Var v

      let of_allocsite a = Allocsite a

      let append_field ?typ l0 ~fn =
        let rec aux = function
          | Var _ | Allocsite _ ->
              Field {prefix= l0; fn; typ}
          | StarField {last_field} as l when Typ.Fieldname.equal fn last_field ->
              l
          | StarField {prefix} ->
              StarField {prefix; last_field= fn}
          | Field {fn= fn'} when Typ.Fieldname.equal fn fn' ->
              StarField {prefix= l0; last_field= fn}
          | Field {prefix= l} ->
              aux l
        in
        aux l0


      let append_star_field l0 ~fn =
        let rec aux = function
          | Var _ | Allocsite _ ->
              StarField {prefix= l0; last_field= fn}
          | StarField {last_field} as l when Typ.Fieldname.equal fn last_field ->
              l
          | StarField {prefix} ->
              StarField {prefix; last_field= fn}
          | Field {prefix= l} ->
              aux l
        in
        aux l0
    end :
      sig
        type t = private
          | Var of Var.t
          | Allocsite of Allocsite.t
          | Field of {prefix: t; fn: Typ.Fieldname.t; typ: field_typ}
          | StarField of {prefix: t; last_field: Typ.Fieldname.t}
        [@@deriving compare]

        val of_var : Var.t -> t

        val of_allocsite : Allocsite.t -> t

        val append_field : ?typ:Typ.t -> t -> fn:Typ.Fieldname.t -> t

        val append_star_field : t -> fn:Typ.Fieldname.t -> t
      end )

  let equal = [%compare.equal: t]

  let eq l1 l2 =
    match (l1, l2) with Allocsite as1, Allocsite as2 -> Allocsite.eq as1 as2 | _ -> Boolean.Top


  let unknown = of_allocsite Allocsite.unknown

  let rec is_unknown = function
    | Var _ ->
        false
    | Allocsite a ->
        Allocsite.is_unknown a
    | Field {prefix= x} | StarField {prefix= x} ->
        is_unknown x


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
        { prefix=
            Allocsite
              (Allocsite.Symbol (SP.Deref ((SP.Deref_COneValuePointer | SP.Deref_CPointer), p)))
        ; fn= f }
    | Field
        { prefix=
            Allocsite
              (Allocsite.Known
                {path= Some (SP.Deref ((SP.Deref_COneValuePointer | SP.Deref_CPointer), p))})
        ; fn= f } ->
        BufferOverrunField.pp ~pp_lhs:(SP.pp_partial_paren ~paren:true)
          ~pp_lhs_alone:(SP.pp_pointer ~paren) ~sep:"->" fmt p f
    | Field {prefix= l; fn= f} ->
        BufferOverrunField.pp ~pp_lhs:(pp_paren ~paren:true) ~pp_lhs_alone:(pp_paren ~paren)
          ~sep:"." fmt l f
    | StarField {prefix; last_field} ->
        BufferOverrunField.pp ~pp_lhs:(pp_star ~paren:true) ~pp_lhs_alone:(pp_star ~paren) ~sep:"."
          fmt prefix last_field


  and pp_star ~paren fmt l = pp_paren ~paren fmt l ; F.pp_print_string fmt ".*"

  let pp = pp_paren ~paren:false

  let to_string x = F.asprintf "%a" pp x

  let is_var = function Var _ -> true | _ -> false

  let is_c_strlen = function
    | Field {fn} ->
        Typ.Fieldname.equal fn (BufferOverrunField.c_strlen ())
    | _ ->
        false


  let is_java_collection_internal_array = function
    | Field {fn} ->
        Typ.Fieldname.equal fn BufferOverrunField.java_collection_internal_array
    | _ ->
        false


  let is_frontend_tmp = function Var x -> not (Var.appears_in_source_code x) | _ -> false

  let rec is_pretty = function
    | Var _ ->
        true
    | Allocsite a ->
        Allocsite.is_pretty a
    | Field {prefix= loc} | StarField {prefix= loc} ->
        is_pretty loc


  let of_c_strlen loc = append_field loc ~fn:(BufferOverrunField.c_strlen ())

  let of_pvar pvar = of_var (Var.of_pvar pvar)

  let of_id id = of_var (Var.of_id id)

  let rec of_path path =
    match path with
    | Symb.SymbolPath.Pvar pvar ->
        of_pvar pvar
    | Symb.SymbolPath.Deref _ | Symb.SymbolPath.Callsite _ ->
        of_allocsite (Allocsite.make_symbol path)
    | Symb.SymbolPath.Field {fn; prefix= path} ->
        append_field (of_path path) ~fn
    | Symb.SymbolPath.StarField {last_field= fn; prefix} ->
        append_star_field (of_path prefix) ~fn


  let is_return = function
    | Var (Var.ProgramVar x) ->
        Mangled.equal (Pvar.get_name x) Ident.name_return
    | _ ->
        false


  let is_field_of ~loc ~field_loc =
    match field_loc with Field {prefix= l} | StarField {prefix= l} -> equal loc l | _ -> false


  let is_literal_string = function Allocsite a -> Allocsite.is_literal_string a | _ -> None

  let is_literal_string_strlen = function
    | Field {prefix= l; fn} when Typ.Fieldname.equal (BufferOverrunField.c_strlen ()) fn ->
        is_literal_string l
    | _ ->
        None


  let rec is_global = function
    | Var (Var.ProgramVar pvar) ->
        Pvar.is_global pvar
    | Var (Var.LogicalVar _) | Allocsite _ ->
        false
    | Field {prefix= loc} | StarField {prefix= loc} ->
        is_global loc


  let rec get_path = function
    | Var (LogicalVar _) ->
        None
    | Var (ProgramVar pvar) ->
        Some (Symb.SymbolPath.of_pvar pvar)
    | Allocsite allocsite ->
        Allocsite.get_path allocsite
    | Field {prefix= l; fn; typ} ->
        Option.map (get_path l) ~f:(fun p -> Symb.SymbolPath.field ?typ p fn)
    | StarField {prefix; last_field} ->
        get_path prefix |> Option.map ~f:(fun p -> Symb.SymbolPath.star_field p last_field)


  let rec get_param_path = function
    | Var _ ->
        None
    | Allocsite allocsite ->
        Allocsite.get_param_path allocsite
    | Field {prefix= l; fn} ->
        Option.map (get_param_path l) ~f:(fun p -> Symb.SymbolPath.field p fn)
    | StarField {prefix; last_field} ->
        get_param_path prefix |> Option.map ~f:(fun p -> Symb.SymbolPath.star_field p last_field)


  let rec represents_multiple_values = function
    | Var _ ->
        false
    | Allocsite allocsite ->
        Allocsite.represents_multiple_values allocsite
    | Field _ as x when is_c_strlen x || is_java_collection_internal_array x ->
        false
    | Field {prefix= l} ->
        represents_multiple_values l
    | StarField _ ->
        true


  let rec exists_pvar ~f = function
    | Var (LogicalVar _) ->
        false
    | Var (ProgramVar pvar) ->
        f pvar
    | Allocsite allocsite ->
        Allocsite.exists_pvar ~f allocsite
    | Field {prefix= l} | StarField {prefix= l} ->
        exists_pvar ~f l


  let exists_str ~f l =
    Option.exists (get_path l) ~f:(fun path -> Symb.SymbolPath.exists_str_partial ~f path)


  let cast typ x =
    match x with
    | Field {prefix= l; fn} ->
        append_field l ~fn ~typ
    | StarField _ | Var _ | Allocsite _ ->
        x
end

module PowLoc = struct
  include AbstractDomain.FiniteSet (Loc)

  let bot = empty

  let is_bot = is_empty

  let unknown = singleton Loc.unknown

  let append_field ploc ~fn =
    if is_bot ploc then singleton Loc.unknown
    else fold (fun l -> add (Loc.append_field l ~fn)) ploc empty


  let append_star_field ploc ~fn =
    if is_bot ploc then singleton Loc.unknown
    else fold (fun l -> add (Loc.append_star_field l ~fn)) ploc empty


  let lift_cmp cmp_loc ploc1 ploc2 =
    match (is_singleton_or_more ploc1, is_singleton_or_more ploc2) with
    | IContainer.Singleton loc1, IContainer.Singleton loc2 ->
        Boolean.EqualOrder.of_equal cmp_loc (Loc.eq loc1 loc2)
    | _ ->
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
    fold (fun l acc -> join acc (subst_loc l eval_locpath)) x empty


  let exists_str ~f x = exists (fun l -> Loc.exists_str ~f l) x

  let of_c_strlen x = map Loc.of_c_strlen x

  let cast typ x = map (Loc.cast typ) x
end

let always_strong_update = false

let can_strong_update : PowLoc.t -> bool =
 fun ploc ->
  if always_strong_update then true
  else
    match PowLoc.is_singleton_or_more ploc with
    | IContainer.Singleton loc ->
        Loc.is_var loc || Loc.is_c_strlen loc
    | _ ->
        false
