(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module Stack = AbductiveDomain.Stack
module Memory = AbductiveDomain.Memory
module CanonValue = AbductiveDomain.CanonValue
module BaseAddressAttributes = CanonValue.Attributes
module BaseDomain = PulseBaseDomain

(* {1 Pretty-Printing of Abstract States}

   The goal is to print (both in plain text and HTML) a more human-readable description of abstract
   states by:

   - following pointers and field accesses in the heap from as much as possible, starting from
     program and logical variables in the stack

   - stopping at "cut points": abstract values that are reachable via multiple access paths

   - exploring again with each cut point as a root until all cut points have been exhausted

   In other words, this computes and displays a spanning tree of the memory graph. The algorithm is
   split into three parts:

   1. [Explainer] builds a map from abstract value to their [description]s and identifies cut points

   2. [HTMLDecorator] is used to display extra information along access paths such as the attributes
      attached to each value. The information is displayed as HTML tooltips so it doesn't clutter
      the presentation.

   3. [Printer] uses both the "explainer" and the "decorator" to explore and print the abstract
      state following the principles above.
*)

type access_path = Var.t * Access.t list [@@deriving compare]

(** how to display an abstract vaue *)
type description =
  | Unique of access_path
      (** the value appears only once in the state; no need to print it explicitly *)
  | Aliases of {print_as: Var.t option; equal_vars: Var.t list; aliases: access_path list}
      (** the value is a cut-point: stop the exploration at this value *)
  | Term of PulseFormula.term * access_path list
      (** the value corresponds to a pure expression that is easier to understand, eg a numerical
          constant *)

let get_as_var ((var, accesses) : access_path) =
  match (Var.is_pvar var, accesses) with
  | false, [] ->
      (* example: [n$0] *) Some var
  | true, [Dereference] ->
      (* example: [*&x] is [x] *)
      Some var
  | _ ->
      None


(** the inverse of [get_as_var] *)
let to_access_path var : access_path = if Var.is_pvar var then (var, [Dereference]) else (var, [])

(** alias to get [compare_var_to_prefer] easily *)
type var_to_prefer = Var.t

(** prefer program variables over logical ones *)
let compare_var_to_prefer var1 var2 =
  let to_metric var = (not (Var.is_pvar var), var) in
  [%compare: bool * Var.t] (to_metric var1) (to_metric var2)


type 'a option_prefer_some = 'a option

let compare_option_prefer_some compare o1 o2 =
  let compare_flipped x1 x2 = compare x2 x1 in
  Option.compare compare_flipped o2 o1


let compare_access_path_to_prefer access_path1 access_path2 =
  let to_metric access_path =
    (get_as_var access_path, List.length (snd access_path), access_path)
  in
  [%compare: var_to_prefer option_prefer_some * int * access_path] (to_metric access_path1)
    (to_metric access_path2)


module Explainer = struct
  (** auxiliary function for [make_explainer]: compute the [(print_as, equal_vars, aliases)]
      arguments of [Aliases] for two access paths; in other words, pick which of the access paths to
      display as variables and which one to prefer if both are variables *)
  let classify_access_paths access_path1 access_path2 =
    match (get_as_var access_path1, get_as_var access_path2) with
    | None, None ->
        (None, [], [access_path1; access_path2])
    | Some var, None ->
        (Some var, [], [access_path2])
    | None, Some var ->
        (Some var, [], [access_path1])
    | Some var1, Some var2 ->
        if compare_var_to_prefer var1 var2 < 0 then (Some var1, [var2], [])
        else (Some var2, [var1], [])


  let make pre_or_post astate =
    let f root_var explainer v accesses =
      let description =
        match PulseFormula.explain_as_term astate.AbductiveDomain.path_condition v with
        | None ->
            Unique (root_var, accesses)
        | Some term ->
            Term (term, [(root_var, accesses)])
      in
      Continue_or_stop.Continue (AbstractValue.Map.add v description explainer)
    in
    let f_revisit root_var explainer v accesses =
      let description =
        match AbstractValue.Map.find v explainer with
        | Term (t, aliases) ->
            Term (t, (root_var, accesses) :: aliases)
        | Unique alias ->
            let print_as, equal_vars, aliases = classify_access_paths alias (root_var, accesses) in
            Aliases {print_as; equal_vars; aliases}
        | Aliases {print_as= Some var; equal_vars; aliases} ->
            let print_as, equal_vars', aliases' =
              classify_access_paths (to_access_path var) (root_var, accesses)
            in
            Aliases {print_as; equal_vars= equal_vars' @ equal_vars; aliases= aliases' @ aliases}
        | Aliases {print_as= None; equal_vars; aliases} -> (
          match get_as_var (root_var, accesses) with
          | None ->
              Aliases {print_as= None; equal_vars; aliases= (root_var, accesses) :: aliases}
          | Some var ->
              Aliases {print_as= Some var; equal_vars; aliases} )
      in
      AbstractValue.Map.add v description explainer
    in
    AbductiveDomain.fold_all astate pre_or_post ~init:AbstractValue.Map.empty ~finish:Fn.id
      ~f_revisit ~f
end

(** the record type exists only to get past OCaml's value restriction and be able to generalize the
    type ['a] for the pretty printer passed as argument in functions that use the decorator *)
type decorator =
  { f:
      'a.
         AbstractValue.t * ValueHistory.t option
      -> (F.formatter -> 'a -> unit)
      -> F.formatter
      -> 'a
      -> unit }

module HTMLDecorator = struct
  type clazz = Invalid | Tooltip

  let string_of_class = function Invalid -> "pulse_invalid" | Tooltip -> "with_tooltip"

  type tag_attrs = {clazz: clazz list; tooltip: (F.formatter -> unit) list}

  let add_class clazz tag_attrs = {tag_attrs with clazz= clazz :: tag_attrs.clazz}

  let add_tooltip tooltip tag_attrs = {tag_attrs with tooltip= tooltip :: tag_attrs.tooltip}

  let empty_tag_attrs = {clazz= []; tooltip= []}

  let value_tooltip v fmt = F.fprintf fmt "value: %a" AbstractValue.pp v

  let mk_tag_attrs v = add_tooltip (value_tooltip v) empty_tag_attrs

  let span {clazz; tooltip} pp fmt x =
    let has_tooltip = not (List.is_empty tooltip) in
    let clazz = if has_tooltip then Tooltip :: clazz else clazz in
    let pp_class fmt clazz =
      if not (List.is_empty clazz) then
        F.fprintf fmt " class=\"%a\""
          (Pp.seq ~sep:" " (fun fmt clazz -> string_of_class clazz |> F.pp_print_string fmt))
          (List.rev clazz)
    in
    let mk_tooltip tooltip =
      let rec pp_tooltip_ is_first fmt = function
        | [] ->
            ()
        | part :: rest ->
            if not is_first then F.pp_print_newline fmt () ;
            part fmt ;
            pp_tooltip_ false fmt rest
      in
      F.asprintf "%a" (pp_tooltip_ true) (List.rev tooltip)
    in
    let open_span = F.asprintf "<span%a>" pp_class clazz in
    let close_span =
      if has_tooltip then
        (* "color_black" because the surrounding span might have a color and we do not want to
           inherit it in the tooltip *)
        F.asprintf "<span class=\"color_black tooltip\">%s</span></span>" (mk_tooltip tooltip)
      else "</span>"
    in
    F.fprintf fmt "@<0>%s%a@<0>%s" open_span pp x close_span


  let with_attributes_hover attrs_opt span_attrs =
    match attrs_opt with
    | Some attrs when not (Attributes.is_empty attrs) ->
        add_tooltip
          (fun fmt ->
            F.fprintf fmt "@[<hv2>attributes: %a@]" (Attributes.pp ~print_rank:false) attrs )
          span_attrs
    | _ ->
        span_attrs


  let has_attr attrs_opt ~(f : Attribute.t -> bool) =
    Option.exists attrs_opt ~f:(fun attrs -> Attributes.exists attrs ~f)


  let decorator astate pre_or_post (v, hist_opt) pp fmt x =
    let attrs =
      let attributes =
        match pre_or_post with
        | `Pre ->
            (astate.AbductiveDomain.pre :> BaseDomain.t).attrs
        | `Post ->
            (astate.AbductiveDomain.post :> BaseDomain.t).attrs
      in
      PulseBaseAddressAttributes.find_opt v attributes
    in
    let span_attrs = mk_tag_attrs v in
    let span_attrs =
      match hist_opt with
      | Some hist when Config.debug_level_analysis >= 3 ->
          add_tooltip
            (fun fmt -> F.fprintf fmt "@[<hv2>history: %a@]" ValueHistory.pp hist)
            span_attrs
      | _ ->
          span_attrs
    in
    let span_attrs = with_attributes_hover attrs span_attrs in
    let span_attrs =
      if has_attr attrs ~f:(function Invalid _ -> true | _ -> false) then
        add_class Invalid span_attrs
      else span_attrs
    in
    span span_attrs pp fmt x


  let dummy_decorator = {f= (fun _v_hist pp fmt x -> pp fmt x)}

  let make (pp_kind : Pp.print_kind) pre_post astate =
    match pp_kind with
    | TEXT ->
        dummy_decorator
    | HTML ->
        {f= (fun v -> decorator astate pre_post v)}
end

module Printer = struct
  let pp_var pp_kind ~with_ampersand fmt var =
    if with_ampersand && Var.is_pvar var then
      F.fprintf fmt "%a%a"
        (Pp.escape_xml F.pp_print_string pp_kind)
        "&" (Pp.escape_xml Var.pp pp_kind) var
    else Var.pp fmt var


  let pp_deref_symbol pp_kind fmt () = Pp.escape_xml F.pp_print_string pp_kind fmt "->"

  let pp_mixed_access_begin pp_kind fmt =
    Pp.with_color pp_kind Blue (Pp.escape_xml F.pp_print_string pp_kind) fmt "<<"


  let pp_mixed_access_end pp_kind fmt =
    Pp.with_color pp_kind Blue (Pp.escape_xml F.pp_print_string pp_kind) fmt ">>"


  let has_edges pre_or_post v astate = Memory.exists_edge v astate ~pre_or_post ~f:(fun _ -> true)

  (** the pointee of the value provided, if that's the one and only access edge from that value,
      otherwise [None] *)
  let get_only_pointer_edge pre_or_post v astate =
    Memory.fold_edges v astate ~pre_or_post ~init:None ~f:(fun prev_edge (access, value) ->
        match (prev_edge, access) with
        | Some _, _ ->
            Some None
        | None, Dereference ->
            Some (Some value)
        | None, _ ->
            Some None )
    |> Option.join


  let pp_stack_and_heap decorator pp_kind pre_or_post explainer fmt astate =
    let delayed_values = ref [] in
    let add_delayed_value value_hist = delayed_values := value_hist :: !delayed_values in
    let printed = ref AbstractValue.Set.empty in
    let should_print_and_record value =
      if AbstractValue.Set.mem value !printed then false
      else (
        printed := AbstractValue.Set.add value !printed ;
        true )
    in
    let pp_value_ fmt (value, hist) =
      match AbstractValue.Map.find value explainer with
      | Aliases {print_as} -> (
          (* TODO: optional histories or something *)
          add_delayed_value (value, hist) ;
          match print_as with
          | Some var ->
              Pp.escape_xml Var.pp pp_kind fmt var
          | None ->
              AbstractValue.pp fmt value )
      | Unique _ ->
          F.pp_print_string fmt "-"
      | Term (t, _) ->
          Pp.escape_xml (PulseFormula.pp_term AbstractValue.pp) pp_kind fmt t
    in
    let pp_value_hist_opt fmt value_hist_opt =
      decorator.f value_hist_opt pp_value_ fmt value_hist_opt
    in
    let pp_value fmt (value, hist) = pp_value_hist_opt fmt (value, Some hist) in
    let pp_value_no_hist fmt value = pp_value_hist_opt fmt (value, None) in
    let rec pp_value_accesses ~is_prev_deref fmt (value, hist_opt) =
      let dereference, field_accesses, array_accesses =
        Memory.fold_edges ~pre_or_post value astate ~init:(None, [], [])
          ~f:(fun (dereference, field_accesses, array_accesses) (access, value_hist) ->
            match (access : Access.t) with
            | FieldAccess field ->
                (dereference, (field, value_hist) :: field_accesses, array_accesses)
            | ArrayAccess (_, index) ->
                (dereference, field_accesses, (index, value_hist) :: array_accesses)
            | Dereference ->
                (Some value_hist, field_accesses, array_accesses) )
      in
      let has_mixed_accesses =
        match (dereference, field_accesses, array_accesses) with
        | _, [], [] | None, _, [] | None, [], _ ->
            false
        | Some _, _ :: _, _ | Some _, _, _ :: _ | None, _ :: _, _ :: _ ->
            true
      in
      if
        match (dereference, field_accesses, array_accesses) with None, [], [] -> true | _ -> false
      then
        F.fprintf fmt "%s%a" (if is_prev_deref then "" else "=") pp_value_hist_opt (value, hist_opt) ;
      if has_mixed_accesses then F.fprintf fmt "%t@[" (pp_mixed_access_begin pp_kind) ;
      Option.iter dereference ~f:(fun element ->
          decorator.f (value, hist_opt) (pp_deref_symbol pp_kind) fmt () ;
          pp_from_value ~is_prev_deref:true fmt element ) ;
      if not (List.is_empty field_accesses) then (
        if has_mixed_accesses then F.fprintf fmt ", @," ;
        F.pp_print_string fmt "." ;
        let is_single_access = match field_accesses with [_] -> true | _ -> false in
        if not is_single_access then F.fprintf fmt "{@[<hv>" ;
        List.fold field_accesses ~init:true ~f:(fun is_first access_info ->
            if not is_first then F.fprintf fmt ",@;" ;
            pp_field_access (value, hist_opt) fmt access_info ;
            false )
        |> ignore ;
        if not is_single_access then F.fprintf fmt "@]}" ) ;
      List.fold array_accesses ~init:true ~f:(fun is_first access_info ->
          if not is_first then F.fprintf fmt ",@;" ;
          pp_array_access fmt access_info ;
          false )
      |> ignore ;
      if has_mixed_accesses then F.fprintf fmt "@]%t" (pp_mixed_access_end pp_kind) ;
      ()
    and pp_field_access (root_val, root_hist_opt) fmt (field, value_hist) =
      decorator.f (root_val, root_hist_opt) Fieldname.pp fmt field ;
      pp_from_value ~is_prev_deref:false fmt value_hist
    and pp_array_access fmt (index, value_hist) =
      F.fprintf fmt "[%a]" pp_value_no_hist index ;
      pp_from_value ~is_prev_deref:false fmt value_hist
    and pp_from_value ~is_prev_deref fmt (value, hist) =
      match AbstractValue.Map.find value explainer with
      | Aliases _ ->
          add_delayed_value (value, Some hist) ;
          F.fprintf fmt "%s%a" (if is_prev_deref then "" else "=") pp_value (value, hist)
      | Unique _ ->
          pp_value_accesses ~is_prev_deref fmt (value, Some hist)
      | Term (t, _) ->
          F.fprintf fmt "%s%a"
            (if is_prev_deref then "" else "=")
            (decorator.f (value, Some hist) (PulseFormula.pp_term AbstractValue.pp))
            t
    in
    let rec empty_delayed fmt =
      let delayed_values_ = !delayed_values in
      if not (List.is_empty delayed_values_) then (
        delayed_values := [] ;
        List.iter delayed_values_ ~f:(fun (value, hist) ->
            if should_print_and_record value && has_edges pre_or_post value astate then (
              F.fprintf fmt "@;* " ;
              pp_value_hist_opt fmt (value, hist) ;
              pp_value_accesses ~is_prev_deref:false fmt (value, hist) ) ;
            empty_delayed fmt ) )
    in
    let need_sep = ref false in
    let new_elem () =
      if !need_sep then F.fprintf fmt "@;* " ;
      need_sep := true
    in
    Stack.fold ~pre_or_post
      (fun var ((value, hist) as value_hist) () ->
        match AbstractValue.Map.find value explainer with
        | Aliases {print_as} ->
            if not (Option.exists print_as ~f:(Var.equal var)) then (
              new_elem () ;
              add_delayed_value (value, Some hist) ;
              F.fprintf fmt "@[<h>%a=%a@]"
                (pp_var pp_kind ~with_ampersand:true)
                var pp_value value_hist )
        | Unique _ -> (
            let pp, var_value_hist, with_ampersand =
              if Var.is_pvar var then
                match get_only_pointer_edge pre_or_post value astate with
                | None ->
                    ( (fun fmt (v, hist) ->
                        pp_value_accesses ~is_prev_deref:false fmt (v, Some hist) )
                    , value_hist
                    , true )
                | Some (var_address_deref_value, hist) ->
                    let pp fmt (v, hist) =
                      match AbstractValue.Map.find var_address_deref_value explainer with
                      | Aliases _ ->
                          add_delayed_value (v, Some hist) ;
                          F.fprintf fmt "=%a" pp_value (v, hist)
                      | Term _ ->
                          F.fprintf fmt "=%a" pp_value (v, hist)
                      | Unique _ ->
                          pp_value_accesses ~is_prev_deref:false fmt (v, Some hist)
                    in
                    (pp, (var_address_deref_value, hist), false)
              else
                ( (fun fmt (v, hist) -> pp_value_accesses ~is_prev_deref:false fmt (v, Some hist))
                , value_hist
                , false )
            in
            match AbstractValue.Map.find (fst var_value_hist) explainer with
            | Aliases {print_as= Some var'} when Var.equal var var' ->
                ()
            | _ ->
                new_elem () ;
                F.fprintf fmt "@[<h>%a%a@]" (pp_var pp_kind ~with_ampersand) var pp var_value_hist )
        | Term (t, _) ->
            new_elem () ;
            add_delayed_value (value, Some hist) ;
            F.fprintf fmt "@[<h>%a=%a@]"
              (pp_var pp_kind ~with_ampersand:true)
              var
              (decorator.f (value, Some hist) (PulseFormula.pp_term AbstractValue.pp))
              t )
      astate () ;
    empty_delayed fmt


  type resolved = Unreachable of AbstractValue.t | AP of access_path | T of PulseFormula.term

  let resolve_access_path v explainer =
    match AbstractValue.Map.find_opt v explainer with
    | None ->
        Unreachable v
    | Some (Unique access_path) ->
        AP access_path
    | Some (Aliases {print_as= Some var}) ->
        AP (to_access_path var)
    | Some (Aliases {print_as= None; aliases}) ->
        AP (List.min_elt aliases ~compare:compare_access_path_to_prefer |> Option.value_exn)
    | Some (Term (t, _)) ->
        T t


  let rec pp_resolve_value pp_kind explainer fmt v =
    match resolve_access_path v explainer with
    | T t ->
        Pp.escape_xml (PulseFormula.pp_term (pp_resolve_value pp_kind explainer)) pp_kind fmt t
    | Unreachable v ->
        AbstractValue.pp fmt v
    | AP (var, rev_accesses) ->
        let with_ampersand, accesses =
          match List.rev rev_accesses with
          | Dereference :: accesses when Var.is_pvar var ->
              (false, accesses)
          | accesses ->
              (true, accesses)
        in
        F.fprintf fmt "%a%a" (pp_var pp_kind ~with_ampersand) var
          (Pp.seq ~sep:"" (pp_access pp_kind explainer))
          accesses


  and pp_access pp_kind explainer fmt (access : Access.t) =
    match access with
    | FieldAccess field ->
        F.fprintf fmt ".%a" (Pp.escape_xml Fieldname.pp pp_kind) field
    | ArrayAccess (_, index) ->
        F.fprintf fmt "[%a]" (Pp.escape_xml (pp_resolve_value pp_kind explainer) pp_kind) index
    | Dereference ->
        pp_deref_symbol pp_kind fmt ()


  let pp_attributes pp_kind pre_or_post explainer fmt astate =
    let attrs =
      match pre_or_post with
      | `Pre ->
          (astate.AbductiveDomain.pre :> BaseDomain.t).attrs
      | `Post ->
          (astate.AbductiveDomain.post :> BaseDomain.t).attrs
    in
    BaseAddressAttributes.fold
      (fun v attrs () ->
        if not (Attributes.is_empty attrs) then
          F.fprintf fmt "%a: %a@;"
            (pp_resolve_value pp_kind explainer)
            (CanonValue.downcast v) (Attributes.pp ~print_rank:false) attrs )
      attrs ()


  let pp_formula pp_kind explainer fmt astate =
    PulseFormula.pp_formula_explained
      (fun fmt v -> (pp_resolve_value pp_kind explainer) fmt v)
      fmt astate


  let pp_conditions pp_kind explainer fmt astate =
    PulseFormula.pp_conditions_explained
      (fun fmt v -> (pp_resolve_value pp_kind explainer) fmt v)
      fmt astate
end

let pp (pp_kind : Pp.print_kind) path_opt fmt astate =
  let pp_ pre_post =
    let explainer = Explainer.make pre_post astate in
    let decorator = HTMLDecorator.make pp_kind pre_post astate in
    let pp_pure =
      match pre_post with `Pre -> Printer.pp_conditions | `Post -> Printer.pp_formula
    in
    F.fprintf fmt "@\n@[<hv2>%s:@;%a%a@]"
      (match pre_post with `Pre -> "Inferred pre" | `Post -> "Current post")
      (Printer.pp_stack_and_heap decorator pp_kind pre_post explainer)
      astate (pp_pure pp_kind explainer) astate.AbductiveDomain.path_condition ;
    match pp_kind with
    | TEXT ->
        F.fprintf fmt "@;%a" (Printer.pp_attributes pp_kind pre_post explainer) astate
    | HTML ->
        ()
  in
  pp_ `Post ;
  pp_ `Pre ;
  F.pp_print_newline fmt () ;
  let pp_raw_state fmt () =
    let pp_path_opt fmt path_opt =
      Option.iter path_opt ~f:(fun path -> F.fprintf fmt "@\n%a" PulsePathContext.pp path)
    in
    F.fprintf fmt "%a%a" AbductiveDomain.pp astate pp_path_opt path_opt
  in
  Pp.html_collapsible_block ~name:"Raw state" pp_kind pp_raw_state fmt ()
