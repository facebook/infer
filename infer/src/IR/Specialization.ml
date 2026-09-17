(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module HeapPath = struct
  (** this is the subset of HilExp.access_expression that make sense in a precondition TODO: deal
      with ArrayAccess *)
  type t = Pvar of Pvar.t | FieldAccess of (Fieldname.t * t) | Dereference of t
  [@@deriving equal, compare, hash, sexp, yojson_of]

  let rec pp fmt = function
    | Pvar pvar ->
        Pvar.pp_value fmt pvar
    | FieldAccess (fieldname, path) ->
        F.fprintf fmt "%a -> %a " pp path Fieldname.pp fieldname
    | Dereference path ->
        F.fprintf fmt "%a -> * " pp path


  module Map = PrettyPrintable.MakeHashSexpPPMap (struct
    type nonrec t = t [@@deriving compare, hash, sexp]

    let pp = pp
  end)

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t [@@deriving compare]

    let pp = pp
  end)
end

module Pulse = struct
  module Aliases = struct
    type t = HeapPath.t list list [@@deriving equal, compare, hash, sexp, yojson_of]

    let pp fmt aliases =
      let pp_alias fmt alias = Pp.seq ~sep:" = " HeapPath.pp fmt alias in
      Pp.seq ~sep:" && " pp_alias fmt aliases
  end

  module DynamicTypes = struct
    type t = Typ.name HeapPath.Map.t [@@deriving equal, compare, hash, sexp]

    let yojson_of_t map = [%yojson_of: (HeapPath.t * Typ.name) list] (HeapPath.Map.bindings map)

    let pp fmt dtypes =
      if not (HeapPath.Map.is_empty dtypes) then (
        F.fprintf fmt "dynamic_types: {" ;
        HeapPath.Map.iter
          (fun path value -> F.fprintf fmt "%a: %a;" HeapPath.pp path Typ.Name.pp value)
          dtypes ;
        F.fprintf fmt "}" )
  end

  module TreeBorrows = struct
    module ArgIndex = struct
      type t = int [@@deriving equal, compare, hash, sexp, yojson_of]

      let of_int i = i

      let to_int i = i

      let pp fmt i = F.pp_print_int fmt i
    end

    module Perm = struct
      type t = Reserved | Unique | Frozen | Disabled | ReservedConflicted
      [@@deriving equal, compare, hash, sexp, yojson_of]

      let pp fmt = function
        | Reserved ->
            F.pp_print_string fmt "Reserved"
        | Unique ->
            F.pp_print_string fmt "Unique"
        | Frozen ->
            F.pp_print_string fmt "Frozen"
        | Disabled ->
            F.pp_print_string fmt "Disabled"
        | ReservedConflicted ->
            F.pp_print_string fmt "ReservedConflicted"
    end

    module Rel = struct
      type t = Local | Foreign [@@deriving equal, compare, hash, sexp, yojson_of]

      let pp fmt = function
        | Local ->
            F.pp_print_string fmt "Local"
        | Foreign ->
            F.pp_print_string fmt "Foreign"
    end

    type t = {perms: (ArgIndex.t * Perm.t) list; rels: (ArgIndex.t * ArgIndex.t * Rel.t) list}
    [@@deriving equal, compare, hash, sexp, yojson_of]

    let bottom = {perms= []; rels= []}

    let is_bottom {perms; rels} = List.is_empty perms && List.is_empty rels

    let pp fmt ({perms; rels} as tb) =
      if not (is_bottom tb) then (
        F.fprintf fmt "tb: {" ;
        List.iter perms ~f:(fun (i, p) -> F.fprintf fmt "perm(%a)=%a;" ArgIndex.pp i Perm.pp p) ;
        List.iter rels ~f:(fun (i, j, r) ->
            F.fprintf fmt "rel(%a,%a)=%a;" ArgIndex.pp i ArgIndex.pp j Rel.pp r ) ;
        F.fprintf fmt "} " )
  end

  type t = {aliases: Aliases.t option; dynamic_types: DynamicTypes.t; tree_borrows: TreeBorrows.t}
  [@@deriving equal, compare, hash, sexp, yojson_of]

  let bottom = {aliases= None; dynamic_types= HeapPath.Map.empty; tree_borrows= TreeBorrows.bottom}

  let is_bottom {aliases; dynamic_types; tree_borrows} =
    Option.is_none aliases
    && HeapPath.Map.is_empty dynamic_types
    && TreeBorrows.is_bottom tree_borrows


  let pp_aliases fmt = function
    | None ->
        ()
    | Some aliases ->
        F.fprintf fmt "alias: %a " Aliases.pp aliases


  let pp fmt {aliases; dynamic_types; tree_borrows} =
    F.fprintf fmt "%a%a%a" pp_aliases aliases DynamicTypes.pp dynamic_types TreeBorrows.pp
      tree_borrows


  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)

  module Map = PrettyPrintable.MakePPMap (struct
    type nonrec t = t

    let pp = pp

    let compare = compare
  end)

  let is_pulse_specialization_limit_reached map =
    Map.cardinal map >= Config.pulse_specialization_limit


  let has_type_in_specialization {dynamic_types} specialized_type =
    (* for Hack we don't care if type is Foo or Foo$static *)
    let get_hack_static_companion_origin typ =
      if Typ.Name.Hack.is_class typ && Typ.Name.Hack.is_static_companion typ then
        Typ.Name.Hack.static_companion_origin typ
      else typ
    in
    HeapPath.Map.exists
      (fun _ typ ->
        let typ = get_hack_static_companion_origin typ in
        let specialized_type = get_hack_static_companion_origin specialized_type in
        Typ.Name.equal typ specialized_type )
      dynamic_types
end

type t = Pulse of Pulse.t [@@deriving equal, compare, hash, sexp]

let pp fmt = function Pulse t -> F.fprintf fmt "Pulse(%a)" Pulse.pp t
