(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

let all_formals_untainted pdesc =
  let make_untainted (name, typ) =
    name, typ, None in
  List.map ~f:make_untainted (Procdesc.get_formals pdesc)

module type Kind = sig
  include TraceElem.Kind

  val unknown : t

  val get : Typ.Procname.t -> Tenv.t -> t option

  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
end

module type S = sig
  include TraceElem.S

  val is_footprint : t -> bool

  val make_footprint : AccessPath.t -> Procdesc.t -> t

  val get_footprint_access_path: t -> AccessPath.t option

  val get : CallSite.t -> Tenv.t -> t option

  val get_tainted_formals : Procdesc.t -> Tenv.t -> (Mangled.t * Typ.t * t option) list
end

module Make (Kind : Kind) = struct
  module Kind = Kind

  type kind =
    | Normal of Kind.t (** known source returned directly or transitively from a callee *)
    | Footprint of AccessPath.t (** unknown source read from the environment *)
  [@@deriving compare]

  let pp_kind fmt = function
    | Normal kind -> Kind.pp fmt kind
    | Footprint ap -> F.fprintf fmt "Footprint(%a)" AccessPath.pp ap

  type t =
    {
      kind : kind;
      site : CallSite.t;
    } [@@deriving compare]

  let is_footprint t = match t.kind with
    | Footprint _ -> true
    | _ -> false

  let get_footprint_access_path t = match t.kind with
    | Footprint ap -> Some ap
    | _ -> None

  let call_site t =
    t.site

  let kind t = match t.kind with
    | Normal kind -> kind
    | Footprint _ -> Kind.unknown

  let make kind site =
    { site; kind = Normal kind; }

  let make_footprint ap pdesc =
    let kind = Footprint ap in
    let site = CallSite.make (Procdesc.get_proc_name pdesc) (Procdesc.get_loc pdesc) in
    { site; kind; }

  let get site tenv = match Kind.get (CallSite.pname site) tenv with
    | Some kind -> Some (make kind site)
    | None -> None

  let get_tainted_formals pdesc tenv =
    let site = CallSite.make (Procdesc.get_proc_name pdesc) (Procdesc.get_loc pdesc) in
    List.map
      ~f:(fun (name, typ, kind_opt) ->
          name, typ, Option.map kind_opt ~f:(fun kind -> make kind site))
      (Kind.get_tainted_formals pdesc tenv)

  let pp fmt s =
    F.fprintf fmt "%a(%a)" pp_kind s.kind CallSite.pp s.site

  let with_callsite t callee_site =
    if is_footprint t
    then failwithf "Can't change the call site of footprint source %a" pp t;
    { t with site = callee_site; }

  module Set = PrettyPrintable.MakePPSet(struct
      type nonrec t = t
      let compare = compare
      let pp = pp
    end)
end

module Dummy = struct
  type t = unit [@@deriving compare]

  let call_site _ = CallSite.dummy

  let kind t = t

  let make kind _ = kind

  let pp _ () = ()

  let is_footprint _ = false

  let make_footprint _ _ = assert false
  let get_footprint_access_path _ = assert false

  let get _ _ = None

  let get_tainted_formals pdesc _=
    List.map ~f:(fun (name, typ) -> name, typ, None) (Procdesc.get_formals pdesc)

  module Kind = struct
    type nonrec t = t
    let compare = compare
    let pp = pp
  end

  module Set = PrettyPrintable.MakePPSet(struct
      type nonrec t = t
      let compare = compare
      let pp = pp
    end)

  let with_callsite t _ = t
end
