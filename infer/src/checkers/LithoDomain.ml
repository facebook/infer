(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module LocalAccessPath = struct
  type t = {access_path: AccessPath.t; parent: Typ.Procname.t} [@@deriving compare]

  let equal = [%compare.equal: t]

  let make access_path parent = {access_path; parent}

  let make_from_access_expression ae parent =
    make (HilExp.AccessExpression.to_access_path ae) parent


  let to_formal_option {access_path= ((_, base_typ) as base), accesses; parent} formal_map =
    match FormalMap.get_formal_index base formal_map with
    | Some formal_index ->
        Some (make ((Var.of_formal_index formal_index, base_typ), accesses) parent)
    | None ->
        None


  let pp fmt t = AccessPath.pp fmt t.access_path
end

let suffixes = String.Set.of_list ["Attr"; "Dip"; "Px"; "Res"; "Sp"]

module MethodCall = struct
  type t = {receiver: LocalAccessPath.t; procname: Typ.Procname.t; location: Location.t}
  [@@deriving compare]

  let make receiver procname location = {receiver; procname; location}

  let pp fmt {receiver; procname} =
    F.fprintf fmt "%a.%a" LocalAccessPath.pp receiver Typ.Procname.pp procname


  let procname_to_string {procname} = Typ.Procname.get_method procname
end

module MethodCallPrefix = struct
  type t =
    { (* TODO: We can remove the [receiver] field after we replace the old checker *)
      receiver: LocalAccessPath.t [@compare.ignore]
    ; prefix: string
    ; procname: Typ.Procname.t [@compare.ignore]
    ; location: Location.t [@compare.ignore] }
  [@@deriving compare]

  let make receiver procname location =
    let method_name = Typ.Procname.get_method procname in
    let prefix_opt =
      String.Set.find_map suffixes ~f:(fun suffix -> String.chop_suffix method_name ~suffix)
    in
    let prefix = Option.value prefix_opt ~default:method_name in
    {receiver; prefix; procname; location}


  let pp fmt {receiver; procname} =
    F.fprintf fmt "%a.%a" LocalAccessPath.pp receiver Typ.Procname.pp procname


  let procname_to_string {procname} = Typ.Procname.get_method procname

  (* NOTE: This is only for sharing some code with the previous version of the checker. *)
  let to_method_call {receiver; procname; location} = MethodCall.make receiver procname location
end

module CallSet = AbstractDomain.FiniteSet (MethodCall)
module OldDomain = AbstractDomain.Map (LocalAccessPath) (CallSet)

module NewDomain = struct
  module CreatedLocation = struct
    type t = {location: Location.t [@ignore]; typ_name: Typ.name} [@@deriving compare]

    let pp fmt {location; typ_name} =
      F.fprintf fmt "Created at %a with type %a" Location.pp location Typ.Name.pp typ_name
  end

  module CreatedLocations = AbstractDomain.InvertedSet (CreatedLocation)

  module Created = struct
    include AbstractDomain.InvertedMap (LocalAccessPath) (CreatedLocations)

    let lookup k x = Option.value (find_opt k x) ~default:CreatedLocations.empty
  end

  module MethodCalls = struct
    module IsBuildMethodCalled = AbstractDomain.BooleanAnd
    module S = AbstractDomain.InvertedSet (MethodCallPrefix)

    type t = {is_build_method_called: IsBuildMethodCalled.t; method_calls: S.t}

    let pp fmt {is_build_method_called; method_calls} =
      F.fprintf fmt "%a%s" S.pp method_calls
        (if is_build_method_called then " then build() called" else "")


    let leq ~lhs ~rhs =
      IsBuildMethodCalled.leq ~lhs:lhs.is_build_method_called ~rhs:rhs.is_build_method_called
      && S.leq ~lhs:lhs.method_calls ~rhs:rhs.method_calls


    let join x y =
      { is_build_method_called=
          IsBuildMethodCalled.join x.is_build_method_called y.is_build_method_called
      ; method_calls= S.join x.method_calls y.method_calls }


    let widen ~prev ~next ~num_iters =
      { is_build_method_called=
          IsBuildMethodCalled.widen ~prev:prev.is_build_method_called
            ~next:next.is_build_method_called ~num_iters
      ; method_calls= S.widen ~prev:prev.method_calls ~next:next.method_calls ~num_iters }


    let empty = {is_build_method_called= false; method_calls= S.empty}

    let singleton e = {is_build_method_called= false; method_calls= S.singleton e}

    let add e ({is_build_method_called; method_calls} as x) =
      if is_build_method_called then x else {x with method_calls= S.add e method_calls}


    let set_build_method_called x = {x with is_build_method_called= true}

    let to_string_set method_calls =
      let accum_as_string method_call acc =
        String.Set.add acc (MethodCallPrefix.procname_to_string method_call)
      in
      S.fold accum_as_string method_calls String.Set.empty


    let get_call_chain method_calls =
      (* TODO: sort chain by inserted order *)
      S.elements method_calls


    let check_required_props ~check_on_string_set parent_typename
        {is_build_method_called; method_calls} =
      if is_build_method_called then
        let prop_set = to_string_set method_calls in
        let call_chain = get_call_chain method_calls in
        check_on_string_set parent_typename call_chain prop_set
  end

  module MethodCalled = struct
    include AbstractDomain.InvertedMap (CreatedLocation) (MethodCalls)

    let add_one k v x =
      let f = function
        | None ->
            Some (MethodCalls.singleton v)
        | Some method_calls ->
            Some (MethodCalls.add v method_calls)
      in
      update k f x


    let add_all created_locations callee x =
      CreatedLocations.fold
        (fun created_location acc -> add_one created_location callee acc)
        created_locations x


    let build_method_called_one created_location x =
      let f v =
        let method_calls = Option.value v ~default:MethodCalls.empty in
        Some (MethodCalls.set_build_method_called method_calls)
      in
      update created_location f x


    let build_method_called created_locations x =
      CreatedLocations.fold build_method_called_one created_locations x


    let check_required_props ~check_on_string_set x =
      let f CreatedLocation.{typ_name} method_calls =
        MethodCalls.check_required_props ~check_on_string_set typ_name method_calls
      in
      iter f x


    let check_required_props_of_created ~check_on_string_set ({CreatedLocation.typ_name} as created)
        x =
      Option.iter (find_opt created x) ~f:(fun method_calls ->
          MethodCalls.check_required_props ~check_on_string_set typ_name method_calls )
  end

  type t = {created: Created.t; method_called: MethodCalled.t}

  let pp fmt {created; method_called} =
    F.fprintf fmt "@[<v 0>@[Created:@;%a@]@,@[MethodCalled:@;%a@]@]" Created.pp created
      MethodCalled.pp method_called


  let leq ~lhs ~rhs =
    Created.leq ~lhs:lhs.created ~rhs:rhs.created
    && MethodCalled.leq ~lhs:lhs.method_called ~rhs:rhs.method_called


  let join x y =
    { created= Created.join x.created y.created
    ; method_called= MethodCalled.join x.method_called y.method_called }


  let widen ~prev ~next ~num_iters =
    { created= Created.widen ~prev:prev.created ~next:next.created ~num_iters
    ; method_called= MethodCalled.widen ~prev:prev.method_called ~next:next.method_called ~num_iters
    }


  let empty = {created= Created.empty; method_called= MethodCalled.empty}

  let assign ~lhs ~rhs ({created} as x) =
    {x with created= Created.add lhs (Created.lookup rhs created) created}


  let call_create lhs typ_name location ({created} as x) =
    {x with created= Created.add lhs (CreatedLocations.singleton {location; typ_name}) created}


  let call_builder ~ret ~receiver callee {created; method_called} =
    let created_locations = Created.lookup receiver created in
    { created= Created.add ret created_locations created
    ; method_called= MethodCalled.add_all created_locations callee method_called }


  let call_build_method ~ret ~receiver {created; method_called} =
    let created_locations = Created.lookup receiver created in
    { created= Created.add ret created_locations created
    ; method_called= MethodCalled.build_method_called created_locations method_called }


  let check_required_props ~check_on_string_set {method_called} =
    MethodCalled.check_required_props ~check_on_string_set method_called


  let check_required_props_of_receiver ~pname ~check_on_string_set receiver {created; method_called}
      =
    let check_on_created_location created =
      MethodCalled.check_required_props_of_created ~check_on_string_set created method_called
    in
    let receiver_path = LocalAccessPath.make_from_access_expression receiver pname in
    let created_locations = Created.lookup receiver_path created in
    CreatedLocations.iter check_on_created_location created_locations
end

include struct
  include AbstractDomain.Pair (OldDomain) (NewDomain)

  let lift_old f (o, _) = f o

  let lift_new f (_, n) = f n

  let map_old f (o, n) = (f o, n)

  let map_new f (o, n) = (o, f n)

  let empty = (OldDomain.empty, NewDomain.empty)

  let add k v = map_old (OldDomain.add k v)

  let remove k = map_old (OldDomain.remove k)

  let bindings = lift_old OldDomain.bindings

  let find k = lift_old (OldDomain.find k)

  let mem k = lift_old (OldDomain.mem k)

  let iter f = lift_old (OldDomain.iter f)

  let fold f (o, _) init = OldDomain.fold f o init

  let assign ~lhs ~rhs = map_new (NewDomain.assign ~lhs ~rhs)

  let call_create ret typ_name location = map_new (NewDomain.call_create ret typ_name location)

  let call_builder ~ret ~receiver callee = map_new (NewDomain.call_builder ~ret ~receiver callee)

  let call_build_method ~ret ~receiver = map_new (NewDomain.call_build_method ~ret ~receiver)

  let check_required_props ~check_on_string_set =
    lift_new (NewDomain.check_required_props ~check_on_string_set)


  let check_required_props_of_receiver ~pname ~check_on_string_set receiver =
    lift_new (NewDomain.check_required_props_of_receiver ~pname ~check_on_string_set receiver)
end

let substitute ~(f_sub : LocalAccessPath.t -> LocalAccessPath.t option) ((_, new_astate) as astate)
    =
  let old_astate, _ =
    fold
      (fun original_access_path call_set acc ->
        let access_path' =
          match f_sub original_access_path with
          | Some access_path ->
              access_path
          | None ->
              original_access_path
        in
        let call_set' =
          CallSet.fold
            (fun ({procname; location} as call) call_set_acc ->
              let receiver =
                match f_sub call.receiver with Some receiver' -> receiver' | None -> call.receiver
              in
              CallSet.add {receiver; procname; location} call_set_acc )
            call_set CallSet.empty
        in
        add access_path' call_set' acc )
      astate empty
  in
  (old_astate, new_astate)


(** Unroll the domain to enumerate all the call chains ending in [call] and apply [f] to each
    maximal chain. For example, if the domain encodes the chains foo().bar().goo() and foo().baz(),
    [f] will be called once on foo().bar().goo() and once on foo().baz() *)
let iter_call_chains_with_suffix ~f call_suffix astate =
  let rec unroll_call_ ({receiver; procname} as call : MethodCall.t) (acc, visited) =
    let is_cycle (call : MethodCall.t) =
      (* detect direct cycles and cycles due to mutual recursion *)
      LocalAccessPath.equal call.receiver receiver || Typ.Procname.Set.mem call.procname visited
    in
    let acc' = call :: acc in
    let visited' = Typ.Procname.Set.add procname visited in
    try
      let calls' = find receiver astate in
      CallSet.iter
        (fun call ->
          if not (is_cycle call) then unroll_call_ call (acc', visited')
          else f receiver.access_path acc' )
        calls'
    with Caml.Not_found -> f receiver.access_path acc'
  in
  unroll_call_ call_suffix ([], Typ.Procname.Set.empty)


let iter_call_chains ~f astate =
  iter
    (fun _ call_set ->
      CallSet.iter (fun call -> iter_call_chains_with_suffix ~f call astate) call_set )
    astate
