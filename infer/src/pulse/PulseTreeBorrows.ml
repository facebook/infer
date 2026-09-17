(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue
module AVMap = AbstractValue.Map
module AVSet = AbstractValue.Set
module IdentMap = Stdlib.Map.Make (Ident)
module TBSpec = Specialization.Pulse.TreeBorrows

module Tag = struct
  type t = int [@@deriving compare, equal]

  let pp fmt t = F.fprintf fmt "T%d" t

  module Key = struct
    type nonrec t = t [@@deriving compare]
  end

  module Map = Stdlib.Map.Make (Key)
  module Set = Stdlib.Set.Make (Key)

  module Info = struct
    type t = {protector: bool; borrowed_cell: AbstractValue.t option} [@@deriving compare, equal]
  end
end

module Perm = TBSpec.Perm

module Access = struct
  type t = Read | Write [@@deriving compare, equal]
end

module Rel = struct
  type t = Local | Foreign | Unrelated
end

module Ub = struct
  type t =
    | Disabled_local_read
    | Disabled_local_read_protected
    | Frozen_local_write
    | Frozen_local_write_protected
    | Disabled_local_write
    | Disabled_local_write_protected
    | ResC_local_write
    | Unique_foreign_read_protected
    | Foreign_write_protected

  let pp fmt = function
    | Disabled_local_read ->
        F.pp_print_string fmt "read through a Disabled tag"
    | Disabled_local_read_protected ->
        F.pp_print_string fmt "read through a protected Disabled tag"
    | Frozen_local_write ->
        F.pp_print_string fmt "write through a Frozen tag"
    | Frozen_local_write_protected ->
        F.pp_print_string fmt "write through a protected Frozen tag"
    | Disabled_local_write ->
        F.pp_print_string fmt "write through a Disabled tag"
    | Disabled_local_write_protected ->
        F.pp_print_string fmt "write through a protected Disabled tag"
    | ResC_local_write ->
        F.pp_print_string fmt "write through a ReservedConflicted tag"
    | Unique_foreign_read_protected ->
        F.pp_print_string fmt "foreign read of a protected Unique tag"
    | Foreign_write_protected ->
        F.pp_print_string fmt "foreign write to a protected tag"
end

module Trans = struct
  let fire_local (perm : Perm.t) ~(protector : bool) (access : Access.t) : (Perm.t, Ub.t) Result.t =
    match (access, perm, protector) with
    | Read, Reserved, _ ->
        Ok Perm.Reserved
    | Read, Unique, _ ->
        Ok Perm.Unique
    | Read, Frozen, _ ->
        Ok Perm.Frozen
    | Read, ReservedConflicted, _ ->
        Ok Perm.ReservedConflicted
    | Read, Disabled, false ->
        Error Ub.Disabled_local_read
    | Read, Disabled, true ->
        Error Ub.Disabled_local_read_protected
    | Write, Reserved, _ ->
        Ok Perm.Unique
    | Write, Unique, _ ->
        Ok Perm.Unique
    | Write, Frozen, false ->
        Error Ub.Frozen_local_write
    | Write, Frozen, true ->
        Error Ub.Frozen_local_write_protected
    | Write, Disabled, false ->
        Error Ub.Disabled_local_write
    | Write, Disabled, true ->
        Error Ub.Disabled_local_write_protected
    | Write, ReservedConflicted, _ ->
        Error Ub.ResC_local_write


  let fire_foreign (perm : Perm.t) ~(protector : bool) (access : Access.t) : (Perm.t, Ub.t) Result.t
      =
    match (access, perm, protector) with
    | Read, Reserved, false ->
        Ok Perm.Reserved
    | Read, Unique, false ->
        Ok Perm.Frozen
    | Read, Frozen, false ->
        Ok Perm.Frozen
    | Read, Disabled, false ->
        Ok Perm.Disabled
    | Read, ReservedConflicted, false ->
        Ok Perm.ReservedConflicted
    | Read, Reserved, true ->
        Ok Perm.ReservedConflicted
    | Read, Unique, true ->
        Error Ub.Unique_foreign_read_protected
    | Read, Frozen, true ->
        Ok Perm.Frozen
    | Read, Disabled, true ->
        Ok Perm.Disabled
    | Read, ReservedConflicted, true ->
        Ok Perm.ReservedConflicted
    | Write, _, false ->
        Ok Perm.Disabled
    | Write, Disabled, true ->
        Ok Perm.Disabled
    | Write, _, true ->
        Error Ub.Foreign_write_protected
end

module Operand = struct
  (** how a value is designated in an instruction: through a temporary [root] identifier and/or an
      access path of memory cells *)
  type t = {root: Ident.t option; cells: AbstractValue.t list}

  let untracked = {root= None; cells= []}

  let of_place cell = {root= None; cells= [cell]}

  let of_temp id cell = {root= Some id; cells= Option.to_list cell}

  let extend op cell = {op with cells= op.cells @ [cell]}

  let last_cell {cells; _} = List.last cells

  let leaf {root; cells} = match root with None -> List.last cells | Some _ -> None
end

module St = struct
  type t =
    { tag_infos: Tag.Info.t Tag.Map.t
    ; tags_at: Perm.t Tag.Map.t AVMap.t
          (** for each memory cell, the permission each tag has on it *)
    ; parent: Tag.t option Tag.Map.t  (** the borrow tree *)
    ; pointer_tag: Tag.t AVMap.t  (** the tag currently held by each pointer cell *)
    ; temps: Tag.t IdentMap.t  (** the tag carried by loaded temporaries *)
    ; object_root: Tag.t AVMap.t  (** owner tag of each borrowed-from cell *)
    ; local_refs: Tag.Set.t Tag.Map.t
    ; formal_tags: Tag.t Pvar.Map.t  (** the tag at entry for each reference argument *)
    ; entry_pre: (TBSpec.t[@ignore])
    ; access_log: (Access.t * Tag.t * AbstractValue.t * Location.t) list
    ; next_tag: int }
  [@@deriving compare, equal]

  let empty =
    { tag_infos= Tag.Map.empty
    ; tags_at= AVMap.empty
    ; parent= Tag.Map.empty
    ; pointer_tag= AVMap.empty
    ; temps= IdentMap.empty
    ; object_root= AVMap.empty
    ; local_refs= Tag.Map.empty
    ; formal_tags= Pvar.Map.empty
    ; entry_pre= TBSpec.bottom
    ; access_log= []
    ; next_tag= 0 }


  let tag_fresh state ~protector ~borrowed_cell =
    let t = state.next_tag in
    let state =
      { state with
        tag_infos= Tag.Map.add t {Tag.Info.protector; borrowed_cell} state.tag_infos
      ; next_tag= t + 1 }
    in
    (t, state)


  let set_parent state tag p = {state with parent= Tag.Map.add tag p state.parent}

  let parent_of state tag = try Tag.Map.find tag state.parent with Stdlib.Not_found -> None

  let rec is_ancestor state ~ancestor ~descendant =
    Tag.equal ancestor descendant
    ||
    match parent_of state descendant with
    | Some p ->
        is_ancestor state ~ancestor ~descendant:p
    | None ->
        false


  let tag_info_of state tag =
    try Tag.Map.find tag state.tag_infos
    with Stdlib.Not_found -> {Tag.Info.protector= false; borrowed_cell= None}


  let protector_of state tag = (tag_info_of state tag).Tag.Info.protector

  let entries_at state av = try AVMap.find av state.tags_at with Stdlib.Not_found -> Tag.Map.empty

  let set_entry state av tag perm =
    {state with tags_at= AVMap.add av (Tag.Map.add tag perm (entries_at state av)) state.tags_at}


  let adopt_borrowed_cell state tag av =
    let info = tag_info_of state tag in
    match info.Tag.Info.borrowed_cell with
    | Some _ ->
        state
    | None ->
        let state =
          { state with
            tag_infos= Tag.Map.add tag {info with Tag.Info.borrowed_cell= Some av} state.tag_infos
          }
        in
        if Tag.Map.mem tag (entries_at state av) then state
        else set_entry state av tag Perm.Reserved


  let perm_at state tag av = Tag.Map.find_opt tag (entries_at state av)

  let own_perm state tag =
    let info = tag_info_of state tag in
    match
      Option.bind info.Tag.Info.borrowed_cell ~f:(fun av ->
          Tag.Map.find_opt tag (entries_at state av) )
    with
    | Some perm ->
        perm
    | None ->
        Perm.Reserved


  let local_refs_of state tag =
    try Tag.Map.find tag state.local_refs with Stdlib.Not_found -> Tag.Set.empty


  let add_local_ref state ~tag ~local_to =
    let cur = local_refs_of state tag in
    {state with local_refs= Tag.Map.add tag (Tag.Set.add local_to cur) state.local_refs}


  let set_formal_tag state pvar tag =
    {state with formal_tags= Pvar.Map.add pvar tag state.formal_tags}


  let formal_tag_of state pvar = Pvar.Map.find_opt pvar state.formal_tags

  let set_entry_pre state pre = {state with entry_pre= pre}

  let entry_pre_of state = state.entry_pre

  let log_access state access tag ~av ~loc =
    {state with access_log= (access, tag, av, loc) :: state.access_log}


  let global_log state = List.rev state.access_log

  let bind_pointer_tag state av tag = {state with pointer_tag= AVMap.add av tag state.pointer_tag}

  let drop_pointer_tag state av =
    if AVMap.mem av state.pointer_tag then
      {state with pointer_tag= AVMap.remove av state.pointer_tag}
    else state


  let bind_temp state id tag = {state with temps= IdentMap.add id tag state.temps}

  let drop_temp state id =
    if IdentMap.mem id state.temps then {state with temps= IdentMap.remove id state.temps}
    else state


  (** The tag a value carries. For a loaded temporary it is its recorded tag, for a place it is the
      tag held by its leaf cell *)
  let tag_of_operand state (op : Operand.t) : Tag.t option =
    match op.Operand.root with
    | Some id ->
        IdentMap.find_opt id state.temps
    | None ->
        Option.bind (Operand.last_cell op) ~f:(fun leaf -> AVMap.find_opt leaf state.pointer_tag)


  let ensure_object_root state base_av =
    match AVMap.find_opt base_av state.object_root with
    | Some owner ->
        (owner, state)
    | None ->
        let owner, state = tag_fresh state ~protector:false ~borrowed_cell:(Some base_av) in
        let state = set_entry state base_av owner Perm.Unique in
        let state = {state with object_root= AVMap.add base_av owner state.object_root} in
        (owner, state)


  let propagate_entries state ~parent_av ~child_av =
    let pmap = entries_at state parent_av in
    if Tag.Map.is_empty pmap then state
    else
      let cmap = entries_at state child_av in
      let cmap' = Tag.Map.union (fun _ child_perm _parent_perm -> Some child_perm) cmap pmap in
      {state with tags_at= AVMap.add child_av cmap' state.tags_at}


  let propagate_along_path state (access_path : AbstractValue.t list) =
    match access_path with
    | [] | [_] ->
        state
    | first :: rest ->
        List.fold rest ~init:(state, first) ~f:(fun (state, parent_av) child_av ->
            (propagate_entries state ~parent_av ~child_av, child_av) )
        |> fst


  let sub_object_cells state ~succs av =
    let rec go state visited order frontier =
      match frontier with
      | [] ->
          (state, List.rev order)
      | a :: rest ->
          let children = succs a |> List.filter ~f:(fun c -> not (AVSet.mem c visited)) in
          let state =
            List.fold children ~init:state ~f:(fun st c ->
                propagate_entries st ~parent_av:a ~child_av:c )
          in
          let visited = List.fold children ~init:visited ~f:(fun v c -> AVSet.add c v) in
          go state visited (List.rev_append children order) (rest @ children)
    in
    go state (AVSet.singleton av) [av] [av]


  let perm_severity (p : Perm.t) =
    match p with
    | Perm.Reserved ->
        0
    | Perm.ReservedConflicted ->
        1
    | Perm.Unique ->
        2
    | Perm.Frozen ->
        3
    | Perm.Disabled ->
        4


  let perm_join a b = if perm_severity a >= perm_severity b then a else b

  let redirect_tag state ~from ~to_ =
    let sub t = if Tag.equal t from then to_ else t in
    let parent =
      Tag.Map.map
        (function Some p when Tag.equal p from -> Some to_ | other -> other)
        (Tag.Map.remove from state.parent)
    in
    let tags_at =
      AVMap.map
        (fun entries ->
          match Tag.Map.find_opt from entries with
          | None ->
              entries
          | Some p ->
              let entries = Tag.Map.remove from entries in
              Tag.Map.update to_
                (function None -> Some p | Some p0 -> Some (perm_join p0 p))
                entries )
        state.tags_at
    in
    let pointer_tag = AVMap.map sub state.pointer_tag in
    let temps = IdentMap.map sub state.temps in
    let object_root = AVMap.map sub state.object_root in
    let local_refs =
      Tag.Map.fold
        (fun t refs m ->
          let t = sub t in
          let refs = Tag.Set.fold (fun r s -> Tag.Set.add (sub r) s) refs Tag.Set.empty in
          let refs = Tag.Set.remove t refs in
          if Tag.Set.is_empty refs then m
          else
            let prev = try Tag.Map.find t m with Stdlib.Not_found -> Tag.Set.empty in
            Tag.Map.add t (Tag.Set.union prev refs) m )
        state.local_refs Tag.Map.empty
    in
    let formal_tags = Pvar.Map.map sub state.formal_tags in
    let access_log = List.map state.access_log ~f:(fun (a, t, av, l) -> (a, sub t, av, l)) in
    { state with
      parent
    ; tags_at
    ; pointer_tag
    ; temps
    ; object_root
    ; local_refs
    ; formal_tags
    ; access_log }


  let merge_owner_trees state ~survivor ~victim =
    let merged = tag_info_of state survivor in
    let tag_infos = Tag.Map.add survivor merged (Tag.Map.remove victim state.tag_infos) in
    redirect_tag {state with tag_infos} ~from:victim ~to_:survivor


  let canonicalize_owners state ~f =
    let tags_at =
      AVMap.fold
        (fun av entries m ->
          let av' = f av in
          match AVMap.find_opt av' m with
          | None ->
              AVMap.add av' entries m
          | Some entries0 ->
              AVMap.add av' (Tag.Map.union (fun _ p0 p -> Some (perm_join p0 p)) entries0 entries) m )
        state.tags_at AVMap.empty
    in
    let pointer_tag =
      AVMap.fold
        (fun av tag m ->
          let av' = f av in
          match AVMap.find_opt av' m with
          | Some tag0 when not (Tag.equal tag0 tag) ->
              AVMap.remove av' m
          | _ ->
              AVMap.add av' tag m )
        state.pointer_tag AVMap.empty
    in
    let tag_infos =
      Tag.Map.map
        (fun m -> {m with Tag.Info.borrowed_cell= Option.map m.Tag.Info.borrowed_cell ~f})
        state.tag_infos
    in
    let access_log = List.map state.access_log ~f:(fun (a, t, av, l) -> (a, t, f av, l)) in
    let state = {state with tags_at; pointer_tag; tag_infos; access_log} in
    let object_root, merges =
      AVMap.fold
        (fun av tag (m, ms) ->
          let av' = f av in
          match AVMap.find_opt av' m with
          | Some tag0 when not (Tag.equal tag0 tag) ->
              let s, v = if Tag.compare tag0 tag <= 0 then (tag0, tag) else (tag, tag0) in
              (AVMap.add av' s m, (s, v) :: ms)
          | _ ->
              (AVMap.add av' tag m, ms) )
        state.object_root (AVMap.empty, [])
    in
    let state = {state with object_root} in
    let resolve redirects t =
      let rec go t = match Tag.Map.find_opt t redirects with Some t' -> go t' | None -> t in
      go t
    in
    let state, redirects =
      List.fold merges ~init:(state, Tag.Map.empty) ~f:(fun (st, rd) (s, v) ->
          let s = resolve rd s and v = resolve rd v in
          if Tag.equal s v then (st, rd)
          else
            let s, v = if Tag.compare s v <= 0 then (s, v) else (v, s) in
            (merge_owner_trees st ~survivor:s ~victim:v, Tag.Map.add v s rd) )
    in
    {state with object_root= AVMap.map (resolve redirects) state.object_root}
end

type error = {loc: Location.t; description: string} [@@deriving compare, equal]

type state = {st: St.t; errors: error list} [@@deriving compare, equal]

let start () = {st= St.empty; errors= []}

let canonicalize ~f (state : state) : state = {state with st= St.canonicalize_owners state.st ~f}

let initial_perm_of_mut is_mut = if is_mut then Perm.Reserved else Perm.Frozen

let do_reborrow ~(protector : bool) (st : St.t) ~(succs : AbstractValue.t -> AbstractValue.t list)
    ~(bind : AbstractValue.t option) ~(is_mut : bool) ~(src : Operand.t)
    ~(borrowed_cell : AbstractValue.t) : St.t * Tag.t option =
  let st = St.propagate_along_path st src.Operand.cells in
  let parent_opt =
    match src with
    | {Operand.root= Some id} ->
        Option.map (IdentMap.find_opt id st.St.temps) ~f:(fun t -> (t, st))
    | {Operand.root= None; cells= base :: _} ->
        let owner, st = St.ensure_object_root st base in
        Some (owner, st)
    | _ ->
        None
  in
  match parent_opt with
  | None ->
      ((match bind with Some av -> St.drop_pointer_tag st av | None -> st), None)
  | Some (parent_tag, st) ->
      let initial_perm = initial_perm_of_mut is_mut in
      let tag, st = St.tag_fresh st ~protector ~borrowed_cell:(Some borrowed_cell) in
      let st = St.set_parent st tag (Some parent_tag) in
      let st, sub_object = St.sub_object_cells st ~succs borrowed_cell in
      let st = List.fold sub_object ~init:st ~f:(fun st a -> St.set_entry st a tag initial_perm) in
      let st = match bind with Some av -> St.bind_pointer_tag st av tag | None -> st in
      (st, Some tag)


let is_errored (state : state) = not (List.is_empty state.errors)

let add_error (state : state) error = {state with errors= error :: state.errors}

let root_of (st : St.t) tag =
  let rec go t = match St.parent_of st t with Some p -> go p | None -> t in
  go tag


let local_set_of (st : St.t) through =
  let rec chain acc t =
    let acc = t :: acc in
    match St.parent_of st t with Some p -> chain acc p | None -> acc
  in
  List.fold (chain [] through) ~init:Tag.Set.empty ~f:(fun s t ->
      Tag.Set.union (Tag.Set.add t s) (St.local_refs_of st t) )


let fire_at_loc (state : state) ~(loc : Location.t) ~(local_set : Tag.Set.t) ~(through : Tag.t)
    ~(arg_tags : Tag.Set.t) (av : AbstractValue.t) (acc : Access.t) : state =
  if is_errored state then state
  else
    let through_root = root_of state.st through in
    Tag.Map.fold
      (fun t perm state ->
        if is_errored state then state
        else if
          Tag.Set.mem t arg_tags && (not (Tag.equal t through)) && not (St.protector_of state.st t)
        then state
        else
          let rel =
            if Tag.Set.mem t local_set then Rel.Local
            else if Tag.equal (root_of state.st t) through_root then Rel.Foreign
            else Rel.Unrelated
          in
          match rel with
          | Rel.Unrelated ->
              state
          | (Rel.Local | Rel.Foreign) as rel -> (
              let protector = St.protector_of state.st t in
              let fire =
                match rel with Rel.Foreign -> Trans.fire_foreign | _ -> Trans.fire_local
              in
              match fire perm ~protector acc with
              | Ok perm' ->
                  {state with st= St.set_entry state.st av t perm'}
              | Error ub ->
                  add_error state {loc; description= F.asprintf "%a" Ub.pp ub} ) )
      (St.entries_at state.st av) state


let access_through ?(access_path = []) ?(arg_tags = Tag.Set.empty)
    ~(succs : AbstractValue.t -> AbstractValue.t list) (state : state) ~(loc : Location.t)
    ~(through : Tag.t) ~(av : AbstractValue.t) (acc : Access.t) : state =
  if is_errored state then state
  else
    let st = St.propagate_along_path state.st access_path in
    let st = St.adopt_borrowed_cell st through av in
    let st, touched = St.sub_object_cells st ~succs av in
    let local_set = local_set_of st through in
    let state = {state with st} in
    let state =
      List.fold touched ~init:state ~f:(fun state a ->
          fire_at_loc state ~loc ~local_set ~through ~arg_tags a acc )
    in
    if is_errored state then state else {state with st= St.log_access state.st acc through ~av ~loc}


let tag_at_base (st : St.t) base_av =
  match AVMap.find_opt base_av st.St.object_root with
  | Some t ->
      Some t
  | None ->
      AVMap.find_opt base_av st.St.pointer_tag


let resolve_access_target (st : St.t) ~(target : Operand.t) : (Tag.t * AbstractValue.t) option =
  let through =
    match target with
    | {Operand.root= Some id} ->
        IdentMap.find_opt id st.St.temps
    | {Operand.root= None; cells= base :: _} ->
        tag_at_base st base
    | _ ->
        None
  in
  match (through, Operand.last_cell target) with Some t, Some av -> Some (t, av) | _ -> None


let exec_access ~(acc : Access.t) ~(target : Operand.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (state : state) : state =
  match resolve_access_target state.st ~target with
  | None ->
      state
  | Some (through, av) ->
      access_through ~access_path:target.Operand.cells ~succs state ~loc ~through ~av acc


let exec_retag ~(dst : Operand.t) ~(src : Operand.t) ~(is_mut : bool) ~(protected : bool)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (state : state) : state =
  match Operand.last_cell src with
  | None ->
      state
  | Some borrowed_cell -> (
      let st, new_tag =
        do_reborrow ~protector:protected state.st ~succs ~bind:(Operand.leaf dst) ~is_mut ~src
          ~borrowed_cell
      in
      let state = {state with st} in
      match new_tag with
      | Some tag ->
          access_through ~succs state ~loc ~through:tag ~av:borrowed_cell Access.Read
      | None ->
          state )


let classify_typ (typ : Typ.t) =
  match typ.Typ.desc with
  | Tptr (_, _) ->
      let is_mut = not (Typ.is_const typ.Typ.quals) in
      if Typ.is_reference_on_source typ.Typ.quals then `Reference is_mut else `RawPtr is_mut
  | _ ->
      `Other


let initial_perm_of_shape = function
  | `Reference m ->
      initial_perm_of_mut m
  | `RawPtr _ ->
      Perm.Reserved


let exec_load ~(id : Ident.t) ~(typ : Typ.t) ~(src : Operand.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (state : state) : state =
  match classify_typ typ with
  | `Other ->
      exec_access ~acc:Access.Read ~target:src ~succs ~loc state
  | `Reference _ | `RawPtr _ ->
      let state = exec_access ~acc:Access.Read ~target:src ~succs ~loc state in
      if is_errored state then state
      else
        let st =
          match
            Option.bind (Operand.last_cell src) ~f:(fun av ->
                AVMap.find_opt av state.st.St.pointer_tag )
          with
          | Some tag ->
              St.bind_temp state.st id tag
          | None ->
              St.drop_temp state.st id
        in
        {state with st}


let exec_store ~(lhs : Operand.t) ~(rhs : Operand.t) ~(typ : Typ.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (state : state) : state =
  match classify_typ typ with
  | `Other ->
      exec_access ~acc:Access.Write ~target:lhs ~succs ~loc state
  | `Reference _ | `RawPtr _ ->
      let state = exec_access ~acc:Access.Write ~target:lhs ~succs ~loc state in
      if is_errored state then state
      else
        let st =
          match Operand.last_cell lhs with
          | None ->
              state.st
          | Some cell -> (
            match St.tag_of_operand state.st rhs with
            | Some tag ->
                St.bind_pointer_tag state.st cell tag
            | None ->
                St.drop_pointer_tag state.st cell )
        in
        {state with st}


let entry_pre (state : state) = St.entry_pre_of state.st

let rel_to_spec : Rel.t -> TBSpec.Rel.t option = function
  | Rel.Local ->
      Some TBSpec.Rel.Local
  | Rel.Foreign ->
      Some TBSpec.Rel.Foreign
  | Rel.Unrelated ->
      None


let write_cover (st : St.t) ~(tag : Tag.t) ~(perm : Perm.t)
    ~(borrowed_cell : AbstractValue.t option) ~(succs : AbstractValue.t -> AbstractValue.t list) :
    St.t =
  match borrowed_cell with
  | None ->
      st
  | Some borrowed_cell ->
      let st = St.set_entry st borrowed_cell tag perm in
      let st, sub_object = St.sub_object_cells st ~succs borrowed_cell in
      List.fold sub_object ~init:st ~f:(fun st a ->
          if Tag.Map.mem tag (St.entries_at st a) then st else St.set_entry st a tag perm )


let perm_of_formal (tree_borrows : TBSpec.t) i (typ : Typ.t) : Perm.t option =
  match classify_typ typ with
  | (`Reference _ | `RawPtr _) as shape ->
      Some
        ( match
            List.Assoc.find tree_borrows.TBSpec.perms (TBSpec.ArgIndex.of_int i)
              ~equal:TBSpec.ArgIndex.equal
          with
        | Some perm ->
            perm
        | None ->
            initial_perm_of_shape shape )
  | `Other ->
      None


let init_formals (formals : (Pvar.t * Typ.t) list) ~(cell_of : Pvar.t -> AbstractValue.t option)
    ~(borrowed_cell_of : Pvar.t -> AbstractValue.t option) ~(tree_borrows : TBSpec.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) (state : state) : state =
  let rels = tree_borrows.TBSpec.rels in
  let idx = TBSpec.ArgIndex.to_int in
  let local_pair a b =
    List.exists rels ~f:(fun (i, j, r) ->
        Int.equal (idx i) a && Int.equal (idx j) b && TBSpec.Rel.equal r TBSpec.Rel.Local )
  in
  let mutual_local a b = local_pair a b && local_pair b a in
  let any_rel a b =
    List.exists rels ~f:(fun (i, j, _) ->
        (Int.equal (idx i) a && Int.equal (idx j) b) || (Int.equal (idx i) b && Int.equal (idx j) a) )
  in
  let state, _seeded =
    List.foldi formals ~init:(state, []) ~f:(fun i (state, seeded) (pvar, typ) ->
        match classify_typ typ with
        | `Other ->
            (state, seeded)
        | (`Reference _ | `RawPtr _) as shape -> (
            let is_ref = match shape with `Reference _ -> true | `RawPtr _ -> false in
            let record (state : state) tag =
              let st = St.set_formal_tag state.st pvar tag in
              let st =
                match cell_of pvar with Some c -> St.bind_pointer_tag st c tag | None -> st
              in
              ({state with st}, (i, tag) :: seeded)
            in
            match
              List.find_map seeded ~f:(fun (j, tj) -> if mutual_local i j then Some tj else None)
            with
            | Some tj ->
                record state tj
            | None ->
                let ti, st = St.tag_fresh state.st ~protector:is_ref ~borrowed_cell:None in
                let st =
                  match
                    List.find_map seeded ~f:(fun (j, tj) -> if any_rel i j then Some tj else None)
                  with
                  | Some tj ->
                      St.set_parent st ti (Some (root_of st tj))
                  | None ->
                      let owner, st = St.tag_fresh st ~protector:false ~borrowed_cell:None in
                      St.set_parent st ti (Some owner)
                in
                record {state with st} ti ) )
  in
  let formal_tag pvar = St.formal_tag_of state.st pvar in
  let pvar_of k = Option.map (List.nth formals (idx k)) ~f:fst in
  let state =
    List.fold rels ~init:state ~f:(fun state (i, j, rel) ->
        match rel with
        | TBSpec.Rel.Foreign ->
            state
        | TBSpec.Rel.Local -> (
          match (Option.bind (pvar_of i) ~f:formal_tag, Option.bind (pvar_of j) ~f:formal_tag) with
          | Some ti, Some tj when not (Tag.equal ti tj) ->
              {state with st= St.add_local_ref state.st ~tag:ti ~local_to:tj}
          | _ ->
              state ) )
  in
  let state =
    List.foldi formals ~init:state ~f:(fun i (state : state) (pvar, typ) ->
        match (perm_of_formal tree_borrows i typ, St.formal_tag_of state.st pvar) with
        | Some perm, Some tag ->
            let borrowed_cell = borrowed_cell_of pvar in
            let st =
              match borrowed_cell with
              | Some av ->
                  St.adopt_borrowed_cell state.st tag av
              | None ->
                  state.st
            in
            {state with st= write_cover st ~tag ~perm ~borrowed_cell ~succs}
        | _ ->
            state )
  in
  let entry_pre =
    let perms =
      List.filter_mapi formals ~f:(fun i (pvar, _) ->
          Option.map (St.formal_tag_of state.st pvar) ~f:(fun tag ->
              (TBSpec.ArgIndex.of_int i, St.own_perm state.st tag) ) )
    in
    {tree_borrows with TBSpec.perms}
  in
  {state with st= St.set_entry_pre state.st entry_pre}


let rel_of (st : St.t) a b : Rel.t =
  if Tag.equal a b then Rel.Local
  else if not (Tag.equal (root_of st a) (root_of st b)) then Rel.Unrelated
  else if Tag.Set.mem b (local_set_of st a) then Rel.Local
  else Rel.Foreign


let precondition_of_actuals (state : state) (actuals : Operand.t list) : TBSpec.t =
  let indexed =
    List.filter_mapi actuals ~f:(fun i op ->
        Option.map (St.tag_of_operand state.st op) ~f:(fun tag -> (i, tag)) )
  in
  let perms =
    List.map indexed ~f:(fun (i, tag) -> (TBSpec.ArgIndex.of_int i, St.own_perm state.st tag))
  in
  let rels =
    List.concat_map indexed ~f:(fun (i, ti) ->
        List.filter_map indexed ~f:(fun (j, tj) ->
            if Int.equal i j then None
            else
              Option.map
                (rel_to_spec (rel_of state.st ti tj))
                ~f:(fun r -> (TBSpec.ArgIndex.of_int i, TBSpec.ArgIndex.of_int j, r)) ) )
  in
  {TBSpec.perms; rels}


let perm_spec_needed ~(formals : (Pvar.t * Typ.t) list) (tb_pre : TBSpec.t) : bool =
  List.exists tb_pre.TBSpec.perms ~f:(fun (i, perm) ->
      match List.nth formals (TBSpec.ArgIndex.to_int i) with
      | Some (_, ftyp) -> (
        match classify_typ ftyp with
        | (`Reference _ | `RawPtr _) as shape ->
            not (Perm.equal perm (initial_perm_of_shape shape))
        | `Other ->
            false )
      | None ->
          false )


let exec_call ~(callee_state : state) ~(callee_pdesc : Procdesc.t)
    ~(subst : AbstractValue.t -> AbstractValue.t option)
    ~(callee_edges : (AbstractValue.t * AbstractValue.t) list)
    ~(callee_ret_cell : AbstractValue.t option) ~(args : Operand.t list) ~(ret_id : Ident.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (state : state) : state =
  if is_errored state then state
  else
    let callee_st = callee_state.st in
    let formals = Procdesc.get_pvar_formals callee_pdesc in
    let zipped = match List.zip formals args with Ok z -> z | Unequal_lengths -> [] in
    let retag_arg (state : state) parent ~is_ref ~is_mut =
      match (St.tag_info_of state.st parent).Tag.Info.borrowed_cell with
      | None ->
          (state, parent)
      | Some borrowed_cell ->
          let tag, st =
            St.tag_fresh state.st ~protector:is_ref ~borrowed_cell:(Some borrowed_cell)
          in
          let st = St.set_parent st tag (Some parent) in
          let st, sub_object = St.sub_object_cells st ~succs borrowed_cell in
          let st =
            List.fold sub_object ~init:st ~f:(fun st a ->
                St.set_entry st a tag (initial_perm_of_mut is_mut) )
          in
          let state =
            access_through ~succs {state with st} ~loc ~through:tag ~av:borrowed_cell Access.Read
          in
          (state, tag)
    in
    let state, formal_to_caller =
      List.fold zipped ~init:(state, []) ~f:(fun (state, acc) ((pvar, ftyp), op) ->
          let shape = classify_typ ftyp in
          match (shape, St.formal_tag_of callee_st pvar, St.tag_of_operand state.st op) with
          | (`Reference is_mut | `RawPtr is_mut), Some ftag, Some parent ->
              let is_ref =
                (match shape with `Reference _ -> true | `RawPtr _ | `Other -> false)
                && match ftyp.Typ.desc with Tptr ({desc= Tvoid}, _) -> false | _ -> true
              in
              let state, ctag = retag_arg state parent ~is_ref ~is_mut in
              (state, (ftag, ctag) :: acc)
          | _ ->
              (state, acc) )
    in
    let formal_to_caller = List.rev formal_to_caller in
    let arg_tags =
      List.fold formal_to_caller ~init:Tag.Set.empty ~f:(fun s (_, ct) -> Tag.Set.add ct s)
    in
    let deepest_formal_ancestor callee_tag =
      formal_to_caller
      |> List.filter ~f:(fun (ftag, _) ->
          St.is_ancestor callee_st ~ancestor:ftag ~descendant:callee_tag )
      |> List.max_elt ~compare:(fun (t1, _) (t2, _) ->
          if Tag.equal t1 t2 then 0
          else if St.is_ancestor callee_st ~ancestor:t2 ~descendant:t1 then 1
          else -1 )
    in
    let route callee_tag = Option.map (deepest_formal_ancestor callee_tag) ~f:snd in
    let perm_of ct =
      match St.own_perm callee_st ct with Perm.ReservedConflicted -> Perm.Reserved | p -> p
    in
    let borrowed_cell_of ct =
      Option.bind (St.tag_info_of callee_st ct).Tag.Info.borrowed_cell ~f:subst
    in
    let materialize_chain st ~from_formal ~to_tag ~root =
      let rec collect ct acc =
        if Tag.equal ct from_formal then acc
        else
          match St.parent_of callee_st ct with Some p -> collect p (ct :: acc) | None -> ct :: acc
      in
      List.fold (collect to_tag []) ~init:(st, root) ~f:(fun (st, parent) ct ->
          let borrowed_cell = borrowed_cell_of ct in
          let t_c, st = St.tag_fresh st ~protector:false ~borrowed_cell in
          let st = St.set_parent st t_c (Some parent) in
          let st =
            match borrowed_cell with Some av -> St.set_entry st av t_c (perm_of ct) | None -> st
          in
          (st, t_c) )
    in
    let state =
      let st =
        List.fold callee_edges ~init:state.st ~f:(fun st (a, b) ->
            match (subst a, subst b) with
            | Some a', Some b' ->
                St.propagate_entries st ~parent_av:a' ~child_av:b'
            | _ ->
                st )
      in
      {state with st}
    in
    let state =
      List.fold (St.global_log callee_st) ~init:state
        ~f:(fun (state : state) (access, callee_t, callee_av, callee_loc) ->
          if is_errored state then state
          else
            match (route callee_t, subst callee_av) with
            | Some caller_tag, Some av ->
                let loc = if Location.equal callee_loc Location.dummy then loc else callee_loc in
                access_through ~arg_tags ~succs state ~loc ~through:caller_tag ~av access
            | _ ->
                state )
    in
    let state =
      List.fold formal_to_caller ~init:state ~f:(fun (state : state) (ftag, ctag) ->
          if is_errored state then state
          else
            let st = state.st in
            let releasing = St.protector_of st ctag in
            let unconflict (p : Perm.t) =
              if releasing then match p with Perm.ReservedConflicted -> Perm.Reserved | p -> p
              else p
            in
            let st =
              if releasing then
                { st with
                  St.tags_at= AVMap.map (fun entries -> Tag.Map.remove ctag entries) st.St.tags_at
                }
              else st
            in
            let st =
              AVMap.fold
                (fun callee_av entries st ->
                  match (Tag.Map.find_opt ftag entries, subst callee_av) with
                  | Some perm, Some av ->
                      let perm =
                        if releasing then unconflict perm
                        else
                          match St.perm_at st ctag av with
                          | Some p0 ->
                              St.perm_join p0 perm
                          | None ->
                              perm
                      in
                      St.set_entry st av ctag perm
                  | _ ->
                      st )
                callee_st.St.tags_at st
            in
            let info = St.tag_info_of st ctag in
            let st =
              { st with
                St.tag_infos=
                  Tag.Map.add ctag
                    { info with
                      Tag.Info.protector= (if releasing then false else info.Tag.Info.protector) }
                    st.St.tag_infos }
            in
            {state with st} )
    in
    let state =
      List.fold formal_to_caller ~init:state ~f:(fun (state : state) (ftag, _) ->
          if is_errored state then state
          else
            match
              Option.bind (St.tag_info_of callee_st ftag).Tag.Info.borrowed_cell ~f:(fun pointee ->
                  Option.both (AVMap.find_opt pointee callee_st.St.pointer_tag) (Some pointee) )
            with
            | None ->
                state
            | Some (escaping_tag, callee_pointee) -> (
              match (deepest_formal_ancestor escaping_tag, subst callee_pointee) with
              | Some (formal_anc, caller_anc), Some caller_pointee
                when not (Tag.equal formal_anc ftag) ->
                  let st, last_tag =
                    materialize_chain state.st ~from_formal:formal_anc ~to_tag:escaping_tag
                      ~root:caller_anc
                  in
                  {state with st= St.bind_pointer_tag st caller_pointee last_tag}
              | _ ->
                  state ) )
    in
    if is_errored state then state
    else
      match
        Option.bind callee_ret_cell ~f:(fun av -> AVMap.find_opt av callee_st.St.pointer_tag)
      with
      | None ->
          state
      | Some callee_ret_tag -> (
        match deepest_formal_ancestor callee_ret_tag with
        | None ->
            state
        | Some (formal_tag, caller_arg_tag) ->
            let st, last_tag =
              materialize_chain state.st ~from_formal:formal_tag ~to_tag:callee_ret_tag
                ~root:caller_arg_tag
            in
            {state with st= St.bind_temp st ret_id last_tag} )


let report_errors proc_desc err_log (state : state) : unit =
  List.iter (List.rev state.errors) ~f:(fun {loc; description} ->
      Reporting.log_issue proc_desc err_log ~loc Checker.TreeBorrows IssueType.tree_borrows_ub
        description )


let pp fmt ({st; errors= _} : state) =
  if Int.equal st.St.next_tag 0 then F.pp_print_string fmt "()"
  else
    let pp_parent fmt (tag, parent_opt) =
      match parent_opt with
      | Some parent_tag ->
          F.fprintf fmt "%a<-%a" Tag.pp tag Tag.pp parent_tag
      | None ->
          Tag.pp fmt tag
    in
    let pp_cell fmt (av, perms) =
      F.fprintf fmt "%a: {%a}" AbstractValue.pp av
        (Pp.comma_seq (fun fmt (tag, perm) -> F.fprintf fmt "%a: %a" Tag.pp tag Perm.pp perm))
        (Tag.Map.bindings perms)
    in
    let pp_ptr fmt (av, tag) = F.fprintf fmt "%a: %a" AbstractValue.pp av Tag.pp tag in
    F.fprintf fmt "@[{tree= [%a];@ roots= [%a];@ cells= [%a];@ ptrs= [%a]}@]"
      (Pp.comma_seq pp_parent) (Tag.Map.bindings st.St.parent) (Pp.comma_seq pp_ptr)
      (AVMap.bindings st.St.object_root)
      (Pp.comma_seq pp_cell) (AVMap.bindings st.St.tags_at) (Pp.comma_seq pp_ptr)
      (AVMap.bindings st.St.pointer_tag)
