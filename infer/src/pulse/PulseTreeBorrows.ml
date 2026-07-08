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

module Perm = struct
  type t = Reserved | Unique | Frozen | Disabled | ReservedConflicted [@@deriving compare, equal]

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

module Access = struct
  type t = Read | Write
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
    ; next_tag: int }
  [@@deriving compare, equal]

  let empty =
    { tag_infos= Tag.Map.empty
    ; tags_at= AVMap.empty
    ; parent= Tag.Map.empty
    ; pointer_tag= AVMap.empty
    ; temps= IdentMap.empty
    ; object_root= AVMap.empty
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
end

type error = {loc: Location.t; description: string} [@@deriving compare, equal]

type state = {st: St.t; errors: error list} [@@deriving compare, equal]

let start () = {st= St.empty; errors= []}

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
      let initial_perm = if is_mut then Perm.Reserved else Perm.Frozen in
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
    let acc = Tag.Set.add t acc in
    match St.parent_of st t with Some p -> chain acc p | None -> acc
  in
  chain Tag.Set.empty through


let fire_at_loc (state : state) ~(loc : Location.t) ~(local_set : Tag.Set.t) ~(through : Tag.t)
    (av : AbstractValue.t) (acc : Access.t) : state =
  if is_errored state then state
  else
    let through_root = root_of state.st through in
    Tag.Map.fold
      (fun t perm state ->
        if is_errored state then state
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


let access_through ?(access_path = []) ~(succs : AbstractValue.t -> AbstractValue.t list)
    (state : state) ~(loc : Location.t) ~(through : Tag.t) ~(av : AbstractValue.t) (acc : Access.t)
    : state =
  if is_errored state then state
  else
    let st = St.propagate_along_path state.st access_path in
    let st = St.adopt_borrowed_cell st through av in
    let st, touched = St.sub_object_cells st ~succs av in
    let local_set = local_set_of st through in
    let state = {state with st} in
    List.fold touched ~init:state ~f:(fun state a ->
        fire_at_loc state ~loc ~local_set ~through a acc )


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


let classify_typ (typ : Typ.t) = match typ.desc with Tptr (_, _) -> `Pointer | _ -> `Other

let exec_load ~(id : Ident.t) ~(typ : Typ.t) ~(src : Operand.t)
    ~(succs : AbstractValue.t -> AbstractValue.t list) ~(loc : Location.t) (state : state) : state =
  match classify_typ typ with
  | `Other ->
      exec_access ~acc:Access.Read ~target:src ~succs ~loc state
  | `Pointer ->
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
  | `Pointer ->
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
