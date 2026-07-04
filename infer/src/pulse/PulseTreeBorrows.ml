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

  let entries_at state av = try AVMap.find av state.tags_at with Stdlib.Not_found -> Tag.Map.empty

  let set_entry state av tag perm =
    {state with tags_at= AVMap.add av (Tag.Map.add tag perm (entries_at state av)) state.tags_at}


  let bind_pointer_tag state av tag = {state with pointer_tag= AVMap.add av tag state.pointer_tag}

  let drop_pointer_tag state av =
    if AVMap.mem av state.pointer_tag then
      {state with pointer_tag= AVMap.remove av state.pointer_tag}
    else state


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

type state = St.t [@@deriving compare, equal]

let start () = St.empty

let do_reborrow ~(protector : bool) (st : St.t) ~(succs : AbstractValue.t -> AbstractValue.t list)
    ~(bind : AbstractValue.t option) ~(is_mut : bool) ~(src : Operand.t)
    ~(borrowed_cell : AbstractValue.t) : St.t =
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
  | None -> (
    match bind with Some av -> St.drop_pointer_tag st av | None -> st )
  | Some (parent_tag, st) -> (
      let initial_perm = if is_mut then Perm.Reserved else Perm.Frozen in
      let tag, st = St.tag_fresh st ~protector ~borrowed_cell:(Some borrowed_cell) in
      let st = St.set_parent st tag (Some parent_tag) in
      let st, sub_object = St.sub_object_cells st ~succs borrowed_cell in
      let st = List.fold sub_object ~init:st ~f:(fun st a -> St.set_entry st a tag initial_perm) in
      match bind with Some av -> St.bind_pointer_tag st av tag | None -> st )


let exec_retag ~(dst : Operand.t) ~(src : Operand.t) ~(is_mut : bool) ~(protected : bool)
    ~(succs : AbstractValue.t -> AbstractValue.t list) (st : state) : state =
  match List.last src.Operand.cells with
  | None ->
      st
  | Some borrowed_cell ->
      do_reborrow ~protector:protected st ~succs ~bind:(Operand.leaf dst) ~is_mut ~src
        ~borrowed_cell


let tag_of_pointer av (st : state) = AVMap.find_opt av st.St.pointer_tag

let parent_of tag (st : state) = Option.join (Tag.Map.find_opt tag st.St.parent)

let perm_at ~cell tag (st : state) =
  Option.bind (AVMap.find_opt cell st.St.tags_at) ~f:(Tag.Map.find_opt tag)


let pp fmt (st : state) =
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
