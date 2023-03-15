(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Partition = struct
  type 'node t =
    | Empty
    | Node of {node: 'node; next: 'node t}
    | Component of {head: 'node; rest: 'node t; next: 'node t}

  let empty = Empty

  let add_node node next = Node {node; next}

  let prepend_node next node = Node {node; next}

  let add_component head rest next = Component {head; rest; next}

  let rec fold_nodes partition ~init ~f =
    match partition with
    | Empty ->
        init
    | Node {node; next} ->
        let init = f init node in
        (fold_nodes [@tailcall]) next ~init ~f
    | Component {head; rest; next} ->
        let init = f init head in
        let init = fold_nodes rest ~init ~f in
        (fold_nodes [@tailcall]) next ~init ~f


  let iter_nodes partition ~f = Container.iter partition ~fold:fold_nodes ~f

  let rec fold_heads partition ~init ~f =
    match partition with
    | Empty ->
        init
    | Node {next} ->
        (fold_heads [@tailcall]) next ~init ~f
    | Component {head; rest; next} ->
        let init = f init head in
        let init = fold_heads rest ~init ~f in
        (fold_heads [@tailcall]) next ~init ~f


  let expand ~fold_right partition =
    let rec expand_aux ~cb = function
      | Empty ->
          cb Empty
      | Node {node; next} ->
          (expand_aux [@tailcall]) next ~cb:(fun init ->
              fold_right node ~init ~f:prepend_node |> cb )
      | Component {head; rest; next} ->
          (expand_aux [@tailcall]) next ~cb:(fun next ->
              (expand_aux [@tailcall]) rest ~cb:(fun init ->
                  match fold_right head ~init ~f:prepend_node with
                  | Empty | Component _ ->
                      (* [fold_right] is expected to always provide a non-empty sequence.  Hence the
                         result of [fold_right ~f:prepend_node] will always start with a Node. *)
                      Logging.(die InternalError)
                        "WeakTopologicalOrder.Partition.expand: the expansion function fold_right \
                         should not return ~init directly"
                  | Node {node= head; next= rest} ->
                      cb (Component {head; rest; next}) ) )
    in
    expand_aux ~cb:Fn.id partition


  let rec pp ~prefix ~pp_node fmt = function
    | Empty ->
        ()
    | Node {node; next} ->
        F.fprintf fmt "%s%a" prefix pp_node node ;
        pp ~prefix:" " ~pp_node fmt next
    | Component {head; rest; next} ->
        F.fprintf fmt "%s(%a%a)" prefix pp_node head (pp ~prefix:" " ~pp_node) rest ;
        pp ~prefix:" " ~pp_node fmt next


  let pp ~pp_node = pp ~prefix:"" ~pp_node
end

module type PreProcCfg = sig
  module Node : sig
    type t

    type id

    val id : t -> id

    module IdMap : PrettyPrintable.PPMap with type key = id
  end

  type t

  val fold_succs : t -> (Node.t, Node.t, 'accum) Container.fold

  val start_node : t -> Node.t
end

module type S = sig
  module CFG : PreProcCfg

  val make : CFG.t -> CFG.Node.t Partition.t
end

module type Make = functor (CFG : PreProcCfg) -> S with module CFG = CFG

module Bourdoncle_SCC (CFG : PreProcCfg) = struct
  module CFG = CFG

  (** [dfn] contains a DFS pre-order indexing. A node is not in the map if it has never been
      visited. A node's dfn is +oo if it has been fully visited (head of cross-edges) or we want to
      hide it for building a subcomponent partition (head of highest back-edges). *)
  module Dfn = CFG.Node.IdMap

  (*
    Unlike Bourdoncle's paper version or OCamlGraph implementation, this implementation handles
    high DFS-depth graphs, which would stack-overflow otherwise.
    It still doesn't handle high component nesting, but it is pretty unlikely to happen in real
    code (means a lot of loop nesting).
  *)

  type stack =
    { node: CFG.Node.t
    ; node_id: CFG.Node.id
    ; node_dfn: int
    ; succs: CFG.Node.t list
    ; mutable succs_to_visit: CFG.Node.t list
    ; mutable head: int  (** Minimum [dfn] of the nodes accessibles from [node]. *)
    ; mutable component: CFG.Node.id ARList.t
          (** Nodes in the current strict-connected component. *)
    ; mutable building_component: bool
    ; next: stack option }

  let make cfg =
    let num = ref 0 in
    let dfn = ref Dfn.empty in
    let stack = ref None in
    let push_on_stack node =
      let node_id = CFG.Node.id node in
      incr num ;
      let node_dfn = !num in
      dfn := Dfn.add node_id node_dfn !dfn ;
      let succs = IContainer.to_rev_list ~fold:(CFG.fold_succs cfg) node in
      stack :=
        Some
          { node
          ; node_id
          ; node_dfn
          ; succs
          ; succs_to_visit= succs
          ; head= Int.max_value
          ; component= ARList.empty
          ; building_component= false
          ; next= !stack }
    in
    let record_head ?add_to_component cur_head =
      let stack_top = Option.value_exn !stack in
      stack_top.head <- min stack_top.head cur_head ;
      Option.iter add_to_component ~f:(fun add ->
          stack_top.component <- ARList.append add stack_top.component )
    in
    let visit node =
      let node_id = CFG.Node.id node in
      match Dfn.find node_id !dfn with
      | node_dfn ->
          (*
            [node_dfn] is going to be either +oo (see [Dfn] for why), in which case [record_head]
            will have no effect; or be the [dfn] of the head of a back-edge or cross-edge in the
            current strictly connected component.
          *)
          record_head node_dfn
      | exception (Not_found_s _ | Caml.Not_found) ->
          push_on_stack node
    in
    let rec process_stack partition =
      match !stack with
      | None ->
          ()
      | Some ({succs_to_visit= succ :: succs_to_visit} as stack_top) ->
          stack_top.succs_to_visit <- succs_to_visit ;
          visit succ ;
          (process_stack [@tailcall]) partition
      | Some {succs_to_visit= []; building_component= true} ->
          ()
      | Some
          {succs_to_visit= []; building_component= false; node_id; node_dfn; head; component; next}
        when head < node_dfn ->
          (* [node] is in a strictly connected component but is (locally) not its head. *)
          stack := next ;
          record_head head ~add_to_component:(ARList.cons node_id component) ;
          (process_stack [@tailcall]) partition
      | Some
          ( { succs_to_visit= []
            ; building_component= false
            ; node
            ; node_id
            ; node_dfn
            ; succs
            ; head
            ; component
            ; next } as stack_top ) ->
          dfn := Dfn.add node_id Int.max_value !dfn ;
          if head > node_dfn then
            (* [node] is not (locally) in a strictly connected component *)
            partition := Partition.add_node node !partition
          else (
            (*
              head = node_dfn. [node] is (locally) the head of a strictly connected component.
              [node] is marked as already visited (line dfn := ... above).
              All nodes in the current [component] are marked as not visited.
              And we recursively construct a WTO for the component.
            *)
            Container.iter component ~fold:ARList.fold_unordered ~f:(fun nid ->
                dfn := Dfn.remove nid !dfn ) ;
            let component_partition =
              let partition = ref Partition.empty in
              stack_top.building_component <- true ;
              stack_top.succs_to_visit <- succs ;
              process_stack partition ;
              !partition
            in
            partition := Partition.add_component node component_partition !partition ) ;
          stack := next ;
          (process_stack [@tailcall]) partition
    in
    let partition = ref Partition.empty in
    push_on_stack (CFG.start_node cfg) ;
    process_stack partition ;
    !partition
end
