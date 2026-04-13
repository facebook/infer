(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
(*
   Proof-producing Congruence Closure
   Robert Nieuwenhuis and Albert Oliveras
   https://www.cs.upc.edu/~oliveras/rta05.pdf

   We do not implement the proof producing part. Their approach is
   nice because we only consider binary terms [App (x,y)] instead of terms
   with arbitrary headers and arity.
*)

type value = string option [@@deriving equal, hash]

module Atom : sig
  type t = private {index: int; value: value} [@@deriving equal, compare, hash]

  val pp : F.formatter -> t -> unit

  type state

  val init : unit -> state

  val mk : state -> string -> t * bool

  val mk_fresh : state -> t

  val cardinal : state -> int

  val fold : state -> init:'a -> f:(t -> 'a -> 'a) -> 'a

  module Set : Stdlib.Set.S with type elt = t
end = struct
  type t = {index: int; value: value [@ignore]} [@@deriving compare, hash]

  let equal a1 a2 = phys_equal a1 a2

  let pp fmt {index; value} =
    match value with None -> F.fprintf fmt "%%%d" index | Some value -> F.fprintf fmt "%s" value


  module HashconsSet = Weak.Make (struct
    type nonrec t = t

    let equal atom1 atom2 = equal_value atom1.value atom2.value

    let hash {value} = hash_value value
  end)

  type state = {hashconsed: HashconsSet.t; mutable fresh: int}

  let init () = {hashconsed= HashconsSet.create 1024; fresh= 0}

  let fold {hashconsed} ~init ~f = HashconsSet.fold f hashconsed init

  let cardinal {fresh} = fresh

  let mk h string =
    let atom0 = {index= h.fresh; value= Some string} in
    let atom = HashconsSet.merge h.hashconsed atom0 in
    let is_new = phys_equal atom atom0 in
    if is_new then h.fresh <- h.fresh + 1 ;
    (atom, is_new)


  let mk_fresh h =
    let atom = {index= h.fresh; value= None} in
    h.fresh <- h.fresh + 1 ;
    atom


  module Set = Stdlib.Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

type enode = {head: Atom.t; children: Atom.t list} [@@deriving equal, hash]

type atom_equation = {rhs: Atom.t; lhs: Atom.t}

let pp_atom_equation fmt {rhs; lhs} = F.fprintf fmt "%a=%a" Atom.pp lhs Atom.pp rhs

type app_equation = {atom: Atom.t; enode: enode}

let pp_enode fmt {head; children} =
  F.fprintf fmt "%a(%a)" Atom.pp head (Pp.comma_seq Atom.pp) children


let pp_app_equation fmt {atom; enode} = F.fprintf fmt "%a=%a" pp_enode enode Atom.pp atom

type pending_item = PendingAtom of atom_equation | PendingApp of app_equation * app_equation

let pp_pending_item fmt item =
  match item with
  | PendingAtom eq ->
      pp_atom_equation fmt eq
  | PendingApp (eq1, eq2) ->
      F.fprintf fmt "%a, %a" pp_app_equation eq1 pp_app_equation eq2


let pending_headers = function
  | PendingAtom {rhs; lhs} ->
      (rhs, lhs)
  | PendingApp ({atom= atom1}, {atom= atom2}) ->
      (atom1, atom2)


module LookupTbl = Stdlib.Hashtbl.Make (struct
  type t = enode [@@deriving equal, hash]
end)

module Class : sig
  type t

  val of_atom : Atom.t -> t

  val merge : t -> t -> t

  val fold : t -> init:'a -> f:('a -> Atom.t -> 'a) -> 'a
end = struct
  (* represent a list of atoms without concatening them *)
  type t = Atom of Atom.t | Merge of t * t

  let of_atom atom = Atom atom

  let merge c1 c2 =
    (* we assume the input classes are disjoint *)
    Merge (c1, c2)


  let rec fold c ~init ~f =
    match c with
    | Atom a ->
        f init a
    | Merge (c1, c2) ->
        let init = fold c1 ~init ~f in
        fold c2 ~init ~f
end

type header = Atom.t

module HeaderSet = Stdlib.Set.Make (struct
  type t = Atom.t * int [@@deriving compare]
end)

type t =
  { repr: Atom.t Dynarray.t
  ; mutable pending: pending_item list
  ; atoms: Atom.t Dynarray.t
  ; classes: Class.t Dynarray.t
  ; parents: app_equation list Dynarray.t
  ; lookup: app_equation LookupTbl.t
  ; input_app_equations: enode option Dynarray.t
  ; term_roots: Atom.Set.t Dynarray.t
  ; mutable app_roots: Atom.Set.t
  ; mutable app_right_neutral: Atom.t option
  ; mutable diff: (header * Atom.t) option (* diff_header, resolved *)
  ; hashcons: Atom.state
  ; mutable update_count: int
  ; mutable headers_with_arity: HeaderSet.t
  ; debug: bool }

let init ~debug =
  { repr= Dynarray.create ()
  ; pending= []
  ; atoms= Dynarray.create ()
  ; classes= Dynarray.create ()
  ; parents= Dynarray.create ()
  ; lookup= LookupTbl.create 32
  ; input_app_equations= Dynarray.create ()
  ; term_roots= Dynarray.create ()
  ; app_roots= Atom.Set.empty
  ; app_right_neutral= None
  ; diff= None
  ; update_count= 0
  ; hashcons= Atom.init ()
  ; headers_with_arity= HeaderSet.empty
  ; debug }


let set_app_right_neutral state atom = state.app_right_neutral <- Some atom

let set_diff state ~diff_header ~resolved = state.diff <- Some (diff_header, resolved)

let get_diff state = state.diff

let reset_update_count state = state.update_count <- 0

let get_update_count state = state.update_count

let incr_update_count state = state.update_count <- state.update_count + 1

let rec representative state atom =
  (* TODO: path compression *)
  let parent = Dynarray.get state.repr atom.Atom.index in
  if phys_equal parent atom then atom else representative state parent


let is_equiv state atom1 atom2 =
  let atom1' = representative state atom1 in
  let atom2' = representative state atom2 in
  phys_equal atom1' atom2'


let is_app_right_neutral state atom =
  Option.exists state.app_right_neutral ~f:(fun atom_neutral -> is_equiv state atom_neutral atom)


let app_right_neutral_exists state = Option.is_some state.app_right_neutral

let rec depth state atom =
  let parent = Dynarray.get state.repr atom.Atom.index in
  if phys_equal parent atom then 0 else 1 + depth state parent


let mk_atom state value =
  let atom, is_new = Atom.mk state.hashcons value in
  if is_new then (
    Dynarray.add_last state.repr atom ;
    Dynarray.add_last state.atoms atom ;
    Dynarray.add_last state.classes (Class.of_atom atom) ;
    Dynarray.add_last state.parents [] ;
    Dynarray.add_last state.input_app_equations None ;
    Dynarray.add_last state.term_roots Atom.Set.empty ) ;
  atom


let mk_fresh_atom state =
  let atom = Atom.mk_fresh state.hashcons in
  Dynarray.add_last state.repr atom ;
  Dynarray.add_last state.atoms atom ;
  Dynarray.add_last state.classes (Class.of_atom atom) ;
  Dynarray.add_last state.parents [] ;
  Dynarray.add_last state.input_app_equations None ;
  Dynarray.add_last state.term_roots Atom.Set.empty ;
  atom


let get_parents {parents} {Atom.index} = Dynarray.get parents index

let flush_parents {parents; debug} ({Atom.index} as atom) =
  if debug then F.printf "parents[%a] <- []\n" Atom.pp atom ;
  Dynarray.set parents index []


let set_parents {parents; debug} ({Atom.index} as atom) l =
  if debug then F.printf "parents[%a] <- [%a]\n" Atom.pp atom (Pp.semicolon_seq pp_app_equation) l ;
  Dynarray.set parents index l


let get_input_app_equation {input_app_equations} atom =
  Dynarray.get input_app_equations atom.Atom.index


let get_enode = get_input_app_equation

let set_input_app_equation {input_app_equations} atom pair =
  Dynarray.set input_app_equations atom.Atom.index (Some pair)


let equiv_atoms state {Atom.index} =
  Dynarray.get state.classes index |> Class.fold ~init:[] ~f:(fun l a -> a :: l)


let equiv_terms state {Atom.index} =
  Dynarray.get state.classes index
  |> Class.fold ~init:[] ~f:(fun l atom ->
         match Dynarray.get state.input_app_equations atom.Atom.index with
         | None ->
             l
         | Some enode ->
             {atom; enode} :: l )


let add_parents state atom app_equation =
  set_parents state atom (app_equation :: get_parents state atom)


let lookup state key = LookupTbl.find_opt state.lookup key

let set_lookup {lookup; debug} key eq =
  if debug then
    F.printf "Lookup(%a,%a) <- %a\n" Atom.pp key.head (Pp.comma_seq Atom.pp) key.children
      pp_app_equation eq ;
  LookupTbl.add lookup key eq


type term = Enode of enode | Atom of Atom.t

let pp_term fmt term = match term with Atom a -> Atom.pp fmt a | Enode enode -> pp_enode fmt enode

let enode_representative state {head; children} =
  let head = representative state head in
  (* TODO: probably useless *)
  let children = List.map ~f:(representative state) children in
  {head; children}


let rec merge state atom term =
  if state.debug then F.printf "MERGE %a=%a\n" pp_term term Atom.pp atom ;
  match term with
  | Atom lhs ->
      state.pending <- PendingAtom {rhs= atom; lhs} :: state.pending ;
      propagate state
  | Enode enode -> (
      (* TODO: add track sizes and decide new representative with them *)
      let ({children= children'} as key) = enode_representative state enode in
      match lookup state key with
      | Some app_eq_b ->
          state.pending <- PendingApp ({atom; enode}, app_eq_b) :: state.pending ;
          propagate state
      | None ->
          let app_equation = {atom; enode} in
          set_lookup state key app_equation ;
          add_parents state key.head app_equation ;
          List.iter children' ~f:(fun child -> add_parents state child app_equation) )


and propagate state =
  match state.pending with
  | [] ->
      ()
  | item :: pending ->
      state.pending <- pending ;
      if state.debug then F.printf "PROPAGATE %a\n" pp_pending_item item ;
      let rhs, lhs = pending_headers item in
      let atom_a' = representative state rhs in
      let lhs' = representative state lhs in
      if not (phys_equal atom_a' lhs') then change_representative state atom_a' lhs' ;
      propagate state


and change_representative ({debug; pending; repr; classes; term_roots} as state)
    ({Atom.index= old_index} as old_repr) ({Atom.index= new_index} as new_repr) =
  if debug then F.printf "repr[%a] <- %a\n" Atom.pp old_repr Atom.pp new_repr ;
  incr_update_count state ;
  Dynarray.set repr old_index new_repr ;
  let old_class = Dynarray.get classes old_index in
  let new_class = Dynarray.get classes new_index in
  Dynarray.set classes new_index (Class.merge new_class old_class) ;
  Dynarray.set term_roots new_index
    (Atom.Set.union (Dynarray.get term_roots new_index) (Dynarray.get term_roots old_index)) ;
  if Atom.Set.mem old_repr state.app_roots then
    state.app_roots <- Atom.Set.remove old_repr state.app_roots |> Atom.Set.add new_repr ;
  let parents_new_repr =
    get_parents state old_repr
    |> List.fold
         ~f:(fun eqs ({enode} as app_eq_c) ->
           let key = enode_representative state enode in
           if debug then F.printf "Lookup(%a) ?\n" pp_enode enode ;
           match lookup state key with
           | Some app_eq_d ->
               state.pending <- PendingApp (app_eq_c, app_eq_d) :: pending ;
               eqs
           | None ->
               set_lookup state key app_eq_c ;
               app_eq_c :: eqs )
         ~init:(get_parents state new_repr)
  in
  flush_parents state old_repr ;
  set_parents state new_repr parents_new_repr


let add_enode state enode =
  let enode' = enode_representative state enode in
  match lookup state enode' with
  | Some {atom} ->
      representative state atom
  | None ->
      let term = Enode enode in
      let atom = mk_fresh_atom state in
      merge state atom term ;
      set_input_app_equation state atom enode ;
      state.app_roots <- Atom.Set.add atom state.app_roots ;
      atom


let add_term_root state ~header:{Atom.index} ~term =
  let roots = Dynarray.get state.term_roots index in
  Dynarray.set state.term_roots index (Atom.Set.add term roots)


let fold_term_roots state {Atom.index} ~f ~init =
  let roots = Dynarray.get state.term_roots index in
  Atom.Set.fold
    (fun atom acc -> if phys_equal (representative state atom) atom then f atom acc else acc)
    roots init


let iter_term_roots state atom ~f = fold_term_roots state atom ~init:() ~f:(fun atom () -> f atom)

let iter_app_roots state ~f = Atom.Set.iter f state.app_roots

let mk_header = mk_atom

let pp_header = Atom.pp

let representative_of_header = representative

let unsafe_header_of_atom atom = atom

let mk_term state head children =
  match children with
  | [] ->
      let term = (head :> Atom.t) in
      add_term_root state ~header:head ~term ;
      state.headers_with_arity <- HeaderSet.add (head, 0) state.headers_with_arity ;
      term
  | _ ->
      let term = add_enode state {head; children} in
      add_term_root state ~header:head ~term ;
      state.headers_with_arity <-
        HeaderSet.add (head, List.length children) state.headers_with_arity ;
      term


let headers_with_arity state = HeaderSet.elements state.headers_with_arity

let show_stats state =
  let size = Atom.cardinal state.hashcons in
  let max_depth = Atom.fold state.hashcons ~init:0 ~f:(fun atom -> max (depth state atom)) in
  F.printf "size=%d\nmax_depth=%d\n" size max_depth


let pp_nested_term ?(depth = 1 lsl 5) state fmt atom =
  let rec pp depth fmt atom =
    if Int.equal depth 0 then F.pp_print_string fmt "..."
    else
      match get_input_app_equation state atom with
      | Some {head; children= []} ->
          (pp depth) fmt head
      | Some {head; children} ->
          F.fprintf fmt "@[<hv4>(%a" (pp depth) head ;
          List.iter children ~f:(fun child -> F.fprintf fmt "@ %a" (pp (depth - 1)) child) ;
          F.fprintf fmt ")@]"
      | None ->
          Atom.pp fmt atom
  in
  pp depth fmt atom


let pp_atom_set state fmt set =
  F.fprintf fmt "{%a}" (Pp.comma_seq (pp_nested_term state)) (Atom.Set.elements set)


let debug state =
  let pp = pp_nested_term state in
  F.printf "repr: @[<hv>" ;
  Dynarray.iteri
    (fun index atom ->
      let repr = Dynarray.get state.repr index in
      if index > 0 then F.printf "@ " ;
      F.printf "%a is %a (repr=%a)" pp atom pp atom pp repr ;
      let set = Dynarray.get state.term_roots index in
      if not (Atom.Set.is_empty set) then F.printf " (roots=%a)" (pp_atom_set state) set )
    state.atoms ;
  F.printf "@]@."
