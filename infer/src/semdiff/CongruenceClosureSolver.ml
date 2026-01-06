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
  type t = private {index: int; value: value} [@@deriving compare]

  val pp : F.formatter -> t -> unit

  type state

  val init : unit -> state

  val mk : state -> string -> t * bool

  val mk_fresh : state -> t

  val cardinal : state -> int

  val fold : state -> init:'a -> f:(t -> 'a -> 'a) -> 'a

  module Set : Stdlib.Set.S with type elt = t
end = struct
  type t = {index: int; value: value [@ignore]} [@@deriving compare]

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

type atom_equation = {rhs: Atom.t; lhs: Atom.t}

let pp_atom_equation fmt {rhs; lhs} = F.fprintf fmt "%a=%a" Atom.pp lhs Atom.pp rhs

type app_equation = {rhs: Atom.t; left: Atom.t; right: Atom.t}

let pp_app_equation fmt {rhs; left; right} =
  F.fprintf fmt "f(%a,%a)=%a" Atom.pp left Atom.pp right Atom.pp rhs


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
  | PendingApp ({rhs= rhs1}, {rhs= rhs2}) ->
      (rhs1, rhs2)


module LookupTbl = Stdlib.Hashtbl.Make (struct
  type t = Atom.t * Atom.t

  let equal (a1, b1) (a2, b2) =
    Int.equal a1.Atom.index a2.Atom.index && Int.equal b1.Atom.index b2.Atom.index


  let hash (a, b) = [%hash: int * int] (a.Atom.index, b.Atom.index)
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

type t =
  { repr: Atom.t Dynarray.t
  ; mutable pending: pending_item list
  ; atoms: Atom.t Dynarray.t
  ; classes: Class.t Dynarray.t
  ; use: app_equation list Dynarray.t
  ; lookup: app_equation LookupTbl.t
  ; input_app_equations: (Atom.t * Atom.t) option Dynarray.t
  ; term_roots: Atom.Set.t Dynarray.t
  ; mutable app_roots: Atom.Set.t
  ; mutable app_right_neutral: Atom.t option
  ; hashcons: Atom.state
  ; mutable update_count: int
  ; debug: bool }

let init ~debug =
  { repr= Dynarray.create ()
  ; pending= []
  ; atoms= Dynarray.create ()
  ; classes= Dynarray.create ()
  ; use= Dynarray.create ()
  ; lookup= LookupTbl.create 32
  ; input_app_equations= Dynarray.create ()
  ; term_roots= Dynarray.create ()
  ; app_roots= Atom.Set.empty
  ; app_right_neutral= None
  ; update_count= 0
  ; hashcons= Atom.init ()
  ; debug }


let set_app_right_neutral state atom = state.app_right_neutral <- Some atom

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
    Dynarray.add_last state.use [] ;
    Dynarray.add_last state.input_app_equations None ;
    Dynarray.add_last state.term_roots Atom.Set.empty ) ;
  atom


let mk_fresh_atom state =
  let atom = Atom.mk_fresh state.hashcons in
  Dynarray.add_last state.repr atom ;
  Dynarray.add_last state.atoms atom ;
  Dynarray.add_last state.classes (Class.of_atom atom) ;
  Dynarray.add_last state.use [] ;
  Dynarray.add_last state.input_app_equations None ;
  Dynarray.add_last state.term_roots Atom.Set.empty ;
  atom


let get_use {use} {Atom.index} = Dynarray.get use index

let flush_use {use; debug} ({Atom.index} as atom) =
  if debug then F.printf "use[%a] <- []\n" Atom.pp atom ;
  Dynarray.set use index []


let set_use {use; debug} ({Atom.index} as atom) l =
  if debug then F.printf "use[%a] <- [%a]\n" Atom.pp atom (Pp.semicolon_seq pp_app_equation) l ;
  Dynarray.set use index l


let get_input_app_equation {input_app_equations} atom =
  Dynarray.get input_app_equations atom.Atom.index


let set_input_app_equation {input_app_equations} atom pair =
  Dynarray.set input_app_equations atom.Atom.index (Some pair)


let equiv_atoms state {Atom.index} =
  Dynarray.get state.classes index |> Class.fold ~init:[] ~f:(fun l a -> a :: l)


let equiv_terms state {Atom.index} =
  Dynarray.get state.classes index
  |> Class.fold ~init:[] ~f:(fun l rhs ->
         match Dynarray.get state.input_app_equations rhs.Atom.index with
         | None ->
             l
         | Some (left, right) ->
             {rhs; left; right} :: l )


let add_use state atom app_equation = set_use state atom (app_equation :: get_use state atom)

let lookup state key = LookupTbl.find_opt state.lookup key

let set_lookup {lookup; debug} ((a, b) as key) eq =
  if debug then F.printf "Lookup(%a,%a) <- %a\n" Atom.pp a Atom.pp b pp_app_equation eq ;
  LookupTbl.add lookup key eq


type term = App of Atom.t * Atom.t | Atom of Atom.t

let pp_term fmt term =
  match term with
  | Atom a ->
      Atom.pp fmt a
  | App (a, b) ->
      F.fprintf fmt "f(%a,%a)" Atom.pp a Atom.pp b


let rec merge state rhs term =
  if state.debug then F.printf "MERGE %a=%a\n" pp_term term Atom.pp rhs ;
  match term with
  | Atom lhs ->
      state.pending <- PendingAtom {rhs; lhs} :: state.pending ;
      propagate state
  | App (left, right) -> (
      let left' = representative state left in
      let right' = representative state right in
      (* TODO: add track sizes and decide new representative with them *)
      let key = (left', right') in
      match lookup state key with
      | Some app_eq_b ->
          state.pending <- PendingApp ({rhs; left; right}, app_eq_b) :: state.pending ;
          propagate state
      | None ->
          let app_equation = {rhs; left; right} in
          set_lookup state key app_equation ;
          add_use state left' app_equation ;
          add_use state right' app_equation )


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
  let use_new_repr =
    get_use state old_repr
    |> List.fold
         ~f:(fun eqs ({left= c1; right= c2} as app_eq_c) ->
           let c1' = representative state c1 in
           let c2' = representative state c2 in
           let key = (c1', c2') in
           if debug then F.printf "Lookup(%a,%a) ?\n" Atom.pp c1' Atom.pp c2' ;
           match lookup state key with
           | Some app_eq_d ->
               state.pending <- PendingApp (app_eq_c, app_eq_d) :: pending ;
               eqs
           | None ->
               set_lookup state key app_eq_c ;
               app_eq_c :: eqs )
         ~init:(get_use state new_repr)
  in
  flush_use state old_repr ;
  set_use state new_repr use_new_repr


let mk_app state ~left ~right =
  let left' = representative state left in
  let right' = representative state right in
  match lookup state (left', right') with
  | Some {rhs} ->
      representative state rhs
  | None ->
      let term = App (left, right) in
      let atom = mk_fresh_atom state in
      merge state atom term ;
      set_input_app_equation state atom (left, right) ;
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

type header = Atom.t

let mk_header = mk_atom

let pp_header = Atom.pp

let representative_of_header = representative

let mk_term state header args =
  let term = List.fold ~init:header ~f:(fun left right -> mk_app state ~left ~right) args in
  add_term_root state ~header ~term ;
  term


let show_stats state =
  let size = Atom.cardinal state.hashcons in
  let max_depth = Atom.fold state.hashcons ~init:0 ~f:(fun atom -> max (depth state atom)) in
  F.printf "size=%d\nmax_depth=%d\n" size max_depth


let pp_nested_term state fmt atom =
  let rec pp ~internal fmt atom =
    match get_input_app_equation state atom with
    | Some (left, right) ->
        if internal then F.fprintf fmt "%a@ %a" (pp ~internal:true) left (pp ~internal:false) right
        else F.fprintf fmt "@[<hv4>(%a@ %a)@]" (pp ~internal:true) left (pp ~internal:false) right
    | None ->
        Atom.pp fmt atom
  in
  pp ~internal:false fmt atom


let pp_atom_set fmt set = F.fprintf fmt "{%a}" (Pp.comma_seq Atom.pp) (Atom.Set.elements set)

let debug state =
  F.printf "repr: @[<hv>" ;
  Dynarray.iteri
    (fun index atom ->
      let repr = Dynarray.get state.repr index in
      if index > 0 then F.printf "@ " ;
      F.printf "%a is %a (repr=%a)" Atom.pp atom (pp_nested_term state) atom Atom.pp repr ;
      let set = Dynarray.get state.term_roots index in
      if not (Atom.Set.is_empty set) then F.printf " (roots=%a)" pp_atom_set set )
    state.atoms ;
  F.printf "@]@."
