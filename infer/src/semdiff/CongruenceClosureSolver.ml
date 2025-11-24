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
  type t = private {index: int; value: value}

  val pp : F.formatter -> t -> unit

  type state

  val init : unit -> state

  val mk : state -> string -> t * bool

  val mk_fresh : state -> t

  val cardinal : state -> int

  val fold : state -> init:'a -> f:(t -> 'a -> 'a) -> 'a
end = struct
  type t = {index: int; value: value}

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

type t =
  { repr: Atom.t Dynarray.t
  ; classes: Atom.t list Dynarray.t
  ; mutable pending: pending_item list
  ; use: app_equation list Dynarray.t
  ; lookup: app_equation LookupTbl.t
  ; mk_app_history: (Atom.t * Atom.t) option Dynarray.t
  ; hashcons: Atom.state
  ; debug: bool }

let init ~debug =
  { repr= Dynarray.create ()
  ; classes= Dynarray.create ()
  ; pending= []
  ; use= Dynarray.create ()
  ; lookup= LookupTbl.create 32
  ; mk_app_history= Dynarray.create ()
  ; hashcons= Atom.init ()
  ; debug }


let rec representative state atom =
  (* TODO: path compression *)
  let parent = Dynarray.get state.repr atom.Atom.index in
  if phys_equal parent atom then atom else representative state parent


let rec depth state atom =
  let parent = Dynarray.get state.repr atom.Atom.index in
  if phys_equal parent atom then 0 else 1 + depth state parent


let mk_atom state value =
  let atom, is_new = Atom.mk state.hashcons value in
  if is_new then (
    Dynarray.add_last state.repr atom ;
    Dynarray.add_last state.classes [atom] ;
    Dynarray.add_last state.use [] ;
    Dynarray.add_last state.mk_app_history None ) ;
  atom


let mk_fresh_atom state =
  let atom = Atom.mk_fresh state.hashcons in
  Dynarray.add_last state.repr atom ;
  Dynarray.add_last state.classes [atom] ;
  Dynarray.add_last state.use [] ;
  Dynarray.add_last state.mk_app_history None ;
  atom


let get_use {use} {Atom.index} = Dynarray.get use index

let flush_use {use; debug} ({Atom.index} as atom) =
  if debug then F.printf "use[%a] <- []\n" Atom.pp atom ;
  Dynarray.set use index []


let set_use {use; debug} ({Atom.index} as atom) l =
  if debug then F.printf "use[%a] <- [%a]\n" Atom.pp atom (Pp.semicolon_seq pp_app_equation) l ;
  Dynarray.set use index l


let get_mk_app_history {mk_app_history} atom = Dynarray.get mk_app_history atom.Atom.index

let set_mk_app_history {mk_app_history} atom pair =
  Dynarray.set mk_app_history atom.Atom.index (Some pair)


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


and change_representative state old_repr new_repr =
  if state.debug then F.printf "repr[%a] <- %a\n" Atom.pp old_repr Atom.pp new_repr ;
  Dynarray.set state.repr old_repr.Atom.index new_repr ;
  (* TODO: update classes *)
  let use_new_repr =
    get_use state old_repr
    |> List.fold
         ~f:(fun eqs ({left= c1; right= c2} as app_eq_c) ->
           let c1' = representative state c1 in
           let c2' = representative state c2 in
           let key = (c1', c2') in
           if state.debug then F.printf "Lookup(%a,%a) ?\n" Atom.pp c1' Atom.pp c2' ;
           match lookup state key with
           | Some app_eq_d ->
               state.pending <- PendingApp (app_eq_c, app_eq_d) :: state.pending ;
               eqs
           | None ->
               set_lookup state key app_eq_c ;
               app_eq_c :: eqs )
         ~init:(get_use state new_repr)
  in
  flush_use state old_repr ;
  set_use state new_repr use_new_repr


let mk_app state ~left ~right =
  let term = App (left, right) in
  let atom = mk_fresh_atom state in
  merge state atom term ;
  set_mk_app_history state atom (left, right) ;
  atom


let mk_term state ~header ~args =
  let header_atom = mk_atom state header in
  List.fold ~init:header_atom ~f:(fun left right -> mk_app state ~left ~right) args


let show_stats state =
  let size = Atom.cardinal state.hashcons in
  let max_depth = Atom.fold state.hashcons ~init:0 ~f:(fun atom -> max (depth state atom)) in
  F.printf "size=%d\nmax_depth=%d\n" size max_depth


let pp_nested_term state atom =
  let rec pp ~internal fmt atom =
    match get_mk_app_history state atom with
    | Some (left, right) ->
        if internal then F.fprintf fmt "%a@ %a" (pp ~internal:true) left (pp ~internal:false) right
        else F.fprintf fmt "@[<hv4>(%a@ %a)@]" (pp ~internal:true) left (pp ~internal:false) right
    | None ->
        Atom.pp fmt atom
  in
  F.printf "%a@." (pp ~internal:false) atom
