(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Global namespace opened in each source file by the build system *)

include (
  Base :
    sig
      include
        (module type of Base
          with module Option := Base.Option
           and module List := Base.List
          (* prematurely deprecated, remove and use Stdlib instead *)
           and module Filename := Base.Filename
           and module Format := Base.Format
           and module Marshal := Base.Marshal
           and module Scanf := Base.Scanf
           and type ('ok, 'err) result := ('ok, 'err) Base.result
         [@warning "-3"])
    end )

(* undeprecate *)
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"

exception Not_found = Caml.Not_found

include Stdio
module Command = Core.Command
module Hash_queue = Core_kernel.Hash_queue
include Import0

(** Tuple operations *)

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

(** Function combinators *)

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)
let ( $ ) f g x = f x ; g x
let ( $> ) x f = f x ; x
let ( <$ ) f x = f x ; x

(** Failures *)

let fail = Trace.fail

exception Unimplemented of string

let todo fmt = Trace.raisef (fun msg -> Unimplemented msg) fmt

let warn fmt =
  let fs = Format.std_formatter in
  Format.pp_open_box fs 2 ;
  Format.pp_print_string fs "Warning: " ;
  Format.kfprintf
    (fun fs () ->
      Format.pp_close_box fs () ;
      Format.pp_force_newline fs () )
    fs fmt

(** Assertions *)

let assertf cnd fmt =
  if not cnd then fail fmt
  else Format.ikfprintf (fun _ () -> ()) Format.str_formatter fmt

let checkf cnd fmt =
  if not cnd then fail fmt
  else Format.ikfprintf (fun _ () -> true) Format.str_formatter fmt

let check f x =
  assert (f x ; true) ;
  x

let violates f x =
  assert (f x ; true) ;
  assert false

type 'a or_error = ('a, exn * Caml.Printexc.raw_backtrace) result

let or_error f x () =
  try Ok (f x) with exn -> Error (exn, Caml.Printexc.get_raw_backtrace ())

(** Extensions *)

module Invariant = struct
  include Base.Invariant

  let invariant here t sexp_of_t f =
    assert (
      ( try f ()
        with exn ->
          let bt = Caml.Printexc.get_raw_backtrace () in
          let exn =
            Error.to_exn
              (Error.create_s
                 (Base.Sexp.message "invariant failed"
                    [ ("", sexp_of_exn exn)
                    ; ("", Source_code_position.sexp_of_t here)
                    ; ("", sexp_of_t t) ]))
          in
          Caml.Printexc.raise_with_backtrace exn bt ) ;
      true )
end

module Option = Option
include Option.Monad_infix
include Option.Monad_syntax
module List = List
module Vector = Vector
include Vector.Infix

module type OrderedType = sig
  type t

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
end

exception Duplicate

module Set = struct
  module type S = sig
    type elt
    type t

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : (Sexp.t -> elt) -> Sexp.t -> t
    val pp : elt pp -> t pp
    val pp_diff : elt pp -> (t * t) pp

    (* initial constructors *)
    val empty : t
    val of_ : elt -> t
    val of_option : elt option -> t
    val of_list : elt list -> t
    val of_vector : elt vector -> t

    (* constructors *)
    val add : t -> elt -> t
    val add_option : elt option -> t -> t
    val add_list : elt list -> t -> t
    val remove : t -> elt -> t
    val filter : t -> f:(elt -> bool) -> t
    val union : t -> t -> t
    val union_list : t list -> t
    val diff : t -> t -> t
    val inter : t -> t -> t
    val diff_inter : t -> t -> t * t

    (* queries *)
    val is_empty : t -> bool
    val mem : t -> elt -> bool
    val is_subset : t -> of_:t -> bool
    val disjoint : t -> t -> bool
    val max_elt : t -> elt option

    (* traversals *)
    val fold : t -> init:'s -> f:('s -> elt -> 's) -> 's
  end

  module Make (Elt : OrderedType) : S with type elt = Elt.t = struct
    module S = Caml.Set.Make (Elt)

    type elt = Elt.t
    type t = S.t

    let compare = S.compare
    let equal = S.equal
    let sexp_of_t s = List.sexp_of_t Elt.sexp_of_t (S.elements s)

    let t_of_sexp elt_of_sexp sexp =
      S.of_list (List.t_of_sexp elt_of_sexp sexp)

    let pp pp_elt fs x = List.pp ",@ " pp_elt fs (S.elements x)

    let pp_diff pp_elt fs (xs, ys) =
      let lose = S.diff xs ys and gain = S.diff ys xs in
      if not (S.is_empty lose) then
        Format.fprintf fs "-- %a" (pp pp_elt) lose ;
      if not (S.is_empty gain) then
        Format.fprintf fs "++ %a" (pp pp_elt) gain

    let empty = S.empty
    let of_ x = S.add x empty
    let of_option = Option.fold ~f:(fun x y -> S.add y x) ~init:empty
    let of_list = S.of_list
    let of_vector x = S.of_list (Vector.to_list x)
    let add s e = S.add e s
    let add_option yo x = Option.fold ~f:(fun x y -> S.add y x) ~init:x yo
    let add_list ys x = List.fold ~f:(fun x y -> S.add y x) ~init:x ys
    let remove s e = S.remove e s
    let filter s ~f = S.filter f s
    let union = S.union
    let union_list ss = List.fold ss ~init:empty ~f:union
    let diff = S.diff
    let inter = S.inter
    let diff_inter x y = (S.diff x y, S.inter x y)
    let is_empty = S.is_empty
    let mem s e = S.mem e s
    let is_subset x ~of_ = S.subset x of_
    let disjoint = S.disjoint
    let max_elt = S.max_elt_opt
    let fold s ~init:z ~f = S.fold (fun z x -> f x z) s z
  end
end

module Map = struct
  module type S = sig
    type key
    type +'a t

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
    val t_of_sexp : (Sexp.t -> key) -> (Sexp.t -> 'a) -> Sexp.t -> 'a t
    val pp : key pp -> 'a pp -> 'a t pp

    val pp_diff :
         data_equal:('a -> 'a -> bool)
      -> key pp
      -> 'a pp
      -> ('a * 'a) pp
      -> ('a t * 'a t) pp

    (* initial constructors *)
    val empty : 'a t

    (* constructors *)
    val set : 'a t -> key:key -> data:'a -> 'a t
    val add_exn : 'a t -> key:key -> data:'a -> 'a t
    val add_multi : 'a list t -> key:key -> data:'a -> 'a list t
    val remove : 'a t -> key -> 'a t
    val update : 'a t -> key -> f:('a option -> 'a) -> 'a t

    val merge :
         'a t
      -> 'b t
      -> f:
           (   key:key
            -> [`Both of 'a * 'b | `Left of 'a | `Right of 'b]
            -> 'c option)
      -> 'c t

    val merge_skewed :
      'a t -> 'a t -> combine:(key:key -> 'a -> 'a -> 'a) -> 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val filter_keys : 'a t -> f:(key -> bool) -> 'a t
    val filter_mapi : 'a t -> f:(key:key -> data:'a -> 'b option) -> 'b t

    (* queries *)
    val is_empty : 'b t -> bool
    val length : 'b t -> int
    val mem : 'a t -> key -> bool
    val find : 'a t -> key -> 'a option
    val find_and_remove : 'a t -> key -> ('a * 'a t) option
    val find_multi : 'a list t -> key -> 'a list
    val data : 'a t -> 'a list
    val to_alist : 'a t -> (key * 'a) list

    (* traversals *)
    val iter : 'a t -> f:('a -> unit) -> unit
    val iteri : 'a t -> f:(key:key -> data:'a -> unit) -> unit
    val for_alli : 'a t -> f:(key:key -> data:'a -> bool) -> bool
    val fold : 'a t -> init:'s -> f:(key:key -> data:'a -> 's -> 's) -> 's
  end

  module Make (Key : OrderedType) : S with type key = Key.t = struct
    module M = Caml.Map.Make (Key)

    type key = Key.t
    type 'a t = 'a M.t

    let compare = M.compare
    let equal = M.equal

    let sexp_of_t sexp_of_val m =
      List.sexp_of_t
        (Sexplib.Conv.sexp_of_pair Key.sexp_of_t sexp_of_val)
        (M.bindings m)

    let t_of_sexp key_of_sexp val_of_sexp sexp =
      Caml.List.fold_left
        (fun m (k, v) -> M.add k v m)
        M.empty
        (List.t_of_sexp
           (Sexplib.Conv.pair_of_sexp key_of_sexp val_of_sexp)
           sexp)

    let pp pp_k pp_v fs m =
      Format.fprintf fs "@[<1>[%a]@]"
        (List.pp ",@ " (fun fs (k, v) ->
             Format.fprintf fs "@[%a @<2>↦ %a@]" pp_k k pp_v v ))
        (M.bindings m)

    let pp_diff ~data_equal pp_key pp_val pp_diff_val fs (x, y) =
      let pp_diff_val fs = function
        | k, `Left v ->
            Format.fprintf fs "-- [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
        | k, `Right v ->
            Format.fprintf fs "++ [@[%a@ @<2>↦ %a@]]" pp_key k pp_val v
        | k, `Unequal vv ->
            Format.fprintf fs "[@[%a@ @<2>↦ %a@]]" pp_key k pp_diff_val vv
      in
      let sd =
        M.merge
          (fun _ v1o v2o ->
            match (v1o, v2o) with
            | Some v1, Some v2 when not (data_equal v1 v2) ->
                Some (`Unequal (v1, v2))
            | Some v1, None -> Some (`Left v1)
            | None, Some v2 -> Some (`Right v2)
            | _ -> None )
          x y
      in
      if not (M.is_empty sd) then
        Format.fprintf fs "[@[<hv>%a@]];@ "
          (List.pp ";@ " pp_diff_val)
          (M.bindings sd)

    exception Duplicate

    let empty = M.empty
    let set m ~key ~data = M.add key data m

    let add_exn m ~key ~data =
      M.update key
        (function None -> Some data | Some _ -> raise Duplicate)
        m

    let add_multi m ~key ~data =
      M.update key
        (function None -> Some [data] | Some vs -> Some (data :: vs))
        m

    let remove m k = M.remove k m
    let update m k ~f = M.update k (fun vo -> Some (f vo)) m

    let merge m n ~f =
      M.merge
        (fun k v1o v2o ->
          match (v1o, v2o) with
          | Some v1, Some v2 -> f ~key:k (`Both (v1, v2))
          | Some v1, None -> f ~key:k (`Left v1)
          | None, Some v2 -> f ~key:k (`Right v2)
          | None, None -> None )
        m n

    let merge_skewed m n ~combine =
      M.merge
        (fun k v1o v2o ->
          match (v1o, v2o) with
          | Some v1, Some v2 -> Some (combine ~key:k v1 v2)
          | Some _, None -> v1o
          | None, Some _ -> v2o
          | None, None -> None )
        m n

    let map m ~f = M.map f m
    let filter_keys m ~f = M.filter (fun k _ -> f k) m

    let filter_mapi m ~f =
      M.fold
        (fun k v m ->
          match f ~key:k ~data:v with Some v' -> M.add k v' m | None -> m )
        m M.empty

    let is_empty = M.is_empty
    let length = M.cardinal
    let mem m k = M.mem k m
    let find m k = M.find_opt k m

    let find_and_remove m k =
      let found = ref None in
      let m =
        M.update k
          (fun v ->
            found := v ;
            None )
          m
      in
      let+ v = !found in
      (v, m)

    let find_multi m k = try M.find k m with Not_found -> []
    let data m = M.fold (fun _ v s -> v :: s) m []
    let to_alist = M.bindings
    let iter m ~f = M.iter (fun _ v -> f v) m
    let iteri m ~f = M.iter (fun k v -> f ~key:k ~data:v) m
    let for_alli m ~f = M.for_all (fun key data -> f ~key ~data) m
    let fold m ~init ~f = M.fold (fun key data s -> f ~key ~data s) m init
  end
end

module Qset = struct
  include Qset

  let pp sep pp_elt fs s = List.pp sep pp_elt fs (to_list s)
end

module Array = struct
  include Base.Array

  let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
end

module String = struct
  include String

  let t_of_sexp = Sexplib.Conv.string_of_sexp
  let sexp_of_t = Sexplib.Conv.sexp_of_string

  module Map = Map.Make (String)
end

module Q = struct
  let pp = Q.pp_print
  let hash = Hashtbl.hash
  let hash_fold_t s q = Int.hash_fold_t s (hash q)
  let sexp_of_t q = Sexp.Atom (Q.to_string q)

  let t_of_sexp = function
    | Sexp.Atom s -> Q.of_string s
    | _ -> assert false

  let of_z = Q.of_bigint

  include Q
end

module Z = struct
  let pp = Z.pp_print
  let hash = [%hash: Z.t]
  let hash_fold_t s z = Int.hash_fold_t s (hash z)
  let sexp_of_t z = Sexp.Atom (Z.to_string z)

  let t_of_sexp = function
    | Sexp.Atom s -> Z.of_string s
    | _ -> assert false

  (* the signed 1-bit integers are -1 and 0 *)
  let true_ = Z.minus_one
  let false_ = Z.zero
  let of_bool = function true -> true_ | false -> false_
  let is_true = Z.equal true_
  let is_false = Z.equal false_

  include Z
end
