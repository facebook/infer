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
          (* extended below, remove *)
          with module Array := Base.Array
           and module Invariant := Base.Invariant
           and module List := Base.List
           and module Map := Base.Map
           and module Option := Base.Option
           and module Result := Base.Result
           and module Set := Base.Set
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

include Stdio
module Command = Core.Command
module Hash_queue = Core_kernel.Hash_queue

(** Tuple operations *)

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

(** Function combinators *)

let ( >> ) f g x = g (f x)
let ( $ ) f g x = f x ; g x
let ( $> ) x f = f x ; x
let ( <$ ) f x = f x ; x

(** Pretty-printing *)

type 'a pp = Formatter.t -> 'a -> unit
type ('a, 'b) fmt = ('a, Formatter.t, unit, 'b) format4

(** Failures *)

let warn fmt =
  let fs = Format.std_formatter in
  Format.pp_open_box fs 2 ;
  Format.pp_print_string fs "Warning: " ;
  Format.kfprintf
    (fun fs () ->
      Format.pp_close_box fs () ;
      Format.pp_force_newline fs () )
    fs fmt

let raisef ?margin exn fmt =
  let bt = Caml.Printexc.get_raw_backtrace () in
  let fs = Format.str_formatter in
  ( match margin with
  | Some m ->
      Format.pp_set_margin fs m ;
      Format.pp_set_max_indent fs (m - 1)
  | None -> () ) ;
  Format.pp_open_box fs 2 ;
  Format.kfprintf
    (fun fs () ->
      Format.pp_close_box fs () ;
      let msg = Format.flush_str_formatter () in
      let exn = exn msg in
      Caml.Printexc.raise_with_backtrace exn bt )
    fs fmt

exception Unimplemented of string

let todo fmt = raisef (fun msg -> Unimplemented msg) fmt
let fail fmt = raisef (fun msg -> Failure msg) fmt

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

let map_preserving_phys_equal map t ~f =
  let change = ref false in
  let t' =
    map t ~f:(fun x ->
        let x' = f x in
        if not (x' == x) then change := true ;
        x' )
  in
  if !change then t' else t

module Option = struct
  include Base.Option

  let pp fmt pp_elt fs = function
    | Some x -> Format.fprintf fs fmt pp_elt x
    | None -> ()

  let cons xo xs = match xo with Some x -> x :: xs | None -> xs
end

include Option.Monad_infix

module List = struct
  include Base.List

  let rec pp ?pre ?suf sep pp_elt fs = function
    | [] -> ()
    | x :: xs ->
        Option.iter pre ~f:(Format.fprintf fs) ;
        pp_elt fs x ;
        ( match xs with
        | [] -> ()
        | xs -> Format.fprintf fs "%( %)%a" sep (pp sep pp_elt) xs ) ;
        Option.iter suf ~f:(Format.fprintf fs)

  let pop_exn = function x :: xs -> (x, xs) | [] -> raise Caml.Not_found

  let find_map_remove xs ~f =
    let rec find_map_remove_ ys = function
      | [] -> None
      | x :: xs -> (
        match f x with
        | Some x' -> Some (x', rev_append ys xs)
        | None -> find_map_remove_ (x :: ys) xs )
    in
    find_map_remove_ [] xs

  let fold_option xs ~init ~f =
    With_return.with_return
    @@ fun {return} ->
    Some
      (fold xs ~init ~f:(fun acc elt ->
           match f acc elt with Some res -> res | None -> return None ))

  let map_preserving_phys_equal t ~f = map_preserving_phys_equal map t ~f

  let remove_exn ?(equal = phys_equal) xs x =
    let rec remove_ ys = function
      | [] -> raise Caml.Not_found
      | z :: xs ->
          if equal x z then rev_append ys xs else remove_ (z :: ys) xs
    in
    remove_ [] xs

  let remove xs x = try Some (remove_exn xs x) with Caml.Not_found -> None

  let rec rev_init n ~f =
    if n = 0 then []
    else
      let n = n - 1 in
      let xs = rev_init n ~f in
      f n :: xs

  let symmetric_diff ~compare xs ys =
    let rec symmetric_diff_ xxs yys =
      match (xxs, yys) with
      | x :: xs, y :: ys ->
          let ord = compare x y in
          if ord = 0 then symmetric_diff_ xs ys
          else if ord < 0 then Either.First x :: symmetric_diff_ xs yys
          else Either.Second y :: symmetric_diff_ xxs ys
      | xs, [] -> map ~f:Either.first xs
      | [], ys -> map ~f:Either.second ys
    in
    symmetric_diff_ (sort ~compare xs) (sort ~compare ys)
end

module Map = struct
  include Base.Map

  let equal_m__t (module Elt : Compare_m) equal_v = equal equal_v

  let find_and_remove_exn m k =
    let found = ref None in
    let m =
      change m k ~f:(fun v ->
          found := v ;
          None )
    in
    match !found with None -> raise Caml.Not_found | Some v -> (v, m)

  let find_and_remove m k =
    try Some (find_and_remove_exn m k) with Caml.Not_found -> None

  let find_or_add (type data) map key ~(default : data) ~if_found ~if_added
      =
    let exception Found of data in
    match
      update map key ~f:(function
        | Some old_data -> Exn.raise_without_backtrace (Found old_data)
        | None -> default )
    with
    | exception Found old_data -> if_found old_data
    | map -> if_added map

  let map_preserving_phys_equal t ~f = map_preserving_phys_equal map t ~f
end

module Result = struct
  include Base.Result

  let pp fmt pp_elt fs = function
    | Ok x -> Format.fprintf fs fmt pp_elt x
    | Error _ -> ()
end

module Vector = struct
  include Vector

  let pp sep pp_elt fs v = List.pp sep pp_elt fs (to_list v)
end

include Vector.Infix

module Set = struct
  include Base.Set

  type ('elt, 'cmp) tree = ('elt, 'cmp) Using_comparator.Tree.t

  let equal_m__t (module Elt : Compare_m) = equal
  let pp pp_elt fs x = List.pp ",@ " pp_elt fs (to_list x)
  let disjoint x y = is_empty (inter x y)
  let add_option yo x = Option.fold ~f:add ~init:x yo
  let add_list ys x = List.fold ~f:add ~init:x ys
  let diff_inter_diff x y = (diff x y, inter x y, diff y x)
  let inter_diff x y = (inter x y, diff y x)
  let of_vector cmp x = of_array cmp (Vector.to_array x)
  let to_tree = Using_comparator.to_tree

  let union x y =
    let xy = union x y in
    let xy_tree = to_tree xy in
    if xy_tree == to_tree x then x
    else if xy_tree == to_tree y then y
    else xy
end

module Qset = struct
  include Qset

  let pp sep pp_elt fs s = List.pp sep pp_elt fs (to_list s)
end

module Array = struct
  include Base.Array

  let pp sep pp_elt fs a = List.pp sep pp_elt fs (to_list a)
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
