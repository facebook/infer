(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Expressions *)

[@@@warning "+9"]

module T = struct
  module T0 = struct
    type op1 =
      (* conversion *)
      | Signed of {bits: int}
      | Unsigned of {bits: int}
      | Convert of {src: Typ.t}
      (* array/struct operations *)
      | Select of int
    [@@deriving compare, equal, hash, sexp]

    type op2 =
      (* comparison *)
      | Eq
      | Dq
      | Gt
      | Ge
      | Lt
      | Le
      | Ugt
      | Uge
      | Ult
      | Ule
      | Ord
      | Uno
      (* arithmetic, numeric and pointer *)
      | Add
      | Sub
      | Mul
      | Div
      | Rem
      | Udiv
      | Urem
      (* boolean / bitwise *)
      | And
      | Or
      | Xor
      | Shl
      | Lshr
      | Ashr
      (* array/struct operations *)
      | Splat
      | Update of int
    [@@deriving compare, equal, hash, sexp]

    type op3 = (* if-then-else *)
      | Conditional
    [@@deriving compare, equal, hash, sexp]

    type opN =
      (* array/struct constants *)
      | Record
      | Struct_rec  (** NOTE: may be cyclic *)
    [@@deriving compare, equal, hash, sexp]

    type t = {desc: desc; term: Term.t}

    and desc =
      | Reg of {name: string; typ: Typ.t}
      | Nondet of {msg: string; typ: Typ.t}
      | Label of {parent: string; name: string}
      | Integer of {data: Z.t; typ: Typ.t}
      | Float of {data: string; typ: Typ.t}
      | Ap1 of op1 * Typ.t * t
      | Ap2 of op2 * Typ.t * t * t
      | Ap3 of op3 * Typ.t * t * t * t
      | ApN of opN * Typ.t * t vector
    [@@deriving compare, equal, hash, sexp]
  end

  include T0
  include Comparator.Make (T0)
end

include T

let term e = e.term

let fix (f : (t -> 'a as 'f) -> 'f) (bot : 'f) (e : t) : 'a =
  let rec fix_f seen e =
    match e.desc with
    | ApN (Struct_rec, _, _) ->
        if List.mem ~equal:( == ) seen e then f bot e
        else f (fix_f (e :: seen)) e
    | _ -> f (fix_f seen) e
  in
  let rec fix_f_seen_nil e =
    match e.desc with
    | ApN (Struct_rec, _, _) -> f (fix_f [e]) e
    | _ -> f fix_f_seen_nil e
  in
  fix_f_seen_nil e

let fix_flip (f : ('z -> t -> 'a as 'f) -> 'f) (bot : 'f) (z : 'z) (e : t) =
  fix (fun f' e z -> f (fun z e -> f' e z) z e) (fun e z -> bot z e) e z

let pp_op2 fs op =
  let pf fmt = Format.fprintf fs fmt in
  match op with
  | Eq -> pf "="
  | Dq -> pf "@<1>≠"
  | Gt -> pf ">"
  | Ge -> pf "@<1>≥"
  | Lt -> pf "<"
  | Le -> pf "@<1>≤"
  | Ugt -> pf "u>"
  | Uge -> pf "@<2>u≥"
  | Ult -> pf "u<"
  | Ule -> pf "@<2>u≤"
  | Ord -> pf "ord"
  | Uno -> pf "uno"
  | Add -> pf "+"
  | Sub -> pf "-"
  | Mul -> pf "@<1>×"
  | Div -> pf "/"
  | Udiv -> pf "udiv"
  | Rem -> pf "rem"
  | Urem -> pf "urem"
  | And -> pf "&&"
  | Or -> pf "||"
  | Xor -> pf "xor"
  | Shl -> pf "shl"
  | Lshr -> pf "lshr"
  | Ashr -> pf "ashr"
  | Splat -> pf "^"
  | Update idx -> pf "[_|%i→_]" idx

let rec pp fs exp =
  let pp_ pp fs exp =
    let pf fmt =
      Format.pp_open_box fs 2 ;
      Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs fmt
    in
    match exp.desc with
    | Reg {name} -> (
      match Var.of_term exp.term with
      | Some v when Var.global v -> pf "%@%s" name
      | _ -> pf "%%%s" name )
    | Nondet {msg} -> pf "nondet \"%s\"" msg
    | Label {name} -> pf "%s" name
    | Integer {data; typ= Pointer _} when Z.equal Z.zero data -> pf "null"
    | Integer {data} -> Trace.pp_styled `Magenta "%a" fs Z.pp data
    | Float {data} -> pf "%s" data
    | Ap1 (Signed {bits}, dst, arg) ->
        pf "((%a)(s%i)@ %a)" Typ.pp dst bits pp arg
    | Ap1 (Unsigned {bits}, dst, arg) ->
        pf "((%a)(u%i)@ %a)" Typ.pp dst bits pp arg
    | Ap1 (Convert {src}, dst, arg) ->
        pf "((%a)(%a)@ %a)" Typ.pp dst Typ.pp src pp arg
    | Ap1 (Select idx, _, rcd) -> pf "%a[%i]" pp rcd idx
    | Ap2 (Splat, _, byt, siz) -> pf "%a^%a" pp byt pp siz
    | Ap2 (Update idx, _, rcd, elt) ->
        pf "[%a@ @[| %i → %a@]]" pp rcd idx pp elt
    | Ap2 (Xor, Integer {bits= 1}, {desc= Integer {data}}, x)
      when Z.is_true data ->
        pf "¬%a" pp x
    | Ap2 (Xor, Integer {bits= 1}, x, {desc= Integer {data}})
      when Z.is_true data ->
        pf "¬%a" pp x
    | Ap2 (op, _, x, y) -> pf "(%a@ %a %a)" pp x pp_op2 op pp y
    | Ap3 (Conditional, _, cnd, thn, els) ->
        pf "(%a@ ? %a@ : %a)" pp cnd pp thn pp els
    | ApN (Record, _, elts) -> pf "{%a}" pp_record elts
    | ApN (Struct_rec, _, elts) -> pf "{|%a|}" (Vector.pp ",@ " pp) elts
  in
  fix_flip pp_ (fun _ _ -> ()) fs exp
  [@@warning "-9"]

and pp_record fs elts =
  [%Trace.fprintf
    fs "%a"
      (fun fs elts ->
        match
          String.init (Vector.length elts) ~f:(fun i ->
              match (Vector.get elts i).desc with
              | Integer {data} -> Char.of_int_exn (Z.to_int data)
              | _ -> raise (Invalid_argument "not a string") )
        with
        | s -> Format.fprintf fs "@[<h>%s@]" (String.escaped s)
        | exception _ ->
            Format.fprintf fs "@[<h>%a@]" (Vector.pp ",@ " pp) elts )
      elts]
  [@@warning "-9"]

(** Invariant *)

let valid_idx idx elts = 0 <= idx && idx < Vector.length elts

let rec invariant exp =
  Invariant.invariant [%here] exp [%sexp_of: t]
  @@ fun () ->
  match exp.desc with
  | Reg {typ} | Nondet {typ} -> assert (Typ.is_sized typ)
  | Integer {data; typ} -> (
    match typ with
    | Integer {bits} ->
        (* data in −(2^(bits − 1)) to 2^(bits − 1) − 1 *)
        let n = Z.shift_left Z.one (bits - 1) in
        assert (Z.(Compare.(neg n <= data && data < n)))
    | Pointer _ -> assert (Z.equal Z.zero data)
    | _ -> assert false )
  | Float {typ} -> (
    match typ with Float _ -> assert true | _ -> assert false )
  | Label _ -> assert true
  | Ap1 (Signed {bits}, dst, arg) -> (
    match (dst, typ_of arg) with
    | Integer {bits= dst_bits}, Typ.Integer _ -> assert (bits <= dst_bits)
    | _ -> assert false )
  | Ap1 (Unsigned {bits}, dst, arg) -> (
    match (dst, typ_of arg) with
    | Integer {bits= dst_bits}, Typ.Integer _ -> assert (bits < dst_bits)
    | _ -> assert false )
  | Ap1 (Convert {src= Integer _}, Integer _, _) -> assert false
  | Ap1 (Convert {src}, dst, arg) ->
      assert (Typ.convertible src dst) ;
      assert (Typ.castable src (typ_of arg)) ;
      assert (not (Typ.equal src dst) (* avoid redundant representations *))
  | Ap1 (Select idx, typ, rcd) -> (
      assert (Typ.castable typ (typ_of rcd)) ;
      match typ with
      | Array _ -> assert true
      | Tuple {elts} | Struct {elts} -> assert (valid_idx idx elts)
      | _ -> assert false )
  | Ap2 (Splat, typ, byt, siz) -> (
      assert (Typ.convertible Typ.byt (typ_of byt)) ;
      assert (Typ.convertible Typ.siz (typ_of siz)) ;
      assert (Typ.is_sized typ) ;
      match siz.desc with
      | Integer {data} -> assert (Z.equal (Z.of_int (Typ.size_of typ)) data)
      | _ -> () )
  | Ap2 (Update idx, typ, rcd, elt) -> (
      assert (Typ.castable typ (typ_of rcd)) ;
      match typ with
      | Tuple {elts} | Struct {elts} ->
          assert (valid_idx idx elts) ;
          assert (Typ.castable (Vector.get elts idx) (typ_of elt))
      | Array {elt= typ_elt} -> assert (Typ.castable typ_elt (typ_of elt))
      | _ -> assert false )
  | Ap2 (op, typ, x, y) -> (
    match (op, typ) with
    | (Eq | Dq | Gt | Ge | Lt | Le), (Integer _ | Float _ | Pointer _)
     |(Ugt | Uge | Ult | Ule), (Integer _ | Pointer _)
     |(Ord | Uno), Float _
     |(Add | Sub), (Integer _ | Float _ | Pointer _)
     |(Mul | Div | Rem), (Integer _ | Float _)
     |(Udiv | Urem | And | Or | Xor | Shl | Lshr | Ashr), Integer _ ->
        let typ_x = typ_of x and typ_y = typ_of y in
        assert (Typ.castable typ typ_x) ;
        assert (Typ.castable typ_x typ_y)
    | _ -> assert false )
  | Ap3 (Conditional, typ, cnd, thn, els) ->
      assert (Typ.is_sized typ) ;
      assert (Typ.castable Typ.bool (typ_of cnd)) ;
      assert (Typ.castable typ (typ_of thn)) ;
      assert (Typ.castable typ (typ_of els))
  | ApN ((Record | Struct_rec), typ, args) -> (
    match typ with
    | Array {elt} ->
        assert (
          Vector.for_all args ~f:(fun arg -> Typ.castable elt (typ_of arg))
        )
    | Tuple {elts} | Struct {elts} ->
        assert (Vector.length elts = Vector.length args) ;
        assert (
          Vector.for_all2_exn elts args ~f:(fun typ arg ->
              Typ.castable typ (typ_of arg) ) )
    | _ -> assert false )
  [@@warning "-9"]

(** Type query *)

and typ_of exp =
  match exp.desc with
  | Reg {typ} | Nondet {typ} | Integer {typ} | Float {typ} -> typ
  | Label _ -> Typ.ptr
  | Ap1 ((Signed _ | Unsigned _ | Convert _), dst, _) -> dst
  | Ap1 (Select idx, typ, _) -> (
    match typ with
    | Array {elt} -> elt
    | Tuple {elts} | Struct {elts} -> Vector.get elts idx
    | _ -> violates invariant exp )
  | Ap2
      ( (Eq | Dq | Gt | Ge | Lt | Le | Ugt | Uge | Ult | Ule | Ord | Uno)
      , _
      , _
      , _ ) ->
      Typ.bool
  | Ap2
      ( ( Add | Sub | Mul | Div | Rem | Udiv | Urem | And | Or | Xor | Shl
        | Lshr | Ashr | Splat | Update _ )
      , typ
      , _
      , _ )
   |Ap3 (Conditional, typ, _, _, _)
   |ApN ((Record | Struct_rec), typ, _) ->
      typ
  [@@warning "-9"]

let typ = typ_of

type exp = t

let pp_exp = pp

(** Registers are the expressions constructed by [Reg] *)
module Reg = struct
  include T

  let pp = pp

  type reg = t

  let var r =
    match Var.of_term r.term with Some v -> v | _ -> violates invariant r

  module Set = struct
    include (
      Set :
        module type of Set with type ('elt, 'cmp) t := ('elt, 'cmp) Set.t )

    type t = Set.M(T).t [@@deriving compare, equal, sexp]

    let pp = Set.pp pp_exp
    let empty = Set.empty (module T)
    let of_list = Set.of_list (module T)
    let union_list = Set.union_list (module T)
    let vars = Set.fold ~init:Var.Set.empty ~f:(fun s r -> add s (var r))
  end

  module Map = struct
    include (
      Map :
        module type of Map
          with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) Map.t )

    type 'v t = 'v Map.M(T).t [@@deriving compare, equal, sexp]

    let empty = Map.empty (module T)
  end

  let demangle =
    let open Ctypes in
    let cxa_demangle =
      (* char *__cxa_demangle(const char *, char *, size_t *, int * ) *)
      Foreign.foreign "__cxa_demangle"
        ( string @-> ptr char @-> ptr size_t @-> ptr int
        @-> returning string_opt )
    in
    let null_ptr_char = from_voidp char null in
    let null_ptr_size_t = from_voidp size_t null in
    let status = allocate int 0 in
    fun mangled ->
      let demangled =
        cxa_demangle mangled null_ptr_char null_ptr_size_t status
      in
      if !@status = 0 then demangled else None

  let pp_demangled fs e =
    match e.desc with
    | Reg {name} -> (
      match demangle name with
      | Some demangled when not (String.equal name demangled) ->
          Format.fprintf fs "“%s”" demangled
      | _ -> () )
    | _ -> ()
    [@@warning "-9"]

  let invariant x =
    Invariant.invariant [%here] x [%sexp_of: t]
    @@ fun () ->
    match x.desc with Reg _ -> invariant x | _ -> assert false

  let name r =
    match r.desc with Reg x -> x.name | _ -> violates invariant r

  let typ r = match r.desc with Reg x -> x.typ | _ -> violates invariant r

  let of_exp e =
    match e.desc with Reg _ -> Some (e |> check invariant) | _ -> None

  let program ?global typ name =
    {desc= Reg {name; typ}; term= Term.var (Var.program ?global name)}
    |> check invariant
end

(** Construct *)

(* registers *)

let reg x = x

(* constants *)

let nondet typ msg =
  {desc= Nondet {msg; typ}; term= Term.nondet msg} |> check invariant

let label ~parent ~name =
  {desc= Label {parent; name}; term= Term.label ~parent ~name}
  |> check invariant

let integer typ data =
  {desc= Integer {data; typ}; term= Term.integer data} |> check invariant

let null = integer Typ.ptr Z.zero
let bool b = integer Typ.bool (Z.of_bool b)
let true_ = bool true
let false_ = bool false

let float typ data =
  {desc= Float {data; typ}; term= Term.float data} |> check invariant

(* type conversions *)

let signed bits x ~to_:typ =
  {desc= Ap1 (Signed {bits}, typ, x); term= Term.signed bits x.term}
  |> check invariant

let unsigned bits x ~to_:typ =
  {desc= Ap1 (Unsigned {bits}, typ, x); term= Term.unsigned bits x.term}
  |> check invariant

let convert src ~to_:dst exp =
  { desc= Ap1 (Convert {src}, dst, exp)
  ; term= Term.convert src ~to_:dst exp.term }
  |> check invariant

(* comparisons *)

let binary op mk ?typ x y =
  let typ = match typ with Some typ -> typ | None -> typ_of x in
  {desc= Ap2 (op, typ, x, y); term= mk x.term y.term} |> check invariant

let ubinary op mk ?typ x y =
  let typ = match typ with Some typ -> typ | None -> typ_of x in
  let umk x y =
    let unsigned = Term.unsigned (Typ.bit_size_of typ) in
    mk (unsigned x) (unsigned y)
  in
  binary op umk ~typ x y

let eq = binary Eq Term.eq
let dq = binary Dq Term.dq
let gt = binary Gt (fun x y -> Term.lt y x)
let ge = binary Ge (fun x y -> Term.le y x)
let lt = binary Lt Term.lt
let le = binary Le Term.le
let ugt = ubinary Ugt (fun x y -> Term.lt y x)
let uge = ubinary Uge (fun x y -> Term.le y x)
let ult = ubinary Ult Term.lt
let ule = ubinary Ule Term.le
let ord = binary Ord Term.ord
let uno = binary Uno Term.uno

(* arithmetic *)

let add = binary Add Term.add
let sub = binary Sub Term.sub
let mul = binary Mul Term.mul
let div = binary Div Term.div
let rem = binary Rem Term.rem
let udiv = ubinary Udiv Term.div
let urem = ubinary Urem Term.rem

(* boolean / bitwise *)

let and_ = binary And Term.and_
let or_ = binary Or Term.or_

(* bitwise *)

let xor = binary Xor Term.xor
let shl = binary Shl Term.shl
let lshr = binary Lshr Term.lshr
let ashr = binary Ashr Term.ashr

(* if-then-else *)

let conditional ?typ ~cnd ~thn ~els =
  let typ = match typ with Some typ -> typ | None -> typ_of thn in
  { desc= Ap3 (Conditional, typ, cnd, thn, els)
  ; term= Term.conditional ~cnd:cnd.term ~thn:thn.term ~els:els.term }
  |> check invariant

(* memory *)

let splat typ ~byt ~siz =
  { desc= Ap2 (Splat, typ, byt, siz)
  ; term= Term.splat ~byt:byt.term ~siz:siz.term }
  |> check invariant

(* records (struct / array values) *)

let record typ elts =
  { desc= ApN (Record, typ, elts)
  ; term= Term.record (Vector.map ~f:(fun elt -> elt.term) elts) }
  |> check invariant

let select typ rcd idx =
  {desc= Ap1 (Select idx, typ, rcd); term= Term.select ~rcd:rcd.term ~idx}
  |> check invariant

let update typ ~rcd idx ~elt =
  { desc= Ap2 (Update idx, typ, rcd, elt)
  ; term= Term.update ~rcd:rcd.term ~idx ~elt:elt.term }
  |> check invariant

let struct_rec key =
  let memo_id = Hashtbl.create key in
  let rec_app = (Staged.unstage (Term.rec_app key)) Term.Record in
  Staged.stage
  @@ fun ~id typ elt_thks ->
  match Hashtbl.find memo_id id with
  | None ->
      (* Add placeholder to prevent computing [elts] in calls to
         [struct_rec] from [elt_thks] for recursive occurrences of [id]. *)
      let elta = Array.create ~len:(Vector.length elt_thks) null in
      let elts = Vector.of_array elta in
      Hashtbl.set memo_id ~key:id ~data:elts ;
      let term =
        rec_app ~id (Vector.map ~f:(fun elt -> lazy elt.term) elts)
      in
      Vector.iteri elt_thks ~f:(fun i (lazy elt) -> elta.(i) <- elt) ;
      {desc= ApN (Struct_rec, typ, elts); term} |> check invariant
  | Some elts ->
      (* Do not check invariant as invariant will be checked above after the
         thunks are forced, before which invariant-checking may spuriously
         fail. Note that it is important that the value constructed here
         shares the array in the memo table, so that the update after
         forcing the recursive thunks also updates this value. *)
      {desc= ApN (Struct_rec, typ, elts); term= rec_app ~id Vector.empty}

let size_of exp = integer Typ.siz (Z.of_int (Typ.size_of (typ exp)))

(** Traverse *)

let fold_exps e ~init ~f =
  let fold_exps_ fold_exps_ e z =
    let z =
      match e.desc with
      | Ap1 (_, _, x) -> fold_exps_ x z
      | Ap2 (_, _, x, y) -> fold_exps_ y (fold_exps_ x z)
      | Ap3 (_, _, w, x, y) -> fold_exps_ w (fold_exps_ y (fold_exps_ x z))
      | ApN (_, _, xs) ->
          Vector.fold xs ~init:z ~f:(fun z elt -> fold_exps_ elt z)
      | _ -> z
    in
    f z e
  in
  fix fold_exps_ (fun _ z -> z) e init

let fold_regs e ~init ~f =
  fold_exps e ~init ~f:(fun z x ->
      match x.desc with Reg _ -> f z (x :> Reg.t) | _ -> z )

(** Query *)

let is_true e =
  match e.desc with
  | Integer {data; typ= Integer {bits= 1; _}} -> Z.is_true data
  | _ -> false

let is_false e =
  match e.desc with
  | Integer {data; typ= Integer {bits= 1; _}} -> Z.is_false data
  | _ -> false
