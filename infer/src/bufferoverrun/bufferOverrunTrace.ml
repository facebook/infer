(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
module F = Format

module BoTrace = struct
  type lib_fun = Snprintf | Vsnprintf [@@deriving compare]

  type final = UnknownFrom of Typ.Procname.t option | RiskyLibCall of lib_fun
  [@@deriving compare]

  type elem = ArrayDeclaration | Assign of PowLoc.t | Parameter of Loc.t | Through
  [@@deriving compare]

  type t =
    | Empty
    | Final of {location: Location.t; kind: final}
    | Elem of {location: Location.t; length: int; kind: elem; from: t}
    | Call of {location: Location.t; length: int; caller: t; callee: t}
  [@@deriving compare]

  let snprintf = RiskyLibCall Snprintf

  let vsnprintf = RiskyLibCall Vsnprintf

  let length = function Empty -> 0 | Final _ -> 1 | Elem {length} | Call {length} -> length

  let compare t1 t2 = [%compare: int * t] (length t1, t1) (length t2, t2)

  let final location kind = Final {location; kind}

  let add_elem location kind from = Elem {location; length= length from + 1; from; kind}

  let singleton location kind = add_elem location kind Empty

  let call location ~caller ~callee =
    Call {location; length= 1 + length caller + length callee; caller; callee}


  let pp_pname_opt fmt = function
    | None ->
        F.fprintf fmt "non-const function"
    | Some pname ->
        Typ.Procname.pp fmt pname


  let pp_location = Location.pp_file_pos

  let pp_lib_fun f = function
    | Snprintf ->
        F.fprintf f "snprintf"
    | Vsnprintf ->
        F.fprintf f "vsnprintf"


  let pp_final f = function
    | UnknownFrom pname_opt ->
        F.fprintf f "UnknownFrom `%a`" pp_pname_opt pname_opt
    | RiskyLibCall lib_fun ->
        F.fprintf f "RiskyLibCall `%a`" pp_lib_fun lib_fun


  let pp_elem f = function
    | ArrayDeclaration ->
        F.pp_print_string f "ArrayDeclaration"
    | Assign locs ->
        F.fprintf f "Assign `%a`" PowLoc.pp locs
    | Parameter loc ->
        F.fprintf f "Parameter `%a`" Loc.pp loc
    | Through ->
        F.pp_print_string f "Through"


  let rec pp f = function
    | Empty ->
        F.pp_print_string f "<empty>"
    | Final {location; kind} ->
        F.fprintf f "%a (%a)" pp_final kind pp_location location
    | Elem {location; from; kind} ->
        F.fprintf f "%a%a (%a)" pp_arrow from pp_elem kind pp_location location
    | Call {location; caller; callee} ->
        F.fprintf f "%aCall (%a) -> %a" pp_arrow caller pp_location location pp callee


  and pp_arrow f = function Empty -> () | t -> F.fprintf f "%a -> " pp t

  let rec final_exists ~f = function
    | Empty ->
        false
    | Final {kind= final} ->
        f final
    | Elem {from} ->
        final_exists ~f from
    | Call {caller; callee} ->
        final_exists ~f caller || final_exists ~f callee


  let has_unknown = final_exists ~f:(function UnknownFrom _ -> true | RiskyLibCall _ -> false)

  let has_risky = final_exists ~f:(function UnknownFrom _ -> false | RiskyLibCall _ -> true)

  let exists_str ~f =
    let rec helper = function
      | Empty | Final _ ->
          false
      | Elem {kind= elem; from} ->
          ( match elem with
          | Assign locs ->
              PowLoc.exists_str ~f locs
          | Parameter l ->
              Loc.exists_str ~f l
          | _ ->
              false )
          || helper from
      | Call {caller; callee} ->
          helper caller || helper callee
    in
    helper


  let final_err_desc = function
    | UnknownFrom pname_opt ->
        F.asprintf "Unknown value from: %a" pp_pname_opt pname_opt
    | RiskyLibCall lib_fun ->
        F.asprintf "Risky value from: %a" pp_lib_fun lib_fun


  let elem_err_desc = function
    | ArrayDeclaration ->
        "Array declaration"
    | Assign _ ->
        "Assignment"
    | Parameter loc ->
        if Loc.is_pretty loc then F.asprintf "Parameter `%a`" Loc.pp loc else ""
    | Through ->
        "Through"


  let rec make_err_trace depth t tail =
    match t with
    | Empty ->
        tail
    | Final {location; kind} ->
        let desc = final_err_desc kind in
        Errlog.make_trace_element depth location desc [] :: tail
    | Elem {location; kind; from} ->
        let desc = elem_err_desc kind in
        let tail =
          if String.is_empty desc then tail
          else Errlog.make_trace_element depth location desc [] :: tail
        in
        make_err_trace depth from tail
    | Call {location; caller; callee} ->
        let desc = "Call" in
        let tail =
          Errlog.make_trace_element depth location desc []
          :: make_err_trace (depth + 1) callee tail
        in
        make_err_trace depth caller tail
end

module Set = struct
  include AbstractDomain.FiniteSet (BoTrace)

  let set_singleton = singleton

  let singleton location elem = singleton (BoTrace.singleton location elem)

  let singleton_final location kind = set_singleton (BoTrace.final location kind)

  (* currently, we keep only one trace for efficiency *)
  let join x y =
    if is_empty x then y
    else if is_empty y then x
    else
      let tx, ty = (min_elt x, min_elt y) in
      if Int.( <= ) (BoTrace.length tx) (BoTrace.length ty) then x else y


  let choose_shortest set = min_elt set

  let add_elem location elem t =
    if is_empty t then singleton location elem else map (BoTrace.add_elem location elem) t


  let non_empty t = if is_empty t then set_singleton BoTrace.Empty else t

  let call location ~traces_caller ~traces_callee =
    let traces_caller = non_empty traces_caller in
    let traces_callee = non_empty traces_callee in
    fold
      (fun caller traces ->
        fold
          (fun callee traces -> add (BoTrace.call location ~caller ~callee) traces)
          traces_callee traces )
      traces_caller empty


  let has_unknown t = exists BoTrace.has_unknown t

  let has_risky t = exists BoTrace.has_risky t

  let exists_str ~f t = exists (BoTrace.exists_str ~f) t

  let make_err_trace depth set tail =
    if is_empty set then tail else BoTrace.make_err_trace depth (choose_shortest set) tail


  let length set = if is_empty set then 0 else BoTrace.length (choose_shortest set)
end

module Issue = struct
  type elem = Alloc [@@deriving compare]

  type binary = ArrayAccess (* offset, length *) | Binop [@@deriving compare]

  type t =
    | Elem of {location: Location.t; length: int; kind: elem; from: Set.t}
    | Binary of {location: Location.t; length: int; kind: binary; left: Set.t; right: Set.t}
    | Call of {location: Location.t; length: int; caller: Set.t; callee: t}
  [@@deriving compare]

  let length = function Elem {length} | Binary {length} | Call {length} -> length

  let compare t1 t2 = [%compare: int * t] (length t1, t1) (length t2, t2)

  let alloc location from = Elem {location; length= 1 + Set.length from; kind= Alloc; from}

  let binary location kind left right =
    Binary {location; length= 3 + Set.length left + Set.length right; kind; left; right}


  let call location caller callee =
    Call {location; length= 1 + Set.length caller + length callee; caller; callee}


  let rec has_common ~f = function
    | Elem {from} ->
        f from
    | Binary {left; right} ->
        f left || f right
    | Call {caller; callee} ->
        f caller || has_common ~f callee


  let has_unknown = has_common ~f:Set.has_unknown

  let has_risky = has_common ~f:Set.has_risky

  let exists_str ~f = has_common ~f:(Set.exists_str ~f)

  let binary_labels = function ArrayAccess -> ("Offset", "Length") | Binop -> ("LHS", "RHS")

  let pp_elem f = function Alloc -> F.pp_print_string f "Alloc"

  let pp_binary f = function
    | ArrayAccess ->
        F.pp_print_string f "ArrayAccess"
    | Binop ->
        F.pp_print_string f "Binop"


  let pp_location = Location.pp_file_pos

  let rec pp f = function
    | Elem {location; kind; from} ->
        F.fprintf f "{%a} -> %a (%a)" Set.pp from pp_elem kind pp_location location
    | Binary {location; kind; left; right} ->
        let left_label, right_label = binary_labels kind in
        F.fprintf f "{%s: %a} {%s: %a} %a (%a)" left_label Set.pp left right_label Set.pp right
          pp_binary kind pp_location location
    | Call {location; caller; callee} ->
        F.fprintf f "{%a} Call (%a) -> %a" Set.pp caller pp_location location pp callee


  let elem_err_desc ~description = function Alloc -> "Allocation: " ^ description

  let binary_err_desc ~description = function
    | ArrayAccess ->
        "Array access: " ^ description
    | Binop ->
        "Binary operation: " ^ description


  let format_label label = F.sprintf "<%s trace>" label

  let make_err_trace ~description t =
    let rec aux depth = function
      | Elem {location; kind; from} ->
          let desc = elem_err_desc ~description kind in
          [("", Set.make_err_trace depth from [Errlog.make_trace_element depth location desc []])]
      | Binary {location; kind; left; right} ->
          let left_label, right_label = binary_labels kind in
          let desc = binary_err_desc ~description kind in
          [ (format_label left_label, Set.make_err_trace depth left [])
          ; (format_label right_label, Set.make_err_trace depth right [])
          ; ("", [Errlog.make_trace_element depth location desc []]) ]
      | Call {location; caller; callee} ->
          let desc = "Call" in
          ("", Set.make_err_trace depth caller [Errlog.make_trace_element depth location desc []])
          :: aux (depth + 1) callee
    in
    aux 0 t
end

include BoTrace
