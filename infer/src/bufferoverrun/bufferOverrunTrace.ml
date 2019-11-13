(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
module F = Format

module BoTrace = struct
  type lib_fun = Snprintf | Strndup | Vsnprintf [@@deriving compare]

  type final = UnknownFrom of Typ.Procname.t option [@@deriving compare]

  type elem =
    | ArrayDeclaration
    | JavaIntDecleration
    | Assign of PowLoc.t
    | Global of Loc.t
    | Parameter of Loc.t
    | Through of {risky_fun: lib_fun option}
  [@@deriving compare]

  type t =
    | Empty
    | Final of {location: Location.t; kind: final}
    | Elem of {location: Location.t; length: int; kind: elem; from: t}
    | Call of {location: Location.t; length: int; caller: t; callee: t}
  [@@deriving compare]

  let snprintf = Snprintf

  let strndup = Strndup

  let vsnprintf = Vsnprintf

  let length = function Empty -> 0 | Final _ -> 1 | Elem {length} | Call {length} -> length

  let compare t1 t2 = [%compare: int * t] (length t1, t1) (length t2, t2)

  let final location kind = Final {location; kind}

  let add_elem location kind from = Elem {location; length= length from + 1; from; kind}

  let singleton location kind = add_elem location kind Empty

  let through ~risky_fun = Through {risky_fun}

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
    | Strndup ->
        F.fprintf f "strndup"
    | Vsnprintf ->
        F.fprintf f "vsnprintf"


  let pp_final f = function
    | UnknownFrom pname_opt ->
        F.fprintf f "UnknownFrom `%a`" pp_pname_opt pname_opt


  let pp_elem f = function
    | ArrayDeclaration ->
        F.pp_print_string f "ArrayDeclaration"
    | JavaIntDecleration ->
        F.pp_print_string f "JavaIntDeclaration"
    | Assign locs ->
        F.fprintf f "Assign `%a`" PowLoc.pp locs
    | Global loc ->
        F.fprintf f "Global `%a`" Loc.pp loc
    | Parameter loc ->
        F.fprintf f "Parameter `%a`" Loc.pp loc
    | Through {risky_fun} ->
        F.pp_print_string f "Through" ;
        if Option.is_some risky_fun then F.pp_print_string f " RiskyLibCall"


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


  let has_unknown = final_exists ~f:(function UnknownFrom _ -> true)

  let elem_has_risky = function
    | JavaIntDecleration | ArrayDeclaration | Assign _ | Global _ | Parameter _ ->
        false
    | Through {risky_fun} ->
        Option.is_some risky_fun


  let rec has_risky = function
    | Empty | Final _ ->
        false
    | Elem {kind; from} ->
        elem_has_risky kind || has_risky from
    | Call {caller; callee} ->
        has_risky caller || has_risky callee


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


  let elem_err_desc = function
    | JavaIntDecleration ->
        "int declaration (java)"
    | ArrayDeclaration ->
        "Array declaration"
    | Assign _ ->
        "Assignment"
    | Global loc ->
        if Loc.is_pretty loc then F.asprintf "Global `%a`" Loc.pp loc else ""
    | Parameter loc ->
        if Loc.is_pretty loc then F.asprintf "Parameter `%a`" Loc.pp loc else ""
    | Through {risky_fun} -> (
      match risky_fun with
      | None ->
          "Through"
      | Some f ->
          F.asprintf "Risky value from: %a" pp_lib_fun f )


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
          Errlog.make_trace_element depth location desc [] :: make_err_trace (depth + 1) callee tail
        in
        make_err_trace depth caller tail
end

module Set = struct
  (* currently, we keep only one trace for efficiency *)
  include AbstractDomain.MinReprSet (BoTrace)

  let set_singleton = singleton

  let singleton location elem = singleton (BoTrace.singleton location elem)

  let singleton_final location kind = set_singleton (BoTrace.final location kind)

  let add_elem location elem t =
    if is_bottom t then singleton location elem else map (BoTrace.add_elem location elem) t


  let non_empty t = if is_bottom t then set_singleton BoTrace.Empty else t

  let call location ~traces_caller ~traces_callee =
    let traces_caller = non_empty traces_caller in
    let traces_callee = non_empty traces_callee in
    fold
      (fun caller traces ->
        fold
          (fun callee traces -> add (BoTrace.call location ~caller ~callee) traces)
          traces_callee traces )
      traces_caller bottom


  let has_unknown t = exists BoTrace.has_unknown t

  let has_risky t = exists BoTrace.has_risky t

  let exists_str ~f t = exists (BoTrace.exists_str ~f) t

  let make_err_trace depth set tail =
    match min_elt set with None -> tail | Some trace -> BoTrace.make_err_trace depth trace tail


  let length set = match min_elt set with None -> 0 | Some trace -> BoTrace.length trace
end

module Issue = struct
  type elem = Alloc [@@deriving compare]

  type binary =
    | ArrayAccess
    (* offset, length *)
    | Binop
  [@@deriving compare]

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
