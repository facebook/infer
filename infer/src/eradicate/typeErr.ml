(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging
module MF = MarkupFormatter
module P = Printf

(** Module for Type Error messages. *)

(** Describe the origin of values propagated by the checker. *)
module type InstrRefT = sig
  type t [@@deriving compare]

  val equal : t -> t -> bool

  type generator

  val create_generator : Procdesc.Node.t -> generator

  val gen : generator -> t

  val get_node : t -> Procdesc.Node.t

  val hash : t -> int

  val replace_node : t -> Procdesc.Node.t -> t
end

(* InstrRefT *)

(** Per-node instruction reference. *)
module InstrRef : InstrRefT = struct
  type t = Procdesc.Node.t * int [@@deriving compare]

  let equal = [%compare.equal : t]

  type generator = Procdesc.Node.t * int ref

  let hash (n, i) = Hashtbl.hash (Procdesc.Node.hash n, i)

  let get_node (n, _) = n

  let replace_node (_, i) n' = (n', i)

  let create_generator n = (n, ref 0)

  let gen instr_ref_gen =
    let node, ir = instr_ref_gen in
    incr ir ; (node, !ir)
end

(* InstrRef *)

type origin_descr = string * Location.t option * AnnotatedSignature.t option

(* callee signature *)
(* ignore origin descr *)
let compare_origin_descr _ _ = 0

type parameter_not_nullable =
  AnnotatedSignature.annotation
  * string
  * (* description *)
    int
  * (* parameter number *)
    Typ.Procname.t
  * Location.t
  * (* callee location *)
    origin_descr
  [@@deriving compare]

(** Instance of an error *)
type err_instance =
  | Condition_redundant of (bool * string option * bool)
  | Inconsistent_subclass_return_annotation of Typ.Procname.t * Typ.Procname.t
  | Inconsistent_subclass_parameter_annotation of string * int * Typ.Procname.t * Typ.Procname.t
  | Field_not_initialized of Typ.Fieldname.t * Typ.Procname.t
  | Field_not_mutable of Typ.Fieldname.t * origin_descr
  | Field_annotation_inconsistent of AnnotatedSignature.annotation * Typ.Fieldname.t * origin_descr
  | Field_over_annotated of Typ.Fieldname.t * Typ.Procname.t
  | Null_field_access of string option * Typ.Fieldname.t * origin_descr * bool
  | Call_receiver_annotation_inconsistent of
      AnnotatedSignature.annotation * string option * Typ.Procname.t * origin_descr
  | Parameter_annotation_inconsistent of parameter_not_nullable
  | Return_annotation_inconsistent of AnnotatedSignature.annotation * Typ.Procname.t * origin_descr
  | Return_over_annotated of Typ.Procname.t
  [@@deriving compare]

module H = Hashtbl.Make (struct
  type t = err_instance * InstrRef.t option [@@deriving compare]

  let equal = [%compare.equal : t]

  let err_instance_hash x =
    let string_hash s = Hashtbl.hash s in
    let string_opt_hash so = Hashtbl.hash so in
    match x with
    | Condition_redundant (b, so, nn)
     -> Hashtbl.hash (1, b, string_opt_hash so, nn)
    | Field_not_initialized (fn, pn)
     -> Hashtbl.hash (2, string_hash (Typ.Fieldname.to_string fn ^ Typ.Procname.to_string pn))
    | Field_not_mutable (fn, _)
     -> Hashtbl.hash (3, string_hash (Typ.Fieldname.to_string fn))
    | Field_annotation_inconsistent (ann, fn, _)
     -> Hashtbl.hash (4, ann, string_hash (Typ.Fieldname.to_string fn))
    | Field_over_annotated (fn, pn)
     -> Hashtbl.hash (5, string_hash (Typ.Fieldname.to_string fn ^ Typ.Procname.to_string pn))
    | Null_field_access (so, fn, _, _)
     -> Hashtbl.hash (6, string_opt_hash so, string_hash (Typ.Fieldname.to_string fn))
    | Call_receiver_annotation_inconsistent (ann, so, pn, _)
     -> Hashtbl.hash (7, ann, string_opt_hash so, Typ.Procname.hash_pname pn)
    | Parameter_annotation_inconsistent (ann, s, n, pn, _, _)
     -> Hashtbl.hash (8, ann, string_hash s, n, Typ.Procname.hash_pname pn)
    | Return_annotation_inconsistent (ann, pn, _)
     -> Hashtbl.hash (9, ann, Typ.Procname.hash_pname pn)
    | Return_over_annotated pn
     -> Hashtbl.hash (10, Typ.Procname.hash_pname pn)
    | Inconsistent_subclass_return_annotation (pn, opn)
     -> Hashtbl.hash (11, Typ.Procname.hash_pname pn, Typ.Procname.hash_pname opn)
    | Inconsistent_subclass_parameter_annotation (param_name, pos, pn, opn)
     -> let pn_hash = string_hash param_name in
        Hashtbl.hash (12, pn_hash, pos, Typ.Procname.hash_pname pn, Typ.Procname.hash_pname opn)

  let hash (err_inst, instr_ref_opt) =
    let x =
      match instr_ref_opt with None -> None | Some instr_ref -> Some (InstrRef.hash instr_ref)
    in
    let y = err_instance_hash err_inst in
    Hashtbl.hash (x, y)
end
(* H *))

type err_state =
  { loc: Location.t  (** location of the error *)
  ; mutable always: bool  (** always fires on its associated node *) }

let err_tbl : err_state H.t = H.create 1

(** Reset the error table. *)
let reset () = H.reset err_tbl

(** Get the forall status of an err_instance.
    The forall status indicates that the error should be printed only if it
    occurs on every path. *)
let get_forall = function
  | Condition_redundant _
   -> true
  | Field_not_initialized _
   -> false
  | Field_not_mutable _
   -> false
  | Field_annotation_inconsistent _
   -> false
  | Field_over_annotated _
   -> false
  | Inconsistent_subclass_return_annotation _
   -> false
  | Inconsistent_subclass_parameter_annotation _
   -> false
  | Null_field_access _
   -> false
  | Call_receiver_annotation_inconsistent _
   -> false
  | Parameter_annotation_inconsistent _
   -> false
  | Return_annotation_inconsistent _
   -> false
  | Return_over_annotated _
   -> false

(** Reset the always field of the forall erros in the node, so if they are not set again
    we know that they don't fire on every path. *)
let node_reset_forall node =
  let iter (err_instance, instr_ref_opt) err_state =
    match (instr_ref_opt, get_forall err_instance) with
    | Some instr_ref, is_forall
     -> let node' = InstrRef.get_node instr_ref in
        if is_forall && Procdesc.Node.equal node node' then err_state.always <- false
    | None, _
     -> ()
  in
  H.iter iter err_tbl

(** Add an error to the error table and return whether it should be printed now. *)
let add_err find_canonical_duplicate err_instance instr_ref_opt loc =
  let is_forall = get_forall err_instance in
  if H.mem err_tbl (err_instance, instr_ref_opt) then false (* don't print now *)
  else
    let instr_ref_opt_deduplicate =
      match (is_forall, instr_ref_opt) with
      | true, Some instr_ref
       -> (* use canonical duplicate for forall checks *)
          let node = InstrRef.get_node instr_ref in
          let canonical_node = find_canonical_duplicate node in
          let instr_ref' = InstrRef.replace_node instr_ref canonical_node in
          Some instr_ref'
      | _
       -> instr_ref_opt
    in
    H.add err_tbl (err_instance, instr_ref_opt_deduplicate) {loc; always= true} ;
    not is_forall

(* print now if it's not a forall check *)

module Strict = struct
  let method_get_strict (signature: AnnotatedSignature.t) =
    let ia, _ = signature.ret in
    Annotations.ia_get_strict ia

  let this_type_get_strict tenv (signature: AnnotatedSignature.t) =
    match signature.params with
    | (p, _, this_type) :: _ when String.equal (Mangled.to_string p) "this" -> (
      match PatternMatch.type_get_annotation tenv this_type with
      | Some ia
       -> Annotations.ia_get_strict ia
      | None
       -> None )
    | _
     -> None

  let signature_get_strict tenv signature =
    match method_get_strict signature with
    | None
     -> this_type_get_strict tenv signature
    | Some x
     -> Some x

  let origin_descr_get_strict tenv origin_descr =
    match origin_descr with
    | _, _, Some signature
     -> signature_get_strict tenv signature
    | _, _, None
     -> None

  let report_on_method_arguments = false

  (* Return (Some parameters) if there is a method call on a @Nullable object,*)
  (* where the origin of @Nullable in the analysis is the return value of a Strict method*)
  (* with parameters. A method is Strict if it or its class are annotated @Strict. *)
  let err_instance_get_strict tenv err_instance : Annot.t option =
    match err_instance with
    | Call_receiver_annotation_inconsistent (AnnotatedSignature.Nullable, _, _, origin_descr)
    | Null_field_access (_, _, origin_descr, _)
     -> origin_descr_get_strict tenv origin_descr
    | Parameter_annotation_inconsistent (AnnotatedSignature.Nullable, _, _, _, _, origin_descr)
      when report_on_method_arguments
     -> origin_descr_get_strict tenv origin_descr
    | _
     -> None
end

(* Strict *)

type st_report_error =
  Typ.Procname.t -> Procdesc.t -> Localise.t -> Location.t -> ?advice:string option
  -> ?field_name:Typ.Fieldname.t option -> ?origin_loc:Location.t option
  -> ?exception_kind:(string -> Localise.error_desc -> exn) -> ?always_report:bool -> string
  -> unit

(** Report an error right now. *)
let report_error_now tenv (st_report_error: st_report_error) err_instance loc pdesc : unit =
  let pname = Procdesc.get_proc_name pdesc in
  let do_print ew_string kind s =
    L.progress "%a:%d " SourceFile.pp loc.Location.file loc.Location.line ;
    let mname =
      match pname with
      | Typ.Procname.Java pname_java
       -> Typ.Procname.java_get_method pname_java
      | _
       -> Typ.Procname.to_simplified_string pname
    in
    L.progress "%s %s in %s %s@." ew_string (Localise.to_issue_id kind) mname s
  in
  let is_err, kind, description, advice, field_name, origin_loc =
    match err_instance with
    | Condition_redundant (b, s_opt, nonnull)
     -> let name =
          if nonnull then Localise.eradicate_condition_redundant_nonnull
          else Localise.eradicate_condition_redundant
        in
        ( false
        , name
        , P.sprintf "The condition %s is always %b according to the existing annotations."
            (Option.value s_opt ~default:"") b
        , Some
            ( "Consider adding a " ^ MF.monospaced_to_string "@Nullable"
            ^ " annotation or removing the redundant check." )
        , None
        , None )
    | Field_not_initialized (fn, pn)
     -> let constructor_name =
          if Typ.Procname.is_constructor pn then "the constructor"
          else
            match pn with
            | Typ.Procname.Java pn_java
             -> MF.monospaced_to_string (Typ.Procname.java_get_method pn_java)
            | _
             -> MF.monospaced_to_string (Typ.Procname.to_simplified_string pn)
        in
        ( true
        , Localise.eradicate_field_not_initialized
        , Format.asprintf "Field %a is not initialized in %s and is not declared %a"
            MF.pp_monospaced (Typ.Fieldname.to_simplified_string fn) constructor_name
            MF.pp_monospaced "@Nullable"
        , None
        , Some fn
        , None )
    | Field_not_mutable (fn, (origin_description, origin_loc, _))
     -> ( true
        , Localise.eradicate_field_not_mutable
        , Format.asprintf "Field %a is modified but is not declared %a. %s" MF.pp_monospaced
            (Typ.Fieldname.to_simplified_string fn) MF.pp_monospaced "@Mutable" origin_description
        , None
        , None
        , origin_loc )
    | Field_annotation_inconsistent (ann, fn, (origin_description, origin_loc, _))
     -> let kind_s, description =
          match ann with
          | AnnotatedSignature.Nullable
           -> ( Localise.eradicate_field_not_nullable
              , Format.asprintf "Field %a can be null but is not declared %a. %s" MF.pp_monospaced
                  (Typ.Fieldname.to_simplified_string fn) MF.pp_monospaced "@Nullable"
                  origin_description )
          | AnnotatedSignature.Present
           -> ( Localise.eradicate_field_value_absent
              , Format.asprintf
                  "Field %a is assigned a possibly absent value but is declared %a. %s"
                  MF.pp_monospaced (Typ.Fieldname.to_simplified_string fn) MF.pp_monospaced
                  "@Present" origin_description )
        in
        (true, kind_s, description, None, None, origin_loc)
    | Field_over_annotated (fn, pn)
     -> let constructor_name =
          if Typ.Procname.is_constructor pn then "the constructor"
          else
            match pn with
            | Typ.Procname.Java pn_java
             -> Typ.Procname.java_get_method pn_java
            | _
             -> Typ.Procname.to_simplified_string pn
        in
        ( true
        , Localise.eradicate_field_over_annotated
        , Format.asprintf "Field %a is always initialized in %s but is declared %a"
            MF.pp_monospaced (Typ.Fieldname.to_simplified_string fn) constructor_name
            MF.pp_monospaced "@Nullable"
        , None
        , Some fn
        , None )
    | Null_field_access (s_opt, fn, (origin_description, origin_loc, _), indexed)
     -> let at_index = if indexed then "element at index" else "field" in
        ( true
        , Localise.eradicate_null_field_access
        , Format.asprintf "Object %a could be null when accessing %s %a. %s" MF.pp_monospaced
            (Option.value s_opt ~default:"") at_index MF.pp_monospaced
            (Typ.Fieldname.to_simplified_string fn) origin_description
        , None
        , None
        , origin_loc )
    | Call_receiver_annotation_inconsistent (ann, s_opt, pn, (origin_description, origin_loc, _))
     -> let kind_s, description =
          match ann with
          | AnnotatedSignature.Nullable
           -> ( Localise.eradicate_null_method_call
              , Format.asprintf "The value of %a in the call to %a could be null. %s"
                  MF.pp_monospaced (Option.value s_opt ~default:"") MF.pp_monospaced
                  (Typ.Procname.to_simplified_string pn) origin_description )
          | AnnotatedSignature.Present
           -> ( Localise.eradicate_value_not_present
              , Format.asprintf "The value of %a in the call to %a is not %a. %s" MF.pp_monospaced
                  (Option.value s_opt ~default:"") MF.pp_monospaced
                  (Typ.Procname.to_simplified_string pn) MF.pp_monospaced "@Present"
                  origin_description )
        in
        (true, kind_s, description, None, None, origin_loc)
    | Parameter_annotation_inconsistent (ann, s, n, pn, _, (origin_desc, origin_loc, _))
     -> let kind_s, description =
          match ann with
          | AnnotatedSignature.Nullable
           -> ( Localise.eradicate_parameter_not_nullable
              , Format.asprintf
                  "%a needs a non-null value in parameter %d but argument %a can be null. %s"
                  MF.pp_monospaced (Typ.Procname.to_simplified_string pn) n MF.pp_monospaced s
                  origin_desc )
          | AnnotatedSignature.Present
           -> ( Localise.eradicate_parameter_value_absent
              , Format.asprintf
                  "%a needs a present value in parameter %d but argument %a can be absent. %s"
                  MF.pp_monospaced (Typ.Procname.to_simplified_string pn) n MF.pp_monospaced s
                  origin_desc )
        in
        (true, kind_s, description, None, None, origin_loc)
    | Return_annotation_inconsistent (ann, pn, (origin_description, origin_loc, _))
     -> let kind_s, description =
          match ann with
          | AnnotatedSignature.Nullable
           -> ( Localise.eradicate_return_not_nullable
              , Format.asprintf "Method %a may return null but it is not annotated with %a. %s"
                  MF.pp_monospaced (Typ.Procname.to_simplified_string pn) MF.pp_monospaced
                  "@Nullable" origin_description )
          | AnnotatedSignature.Present
           -> ( Localise.eradicate_return_value_not_present
              , Format.asprintf
                  "Method %a may return an absent value but it is annotated with %a. %s"
                  MF.pp_monospaced (Typ.Procname.to_simplified_string pn) MF.pp_monospaced
                  "@Present" origin_description )
        in
        (true, kind_s, description, None, None, origin_loc)
    | Return_over_annotated pn
     -> ( false
        , Localise.eradicate_return_over_annotated
        , Format.asprintf "Method %a is annotated with %a but never returns null." MF.pp_monospaced
            (Typ.Procname.to_simplified_string pn) MF.pp_monospaced "@Nullable"
        , None
        , None
        , None )
    | Inconsistent_subclass_return_annotation (pn, opn)
     -> ( false
        , Localise.eradicate_inconsistent_subclass_return_annotation
        , Format.asprintf "Method %a is annotated with %a but overrides unannotated method %a."
            MF.pp_monospaced (Typ.Procname.to_simplified_string ~withclass:true pn)
            MF.pp_monospaced "@Nullable" MF.pp_monospaced
            (Typ.Procname.to_simplified_string ~withclass:true opn)
        , None
        , None
        , None )
    | Inconsistent_subclass_parameter_annotation (param_name, pos, pn, opn)
     -> let translate_position = function
          | 1
           -> "First"
          | 2
           -> "Second"
          | 3
           -> "Third"
          | n
           -> string_of_int n ^ "th"
        in
        ( false
        , Localise.eradicate_inconsistent_subclass_parameter_annotation
        , Format.asprintf
            "%s parameter %a of method %a is not %a but is declared %ain the parent class method %a."
            (translate_position pos) MF.pp_monospaced param_name MF.pp_monospaced
            (Typ.Procname.to_simplified_string ~withclass:true pn) MF.pp_monospaced "@Nullable"
            MF.pp_monospaced "@Nullable" MF.pp_monospaced
            (Typ.Procname.to_simplified_string ~withclass:true opn)
        , None
        , None
        , None )
  in
  let ew_string = if is_err then "Error" else "Warning" in
  do_print ew_string kind description ;
  let always_report = Strict.err_instance_get_strict tenv err_instance <> None in
  st_report_error pname pdesc kind loc ~advice ~field_name ~origin_loc
    ~exception_kind:(fun k d -> Exceptions.Eradicate (k, d))
    ~always_report description

(** Report an error unless is has been reported already, or unless it's a forall error
    since it requires waiting until the end of the analysis and be printed by flush. *)
let report_error tenv (st_report_error: st_report_error) find_canonical_duplicate err_instance
    instr_ref_opt loc pdesc =
  let should_report_now = add_err find_canonical_duplicate err_instance instr_ref_opt loc in
  if should_report_now then report_error_now tenv st_report_error err_instance loc pdesc

(** Report the forall checks at the end of the analysis and reset the error table *)
let report_forall_checks_and_reset tenv st_report_error proc_desc =
  let iter (err_instance, instr_ref_opt) err_state =
    match (instr_ref_opt, get_forall err_instance) with
    | Some instr_ref, is_forall
     -> let node = InstrRef.get_node instr_ref in
        State.set_node node ;
        if is_forall && err_state.always then
          report_error_now tenv st_report_error err_instance err_state.loc proc_desc
    | None, _
     -> ()
  in
  H.iter iter err_tbl ; reset ()
