(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Support for localisation *)

module F = Format
module MF = MarkupFormatter

module Tags = struct
  type t = (string * string) list [@@deriving compare]

  let bucket = "bucket"

  let call_line = "call_line"

  (** expression where a value escapes to *)
  let escape_to = "escape_to"

  let line = "line"

  (** string describing a C value, e.g. "x.date" *)
  let value = "value"

  (** describes a NPE that comes from parameter not nullable *)
  let parameter_not_null_checked = "parameter_not_null_checked"

  (** describes a NPE that comes from field not nullable *)
  let field_not_null_checked = "field_not_null_checked"

  (** @Nullable-annoted field/param/retval that causes a warning *)
  let nullable_src = "nullable_src"

  (** Weak variable captured in a block that causes a warning *)
  let weak_captured_var_src = "weak_captured_var_src"

  let empty_vector_access = "empty_vector_access"

  let create () = ref []

  let add tags tag value = List.Assoc.add ~equal:String.equal tags tag value

  let update tags tag value = tags := add !tags tag value

  let get tags tag = List.Assoc.find ~equal:String.equal tags tag
end

type error_desc = {descriptions: string list; tags: Tags.t; dotty: string option}
[@@deriving compare]

(** empty error description *)
let no_desc : error_desc = {descriptions= []; tags= []; dotty= None}

(** verbatim desc from a string, not to be used for user-visible descs *)
let verbatim_desc s = {no_desc with descriptions= [s]}

(** pretty print an error description *)
let pp_error_desc fmt err_desc = Pp.seq F.pp_print_string fmt err_desc.descriptions

let error_desc_get_dotty err_desc = err_desc.dotty

module BucketLevel = struct
  (** highest likelihood *)
  let b1 = "B1"

  let b2 = "B2"

  let b3 = "B3"

  let b4 = "B4"

  (** lowest likelihood *)
  let b5 = "B5"
end

(** get the bucket value of an error_desc, if any *)
let error_desc_get_bucket err_desc = Tags.get err_desc.tags Tags.bucket

(** set the bucket value of an error_desc *)
let error_desc_set_bucket err_desc bucket =
  let tags = Tags.add err_desc.tags Tags.bucket bucket in
  let descriptions =
    if Config.show_buckets then Printf.sprintf "[%s]" bucket :: err_desc.descriptions
    else err_desc.descriptions
  in
  {err_desc with descriptions; tags}


let error_desc_is_reportable_bucket err_desc =
  let issue_bucket = error_desc_get_bucket err_desc in
  let high_buckets = BucketLevel.[b1; b2] in
  Option.value_map issue_bucket ~default:false ~f:(fun b ->
      List.mem ~equal:String.equal high_buckets b )


(** get the value tag, if any *)
let get_value_line_tag tags =
  try
    let value = snd (List.find_exn ~f:(fun (tag, _) -> String.equal tag Tags.value) tags) in
    let line = snd (List.find_exn ~f:(fun (tag, _) -> String.equal tag Tags.line) tags) in
    Some [value; line]
  with
  | Not_found_s _ | Caml.Not_found ->
      None


(** extract from desc a value on which to apply polymorphic hash and equality *)
let desc_get_comparable err_desc =
  match get_value_line_tag err_desc.tags with Some sl' -> sl' | None -> err_desc.descriptions


(** hash function for error_desc *)
let error_desc_hash desc = Hashtbl.hash (desc_get_comparable desc)

(** equality for error_desc *)
let error_desc_equal desc1 desc2 =
  [%compare.equal: string list] (desc_get_comparable desc1) (desc_get_comparable desc2)


let line_tag_ tags tag loc =
  let line_str = string_of_int loc.Location.line in
  Tags.update tags tag line_str ;
  let s = "line " ^ line_str in
  if loc.Location.col <> -1 then
    let col_str = string_of_int loc.Location.col in
    s ^ ", column " ^ col_str
  else s


let at_line_tag tags tag loc = "at " ^ line_tag_ tags tag loc

let line_ tags loc = line_tag_ tags Tags.line loc

let at_line tags loc = at_line_tag tags Tags.line loc

let call_to proc_name =
  let proc_name_str = Typ.Procname.to_simplified_string proc_name in
  "call to " ^ MF.monospaced_to_string proc_name_str


let call_to_at_line tags proc_name loc =
  call_to proc_name ^ " " ^ at_line_tag tags Tags.call_line loc


let by_call_to proc_name = "by " ^ call_to proc_name

let by_call_to_ra tags ra = "by " ^ call_to_at_line tags ra.PredSymb.ra_pname ra.PredSymb.ra_loc

let add_by_call_to_opt problem_str proc_name_opt =
  match proc_name_opt with
  | Some proc_name ->
      problem_str ^ " " ^ by_call_to proc_name
  | None ->
      problem_str


let mem_dyn_allocated = "memory dynamically allocated"

let lock_acquired = "lock acquired"

let released = "released"

let reachable = "reachable"

(** dereference strings used to explain a dereference action in an error message *)
type deref_str =
  { tags: (string * string) list ref  (** tags for the error description *)
  ; value_pre: string option  (** string printed before the value being dereferenced *)
  ; value_post: string option  (** string printed after the value being dereferenced *)
  ; problem_str: string  (** description of the problem *) }

let pointer_or_object () = if Language.curr_language_is Java then "object" else "pointer"

let deref_str_null_ proc_name_opt problem_str_ =
  let problem_str = add_by_call_to_opt problem_str_ proc_name_opt in
  {tags= Tags.create (); value_pre= Some (pointer_or_object ()); value_post= None; problem_str}


(** dereference strings for null dereference *)
let deref_str_null proc_name_opt =
  let problem_str = "could be null and is dereferenced" in
  deref_str_null_ proc_name_opt problem_str


let access_str_empty proc_name_opt =
  let problem_str = "could be empty and is accessed" in
  deref_str_null_ proc_name_opt problem_str


(** dereference strings for null dereference due to Nullable annotation *)
let deref_str_nullable proc_name_opt nullable_obj_str =
  let tags = Tags.create () in
  Tags.update tags Tags.nullable_src nullable_obj_str ;
  (* to be completed once we know if the deref'd expression is directly or transitively @Nullable*)
  let problem_str = "" in
  deref_str_null_ proc_name_opt problem_str


(** dereference strings for null dereference due to weak captured variable in block *)
let deref_str_weak_variable_in_block proc_name_opt nullable_obj_str =
  let tags = Tags.create () in
  Tags.update tags Tags.weak_captured_var_src nullable_obj_str ;
  let problem_str = "" in
  deref_str_null_ proc_name_opt problem_str


(** dereference strings for nonterminal nil arguments in c/objc variadic methods *)
let deref_str_nil_argument_in_variadic_method pn total_args arg_number =
  let function_method, nil_null =
    if Typ.Procname.is_c_method pn then ("method", "nil") else ("function", "null")
  in
  let problem_str =
    Printf.sprintf
      "could be %s which results in a call to %s with %d arguments instead of %d (%s indicates \
       that the last argument of this variadic %s has been reached)"
      nil_null
      (Typ.Procname.to_simplified_string pn)
      arg_number (total_args - 1) nil_null function_method
  in
  deref_str_null_ None problem_str


(** dereference strings for an undefined value coming from the given procedure *)
let deref_str_undef (proc_name, loc) =
  let tags = Tags.create () in
  let proc_name_str = Typ.Procname.to_simplified_string proc_name in
  { tags
  ; value_pre= Some (pointer_or_object ())
  ; value_post= None
  ; problem_str=
      "could be assigned by a call to skip function " ^ proc_name_str
      ^ at_line_tag tags Tags.call_line loc
      ^ " and is dereferenced or freed" }


(** dereference strings for a freed pointer dereference *)
let deref_str_freed ra =
  let tags = Tags.create () in
  let freed_or_closed_by_call =
    let freed_or_closed =
      match ra.PredSymb.ra_res with
      | PredSymb.Rmemory _ ->
          "freed"
      | PredSymb.Rfile ->
          "closed"
      | PredSymb.Rignore ->
          "freed"
      | PredSymb.Rlock ->
          "locked"
    in
    freed_or_closed ^ " " ^ by_call_to_ra tags ra
  in
  { tags
  ; value_pre= Some (pointer_or_object ())
  ; value_post= None
  ; problem_str= "was " ^ freed_or_closed_by_call ^ " and is dereferenced or freed" }


(** dereference strings for a dangling pointer dereference *)
let deref_str_dangling dangling_kind_opt =
  let dangling_kind_prefix =
    match dangling_kind_opt with
    | Some PredSymb.DAuninit ->
        "uninitialized "
    | Some PredSymb.DAaddr_stack_var ->
        "deallocated stack "
    | Some PredSymb.DAminusone ->
        "-1 "
    | None ->
        ""
  in
  { tags= Tags.create ()
  ; value_pre= Some (dangling_kind_prefix ^ pointer_or_object ())
  ; value_post= None
  ; problem_str= "could be dangling and is dereferenced or freed" }


(** dereference strings for a pointer size mismatch *)
let deref_str_pointer_size_mismatch typ_from_instr typ_of_object =
  let str_from_typ typ =
    let pp f = Typ.pp_full Pp.text f typ in
    F.asprintf "%t" pp
  in
  { tags= Tags.create ()
  ; value_pre= Some (pointer_or_object ())
  ; value_post= Some ("of type " ^ str_from_typ typ_from_instr)
  ; problem_str= "could be used to access an object of smaller type " ^ str_from_typ typ_of_object
  }


(** dereference strings for an array out of bound access *)
let deref_str_array_bound size_opt index_opt =
  let tags = Tags.create () in
  let size_str_opt =
    match size_opt with
    | Some n ->
        let n_str = IntLit.to_string n in
        Some ("of size " ^ n_str)
    | None ->
        None
  in
  let index_str =
    match index_opt with
    | Some n ->
        let n_str = IntLit.to_string n in
        "index " ^ n_str
    | None ->
        "an index"
  in
  { tags
  ; value_pre= Some "array"
  ; value_post= size_str_opt
  ; problem_str= "could be accessed with " ^ index_str ^ " out of bounds" }


(** Java unchecked exceptions errors *)
let java_unchecked_exn_desc proc_name exn_name pre_str : error_desc =
  { no_desc with
    descriptions=
      [ MF.monospaced_to_string (Typ.Procname.to_string proc_name)
      ; "can throw " ^ MF.monospaced_to_string (Typ.Name.name exn_name)
      ; "whenever " ^ pre_str ] }


let desc_unsafe_guarded_by_access accessed_fld guarded_by_str loc =
  let line_info = at_line (Tags.create ()) loc in
  let accessed_fld_str = Typ.Fieldname.to_string accessed_fld in
  let annot_str = Printf.sprintf "@GuardedBy(\"%s\")" guarded_by_str in
  let syncronized_str =
    MF.monospaced_to_string (Printf.sprintf "synchronized(%s)" guarded_by_str)
  in
  let msg =
    Format.asprintf
      "The field %a is annotated with %a, but the lock %a is not held during the access to the \
       field %s. Since the current method is non-private, it can be called from outside the \
       current class without synchronization. Consider wrapping the access in a %s block or \
       making the method private."
      MF.pp_monospaced accessed_fld_str MF.pp_monospaced annot_str MF.pp_monospaced guarded_by_str
      line_info syncronized_str
  in
  {no_desc with descriptions= [msg]}


let desc_custom_error loc : error_desc =
  {no_desc with descriptions= ["detected"; at_line (Tags.create ()) loc]}


(** type of access *)
type access =
  | Last_assigned of int * bool
  (* line, null_case_flag *)
  | Last_accessed of int * bool
  (* line, is_nullable flag *)
  | Initialized_automatically
  | Returned_from_call of int

let nullable_annotation_name proc_name =
  match Config.nullable_annotation with
  | Some name ->
      name
  | None when Typ.Procname.is_java proc_name ->
      "@Nullable"
  | None (* default Clang annotation name *) ->
      "_Nullable"


let access_desc access_opt =
  match access_opt with
  | None ->
      []
  | Some (Last_accessed (n, _)) ->
      let line_str = string_of_int n in
      ["last accessed on line " ^ line_str]
  | Some (Last_assigned (n, _)) ->
      let line_str = string_of_int n in
      ["last assigned on line " ^ line_str]
  | Some (Returned_from_call _) ->
      []
  | Some Initialized_automatically ->
      ["initialized automatically"]


let dereference_string proc_name deref_str value_str access_opt loc =
  let tags = deref_str.tags in
  Tags.update tags Tags.value value_str ;
  let is_call_access = match access_opt with Some (Returned_from_call _) -> true | _ -> false in
  let value_desc =
    String.concat ~sep:""
      [ (match deref_str.value_pre with Some s -> s ^ " " | _ -> "")
      ; (if is_call_access then "returned by " else "")
      ; MF.monospaced_to_string value_str
      ; (match deref_str.value_post with Some s -> " " ^ MF.monospaced_to_string s | _ -> "") ]
  in
  let problem_desc =
    let problem_str =
      let annotation_name = nullable_annotation_name proc_name in
      match (Tags.get !tags Tags.nullable_src, Tags.get !tags Tags.weak_captured_var_src) with
      | Some nullable_src, _ ->
          if String.equal nullable_src value_str then
            "is annotated with " ^ annotation_name ^ " and is dereferenced without a null check"
          else
            "is indirectly marked " ^ annotation_name ^ " (source: "
            ^ MF.monospaced_to_string nullable_src
            ^ ") and is dereferenced without a null check"
      | None, Some weak_var_str ->
          if String.equal weak_var_str value_str then
            "is a weak pointer captured in the block and is dereferenced without a null check"
          else
            "is equal to the variable "
            ^ MF.monospaced_to_string weak_var_str
            ^ ", a weak pointer captured in the block, and is dereferenced without a null check"
      | None, None ->
          deref_str.problem_str
    in
    [problem_str ^ " " ^ at_line tags loc]
  in
  let access_desc = access_desc access_opt in
  {no_desc with descriptions= (value_desc :: access_desc) @ problem_desc; tags= !tags}


let parameter_field_not_null_checked_desc (desc : error_desc) exp =
  let parameter_not_nullable_desc var =
    let var_s = Pvar.to_string var in
    let param_not_null_desc =
      "Parameter " ^ MF.monospaced_to_string var_s
      ^ " is not checked for null, there could be a null pointer dereference:"
    in
    { desc with
      descriptions= param_not_null_desc :: desc.descriptions
    ; tags= (Tags.parameter_not_null_checked, var_s) :: desc.tags }
  in
  let field_not_nullable_desc exp =
    let rec exp_to_string exp =
      match exp with
      | Exp.Lfield (exp', field, _) ->
          exp_to_string exp' ^ " -> " ^ Typ.Fieldname.to_string field
      | Exp.Lvar pvar ->
          Mangled.to_string (Pvar.get_name pvar)
      | _ ->
          ""
    in
    let var_s = exp_to_string exp in
    let field_not_null_desc =
      "Instance variable " ^ MF.monospaced_to_string var_s
      ^ " is not checked for null, there could be a null pointer dereference:"
    in
    { desc with
      descriptions= field_not_null_desc :: desc.descriptions
    ; tags= (Tags.field_not_null_checked, var_s) :: desc.tags }
  in
  match exp with
  | Exp.Lvar var ->
      parameter_not_nullable_desc var
  | Exp.Lfield _ ->
      field_not_nullable_desc exp
  | _ ->
      desc


let has_tag (desc : error_desc) tag =
  List.exists ~f:(fun (tag', _) -> String.equal tag tag') desc.tags


let is_parameter_not_null_checked_desc desc = has_tag desc Tags.parameter_not_null_checked

let is_field_not_null_checked_desc desc = has_tag desc Tags.field_not_null_checked

let desc_allocation_mismatch alloc dealloc =
  let tags = Tags.create () in
  let using (primitive_pname, called_pname, loc) =
    let by_call =
      if Typ.Procname.equal primitive_pname called_pname then ""
      else
        " by call to " ^ MF.monospaced_to_string (Typ.Procname.to_simplified_string called_pname)
    in
    "using "
    ^ MF.monospaced_to_string (Typ.Procname.to_simplified_string primitive_pname)
    ^ by_call ^ " "
    ^ at_line (Tags.create ()) (* ignore the tag *) loc
  in
  let description =
    Format.sprintf "%s %s is deallocated %s" mem_dyn_allocated (using alloc) (using dealloc)
  in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_condition_always_true_false i cond_str_opt loc =
  let tags = Tags.create () in
  let value = match cond_str_opt with None -> "" | Some s -> s in
  let tt_ff = if IntLit.iszero i then "false" else "true" in
  Tags.update tags Tags.value value ;
  let description =
    Format.sprintf "Boolean condition %s is always %s %s"
      (if String.equal value "" then "" else " " ^ MF.monospaced_to_string value)
      tt_ff (at_line tags loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_deallocate_stack_variable var_str proc_name loc =
  let tags = Tags.create () in
  Tags.update tags Tags.value var_str ;
  let description =
    Format.asprintf "Stack variable %a is freed by a %s" MF.pp_monospaced var_str
      (call_to_at_line tags proc_name loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_deallocate_static_memory const_str proc_name loc =
  let tags = Tags.create () in
  Tags.update tags Tags.value const_str ;
  let description =
    Format.asprintf "Constant string %a is freed by a %s" MF.pp_monospaced const_str
      (call_to_at_line tags proc_name loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_class_cast_exception pname_opt typ_str1 typ_str2 exp_str_opt loc =
  let tags = Tags.create () in
  let in_expression =
    match exp_str_opt with
    | Some exp_str ->
        Tags.update tags Tags.value exp_str ;
        " in expression " ^ MF.monospaced_to_string exp_str ^ " "
    | None ->
        " "
  in
  let at_line' () =
    match pname_opt with
    | Some proc_name ->
        "in " ^ call_to_at_line tags proc_name loc
    | None ->
        at_line tags loc
  in
  let description =
    Format.asprintf "%a cannot be cast to %a %s %s" MF.pp_monospaced typ_str1 MF.pp_monospaced
      typ_str2 in_expression (at_line' ())
  in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_divide_by_zero expr_str loc =
  let tags = Tags.create () in
  Tags.update tags Tags.value expr_str ;
  let description =
    Format.asprintf "Expression %a could be zero %s" MF.pp_monospaced expr_str (at_line tags loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_empty_vector_access pname_opt object_str loc =
  let vector_str = Format.asprintf "Vector %a" MF.pp_monospaced object_str in
  let desc = access_str_empty pname_opt in
  let tags = desc.tags in
  Tags.update tags Tags.empty_vector_access object_str ;
  let descriptions = [vector_str; desc.problem_str; at_line tags loc] in
  {no_desc with descriptions; tags= !tags}


let is_empty_vector_access_desc desc = has_tag desc Tags.empty_vector_access

let desc_frontend_warning desc sugg_opt loc =
  let tags = Tags.create () in
  let sugg = match sugg_opt with Some sugg -> sugg | None -> "" in
  (* If the description ends in a period, we remove it because the sentence continues with
  "at line ..." *)
  let desc = match String.chop_suffix ~suffix:"." desc with Some desc -> desc | None -> desc in
  let description = Format.sprintf "%s %s. %s" desc (at_line tags loc) sugg in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_leak hpred_type_opt value_str_opt resource_opt resource_action_opt loc bucket_opt =
  let tags = Tags.create () in
  let () =
    match bucket_opt with Some bucket -> Tags.update tags Tags.bucket bucket | None -> ()
  in
  let xxx_allocated_to =
    let value_str, to_, on_ =
      match value_str_opt with
      | None ->
          ("", "", "")
      | Some s ->
          Tags.update tags Tags.value s ;
          (MF.monospaced_to_string s, " to ", " on ")
    in
    let typ_str =
      match hpred_type_opt with
      | Some (Exp.Sizeof {typ= {desc= Tstruct name}}) when Typ.Name.is_class name ->
          " of type " ^ MF.monospaced_to_string (Typ.Name.name name) ^ " "
      | _ ->
          " "
    in
    let desc_str =
      match resource_opt with
      | Some (PredSymb.Rmemory _) ->
          mem_dyn_allocated ^ to_ ^ value_str
      | Some PredSymb.Rfile ->
          "resource" ^ typ_str ^ "acquired" ^ to_ ^ value_str
      | Some PredSymb.Rlock ->
          lock_acquired ^ on_ ^ value_str
      | Some PredSymb.Rignore | None ->
          if is_none value_str_opt then "memory" else value_str
    in
    if String.equal desc_str "" then [] else [desc_str]
  in
  let by_call_to =
    match resource_action_opt with Some ra -> [by_call_to_ra tags ra] | None -> []
  in
  let is_not_rxxx_after =
    let rxxx =
      match resource_opt with
      | Some (PredSymb.Rmemory _) ->
          reachable
      | Some PredSymb.Rfile | Some PredSymb.Rlock ->
          released
      | Some PredSymb.Rignore | None ->
          reachable
    in
    ["is not " ^ rxxx ^ " after " ^ line_ tags loc]
  in
  let bucket_str =
    match bucket_opt with Some bucket when Config.show_buckets -> bucket | _ -> ""
  in
  { no_desc with
    descriptions= (bucket_str :: xxx_allocated_to) @ by_call_to @ is_not_rxxx_after; tags= !tags }


(** kind of precondition not met *)
type pnm_kind = Pnm_bounds | Pnm_dangling

let desc_precondition_not_met kind proc_name loc =
  let tags = Tags.create () in
  let kind_str =
    match kind with
    | None ->
        []
    | Some Pnm_bounds ->
        ["possible array out of bounds"]
    | Some Pnm_dangling ->
        ["possible dangling pointer dereference"]
  in
  {no_desc with descriptions= kind_str @ ["in " ^ call_to_at_line tags proc_name loc]; tags= !tags}


let desc_null_test_after_dereference expr_str line loc =
  let tags = Tags.create () in
  Tags.update tags Tags.value expr_str ;
  let description =
    Format.asprintf "Pointer %a was dereferenced at line %d and is tested for null %s"
      MF.pp_monospaced expr_str line (at_line tags loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_retain_cycle cycle_str loc cycle_dotty =
  Logging.d_strln "Proposition with retain cycle:" ;
  let tags = Tags.create () in
  let desc =
    Format.sprintf "Retain cycle %s involving the following objects:%s" (at_line tags loc)
      cycle_str
  in
  {descriptions= [desc]; tags= !tags; dotty= cycle_dotty}


let registered_observer_being_deallocated_str obj_str =
  "Object " ^ obj_str
  ^ " is registered in a notification center but not being removed before deallocation"


let desc_registered_observer_being_deallocated pvar loc =
  let tags = Tags.create () in
  let obj_str = MF.monospaced_to_string (Pvar.to_string pvar) in
  { no_desc with
    descriptions=
      [ registered_observer_being_deallocated_str obj_str
        ^ at_line tags loc ^ ". Being still registered as observer of the notification "
        ^ "center, the deallocated object " ^ obj_str ^ " may be notified in the future." ]
  ; tags= !tags }


let desc_unary_minus_applied_to_unsigned_expression expr_str_opt typ_str loc =
  let tags = Tags.create () in
  let expression =
    match expr_str_opt with
    | Some s ->
        Tags.update tags Tags.value s ; "expression " ^ s
    | None ->
        "an expression"
  in
  let description =
    Format.asprintf "A unary minus is applied to %a of type %s %s" MF.pp_monospaced expression
      typ_str (at_line tags loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_skip_function proc_name =
  let tags = Tags.create () in
  let proc_name_str = Typ.Procname.to_string proc_name in
  Tags.update tags Tags.value proc_name_str ;
  {no_desc with descriptions= [proc_name_str]; tags= !tags}


let desc_inherently_dangerous_function proc_name =
  let proc_name_str = Typ.Procname.to_string proc_name in
  let tags = Tags.create () in
  Tags.update tags Tags.value proc_name_str ;
  {no_desc with descriptions= [MF.monospaced_to_string proc_name_str]; tags= !tags}


let desc_stack_variable_address_escape pvar addr_dexp_str loc =
  let expr_str = Pvar.to_string pvar in
  let tags = Tags.create () in
  Tags.update tags Tags.value expr_str ;
  let escape_to_str =
    match addr_dexp_str with
    | Some s ->
        Tags.update tags Tags.escape_to s ;
        "to " ^ s ^ " "
    | None ->
        ""
  in
  let variable_str =
    if Pvar.is_frontend_tmp pvar then "temporary"
    else Format.asprintf "stack variable %a" MF.pp_monospaced expr_str
  in
  let description =
    Format.asprintf "Address of %s escapes %s%s" variable_str escape_to_str (at_line tags loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}


let desc_uninitialized_dangling_pointer_deref deref expr_str loc =
  let tags = Tags.create () in
  Tags.update tags Tags.value expr_str ;
  let prefix = match deref.value_pre with Some s -> s | _ -> "" in
  let description =
    Format.asprintf "%s %a %s %s" prefix MF.pp_monospaced expr_str deref.problem_str
      (at_line tags loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}
