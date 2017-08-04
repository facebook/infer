(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Support for localisation *)

module F = Format
module MF = MarkupFormatter

type t = string * string [@@deriving compare]

(* issue_id, human_readable *)

let equal = [%compare.equal : t]

(** create from an ordinary string *)
let from_string ?hum s : t =
  let prettify () =
    String.lowercase s |> String.split ~on:'_' |> List.map ~f:String.capitalize
    |> String.concat ~sep:" " |> String.strip
  in
  (s, match hum with Some str -> str | _ -> prettify ())

(** return the id of an issue *)
let to_issue_id (s, _) = s

let to_human_readable_string (_, s) = s

(** pretty print a localised string *)
let pp fmt t = Format.fprintf fmt "%s" (to_issue_id t)

let analysis_stops = from_string "ANALYSIS_STOPS"

let array_out_of_bounds_l1 = from_string "ARRAY_OUT_OF_BOUNDS_L1"

let array_out_of_bounds_l2 = from_string "ARRAY_OUT_OF_BOUNDS_L2"

let array_out_of_bounds_l3 = from_string "ARRAY_OUT_OF_BOUNDS_L3"

let buffer_overrun = from_string "BUFFER_OVERRUN"

let checkers_access_global = from_string "CHECKERS_ACCESS_GLOBAL"

let checkers_immutable_cast = from_string "CHECKERS_IMMUTABLE_CAST"

let checkers_print_c_call = from_string "CHECKERS_PRINT_C_CALL"

let checkers_print_objc_method_calls = from_string "CHECKERS_PRINT_OBJC_METHOD_CALLS"

let checkers_printf_args = from_string "CHECKERS_PRINTF_ARGS"

let checkers_repeated_calls = from_string "CHECKERS_REPEATED_CALLS"

let checkers_trace_calls_sequence = from_string "CHECKERS_TRACE_CALLS_SEQUENCE"

let class_cast_exception = from_string "CLASS_CAST_EXCEPTION"

let cluster_callback = from_string "CLUSTER_CALLBACK"

let comparing_floats_for_equality = from_string "COMPARING_FLOAT_FOR_EQUALITY"

let condition_always_false = from_string "CONDITION_ALWAYS_FALSE"

let condition_always_true = from_string "CONDITION_ALWAYS_TRUE"

let context_leak = from_string "CONTEXT_LEAK"

let dangling_pointer_dereference = from_string "DANGLING_POINTER_DEREFERENCE"

let dead_store = from_string "DEAD_STORE"

let deallocate_stack_variable = from_string "DEALLOCATE_STACK_VARIABLE"

let deallocate_static_memory = from_string "DEALLOCATE_STATIC_MEMORY"

let deallocation_mismatch = from_string "DEALLOCATION_MISMATCH"

let divide_by_zero = from_string "DIVIDE_BY_ZERO"

let double_lock = from_string "DOUBLE_LOCK"

let empty_vector_access = from_string "EMPTY_VECTOR_ACCESS"

let eradicate_condition_redundant =
  from_string "ERADICATE_CONDITION_REDUNDANT" ~hum:"Condition Redundant"

let eradicate_condition_redundant_nonnull =
  from_string "ERADICATE_CONDITION_REDUNDANT_NONNULL" ~hum:"Condition Redundant Non-Null"

let eradicate_field_not_initialized =
  from_string "ERADICATE_FIELD_NOT_INITIALIZED" ~hum:"Field Not Initialized"

let eradicate_field_not_mutable =
  from_string "ERADICATE_FIELD_NOT_MUTABLE" ~hum:"Field Not Mutable"

let eradicate_field_not_nullable =
  from_string "ERADICATE_FIELD_NOT_NULLABLE" ~hum:"Field Not Nullable"

let eradicate_field_over_annotated =
  from_string "ERADICATE_FIELD_OVER_ANNOTATED" ~hum:"Field Over Annotated"

let eradicate_field_value_absent =
  from_string "ERADICATE_FIELD_VALUE_ABSENT" ~hum:"Field Value Absent"

let eradicate_inconsistent_subclass_parameter_annotation =
  from_string "ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION"
    ~hum:"Inconsistent Subclass Parameter Annotation"

let eradicate_inconsistent_subclass_return_annotation =
  from_string "ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION"
    ~hum:"Inconsistent Subclass Return Annotation"

let eradicate_null_field_access =
  from_string "ERADICATE_NULL_FIELD_ACCESS" ~hum:"Null Field Access"

let eradicate_null_method_call = from_string "ERADICATE_NULL_METHOD_CALL" ~hum:"Null Method Call"

let eradicate_parameter_not_nullable =
  from_string "ERADICATE_PARAMETER_NOT_NULLABLE" ~hum:"Parameter Not Nullable"

let eradicate_parameter_value_absent =
  from_string "ERADICATE_PARAMETER_VALUE_ABSENT" ~hum:"Parameter Value Absent"

let eradicate_return_not_nullable =
  from_string "ERADICATE_RETURN_NOT_NULLABLE" ~hum:"Return Not Nullable"

let eradicate_return_over_annotated =
  from_string "ERADICATE_RETURN_OVER_ANNOTATED" ~hum:"Return Over Annotated"

let eradicate_return_value_not_present =
  from_string "ERADICATE_RETURN_VALUE_NOT_PRESENT" ~hum:"Return Value Not Present"

let eradicate_value_not_present =
  from_string "ERADICATE_VALUE_NOT_PRESENT" ~hum:"Value Not Present"

let field_should_be_nullable = from_string "FIELD_SHOULD_BE_NULLABLE"

let field_not_null_checked = from_string "IVAR_NOT_NULL_CHECKED"

let inherently_dangerous_function = from_string "INHERENTLY_DANGEROUS_FUNCTION"

let memory_leak = from_string "MEMORY_LEAK"

let null_dereference = from_string "NULL_DEREFERENCE"

let null_test_after_dereference = from_string "NULL_TEST_AFTER_DEREFERENCE"

let parameter_not_null_checked = from_string "PARAMETER_NOT_NULL_CHECKED"

let pointer_size_mismatch = from_string "POINTER_SIZE_MISMATCH"

let precondition_not_found = from_string "PRECONDITION_NOT_FOUND"

let precondition_not_met = from_string "PRECONDITION_NOT_MET"

let premature_nil_termination = from_string "PREMATURE_NIL_TERMINATION_ARGUMENT"

let proc_callback = from_string "PROC_CALLBACK" ~hum:"Procedure Callback"

let quandary_taint_error = from_string "QUANDARY_TAINT_ERROR"

let registered_observer_being_deallocated = from_string "REGISTERED_OBSERVER_BEING_DEALLOCATED"

let resource_leak = from_string "RESOURCE_LEAK"

let retain_cycle = from_string "RETAIN_CYCLE"

let return_expression_required = from_string "RETURN_EXPRESSION_REQUIRED"

let return_statement_missing = from_string "RETURN_STATEMENT_MISSING"

let return_value_ignored = from_string "RETURN_VALUE_IGNORED"

let skip_function = from_string "SKIP_FUNCTION"

let skip_pointer_dereference = from_string "SKIP_POINTER_DEREFERENCE"

let stack_variable_address_escape = from_string "STACK_VARIABLE_ADDRESS_ESCAPE"

let static_initialization_order_fiasco = from_string "STATIC_INITIALIZATION_ORDER_FIASCO"

let thread_safety_violation = from_string "THREAD_SAFETY_VIOLATION"

let unary_minus_applied_to_unsigned_expression =
  from_string "UNARY_MINUS_APPLIED_TO_UNSIGNED_EXPRESSION"

let uninitialized_value = from_string "UNINITIALIZED_VALUE"

let unreachable_code_after = from_string "UNREACHABLE_CODE"

let unsafe_guarded_by_access = from_string "UNSAFE_GUARDED_BY_ACCESS"

let use_after_free = from_string "USE_AFTER_FREE"

module Tags = struct
  type t = (string * string) list [@@deriving compare]

  let accessed_line = "accessed_line"

  (* line where value was last accessed *)
  let alloc_function = "alloc_function"

  (* allocation function used *)
  let alloc_call = "alloc_call"

  (* call in the current procedure which triggers the allocation *)
  let alloc_line = "alloc_line"

  (* line of alloc_call *)
  let array_index = "array_index"

  (* index of the array *)
  let array_size = "array_size"

  (* size of the array *)
  let assigned_line = "assigned_line"

  (* line where value was last assigned *)
  let bucket = "bucket"

  (* bucket to classify likelyhood of real bug *)
  let call_procedure = "call_procedure"

  (* name of the procedure called *)
  let call_line = "call_line"

  (* line of call_procedure *)
  let dealloc_function = "dealloc_function"

  (* deallocation function used *)
  let dealloc_call = "dealloc_call"

  (* call in the current procedure which triggers the deallocation *)
  let dealloc_line = "dealloc_line"

  (* line of dealloc_call *)
  let dereferenced_line = "dereferenced_line"

  (* line where value was dereferenced *)
  let escape_to = "escape_to"

  (* expression wher a value escapes to *)
  let line = "line"

  (* line of the error *)
  let type1 = "type1"

  (* 1st Java type *)
  let type2 = "type2"

  (* 2nd Java type *)
  let value = "value"

  (* string describing a C value, e.g. "x.date" *)
  let parameter_not_null_checked = "parameter_not_null_checked"

  (* describes a NPE that comes from parameter not nullable *)
  let field_not_null_checked = "field_not_null_checked"

  (* describes a NPE that comes from field not nullable *)
  let nullable_src = "nullable_src"

  (* @Nullable-annoted field/param/retval that causes a warning *)
  let weak_captured_var_src = "weak_captured_var_src"

  (* Weak variable captured in a block that causes a warning *)
  let double_lock = "double_lock"

  let empty_vector_access = "empty_vector_access"

  let create () = ref []

  let add tags tag value = List.Assoc.add ~equal:String.equal tags tag value

  let update tags tag value = tags := add !tags tag value

  let get tags tag = List.Assoc.find ~equal:String.equal tags tag

  let tag_value_records_of_tags tags =
    List.map ~f:(fun (tag, value) -> {Jsonbug_t.tag= tag; value}) tags

  let tags_of_tag_value_records (tag_value_records: Jsonbug_t.tag_value_record list) =
    List.map ~f:(fun {Jsonbug_t.tag; value} -> (tag, value)) tag_value_records

  let lines_of_tags (tags: t) =
    let line_tags =
      String.Set.of_list
        [dereferenced_line; call_line; assigned_line; alloc_line; accessed_line; dealloc_line]
    in
    List.filter_map
      ~f:(fun (tag, value) ->
        if String.Set.mem line_tags tag then Some (int_of_string value) else None)
      tags
end

type error_desc =
  {descriptions: string list; advice: string option; tags: Tags.t; dotty: string option}
  [@@deriving compare]

(** empty error description *)
let no_desc : error_desc = {descriptions= []; advice= None; tags= []; dotty= None}

(** verbatim desc from a string, not to be used for user-visible descs *)
let verbatim_desc s = {no_desc with descriptions= [s]}

let custom_desc s tags = {no_desc with descriptions= [s]; tags}

let custom_desc_with_advice description advice tags =
  {no_desc with descriptions= [description]; advice= Some advice; tags}

(** pretty print an error description *)
let pp_error_desc fmt err_desc =
  let pp_item fmt s = F.fprintf fmt "%s" s in
  Pp.seq pp_item fmt err_desc.descriptions

(** pretty print an error advice *)
let pp_error_advice fmt err_desc =
  match err_desc.advice with Some advice -> F.fprintf fmt "%s" advice | None -> ()

(** get tags of error description *)
let error_desc_get_tags err_desc = err_desc.tags

let error_desc_get_dotty err_desc = err_desc.dotty

module BucketLevel = struct
  let b1 = "B1"

  (* highest likelyhood *)
  let b2 = "B2"

  let b3 = "B3"

  let b4 = "B4"

  let b5 = "B5"

  (* lowest likelyhood *)
end

(** takes in input a tag to extract from the given error_desc
    and returns its value *)
let error_desc_extract_tag_value err_desc tag_to_extract =
  let find_value tag v = match v with t, _ when String.equal t tag -> true | _ -> false in
  match List.find ~f:(find_value tag_to_extract) err_desc.tags with Some (_, s) -> s | None -> ""

let error_desc_to_tag_value_pairs err_desc = err_desc.tags

(** returns the content of the value tag of the error_desc *)
let error_desc_get_tag_value error_desc = error_desc_extract_tag_value error_desc Tags.value

(** returns the content of the call_procedure tag of the error_desc *)
let error_desc_get_tag_call_procedure error_desc =
  error_desc_extract_tag_value error_desc Tags.call_procedure

(** get the bucket value of an error_desc, if any *)
let error_desc_get_bucket err_desc = Tags.get err_desc.tags Tags.bucket

(** set the bucket value of an error_desc; the boolean indicates where the bucket should be shown in the message *)
let error_desc_set_bucket err_desc bucket show_in_message =
  let tags' = Tags.add err_desc.tags Tags.bucket bucket in
  let l = err_desc.descriptions in
  let l' = if not show_in_message then l else ("[" ^ bucket ^ "]") :: l in
  {err_desc with descriptions= l'; tags= tags'}

(** get the value tag, if any *)
let get_value_line_tag tags =
  try
    let value = snd (List.find_exn ~f:(fun (tag, _) -> String.equal tag Tags.value) tags) in
    let line = snd (List.find_exn ~f:(fun (tag, _) -> String.equal tag Tags.line) tags) in
    Some [value; line]
  with Not_found -> None

(** extract from desc a value on which to apply polymorphic hash and equality *)
let desc_get_comparable err_desc =
  match get_value_line_tag err_desc.tags with Some sl' -> sl' | None -> err_desc.descriptions

(** hash function for error_desc *)
let error_desc_hash desc = Hashtbl.hash (desc_get_comparable desc)

(** equality for error_desc *)
let error_desc_equal desc1 desc2 =
  [%compare.equal : string list] (desc_get_comparable desc1) (desc_get_comparable desc2)

let _line_tag tags tag loc =
  let line_str = string_of_int loc.Location.line in
  Tags.update tags tag line_str ;
  let s = "line " ^ line_str in
  if loc.Location.col <> -1 then
    let col_str = string_of_int loc.Location.col in
    s ^ ", column " ^ col_str
  else s

let at_line_tag tags tag loc = "at " ^ _line_tag tags tag loc

let _line tags loc = _line_tag tags Tags.line loc

let at_line tags loc = at_line_tag tags Tags.line loc

let call_to tags proc_name =
  let proc_name_str = Typ.Procname.to_simplified_string proc_name in
  Tags.update tags Tags.call_procedure proc_name_str ;
  "call to " ^ MF.monospaced_to_string proc_name_str

let call_to_at_line tags proc_name loc =
  call_to tags proc_name ^ " " ^ at_line_tag tags Tags.call_line loc

let by_call_to tags proc_name = "by " ^ call_to tags proc_name

let by_call_to_ra tags ra = "by " ^ call_to_at_line tags ra.PredSymb.ra_pname ra.PredSymb.ra_loc

let add_by_call_to_opt problem_str tags proc_name_opt =
  match proc_name_opt with
  | Some proc_name
   -> problem_str ^ " " ^ by_call_to tags proc_name
  | None
   -> problem_str

let rec format_typ typ =
  match typ.Typ.desc with
  | Typ.Tptr (t, _) when Config.curr_language_is Config.Java
   -> format_typ t
  | Typ.Tstruct name
   -> Typ.Name.name name
  | _
   -> Typ.to_string typ

let format_field f =
  if Config.curr_language_is Config.Java then Typ.Fieldname.java_get_field f
  else Typ.Fieldname.to_string f

let format_method pname =
  match pname with
  | Typ.Procname.Java pname_java
   -> Typ.Procname.java_get_method pname_java
  | _
   -> Typ.Procname.to_string pname

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

let pointer_or_object () = if Config.curr_language_is Config.Java then "object" else "pointer"

let _deref_str_null proc_name_opt _problem_str tags =
  let problem_str = add_by_call_to_opt _problem_str tags proc_name_opt in
  {tags; value_pre= Some (pointer_or_object ()); value_post= None; problem_str}

(** dereference strings for null dereference *)
let deref_str_null proc_name_opt =
  let problem_str = "could be null and is dereferenced" in
  _deref_str_null proc_name_opt problem_str (Tags.create ())

let access_str_empty proc_name_opt =
  let problem_str = "could be empty and is accessed" in
  _deref_str_null proc_name_opt problem_str (Tags.create ())

(** dereference strings for null dereference due to Nullable annotation *)
let deref_str_nullable proc_name_opt nullable_obj_str =
  let tags = Tags.create () in
  Tags.update tags Tags.nullable_src nullable_obj_str ;
  (* to be completed once we know if the deref'd expression is directly or transitively @Nullable*)
  let problem_str = "" in
  _deref_str_null proc_name_opt problem_str tags

(** dereference strings for null dereference due to weak captured variable in block *)
let deref_str_weak_variable_in_block proc_name_opt nullable_obj_str =
  let tags = Tags.create () in
  Tags.update tags Tags.weak_captured_var_src nullable_obj_str ;
  let problem_str = "" in
  _deref_str_null proc_name_opt problem_str tags

(** dereference strings for nonterminal nil arguments in c/objc variadic methods *)
let deref_str_nil_argument_in_variadic_method pn total_args arg_number =
  let tags = Tags.create () in
  let function_method, nil_null =
    if Typ.Procname.is_c_method pn then ("method", "nil") else ("function", "null")
  in
  let problem_str =
    Printf.sprintf
      "could be %s which results in a call to %s with %d arguments instead of %d (%s indicates that the last argument of this variadic %s has been reached)"
      nil_null (Typ.Procname.to_simplified_string pn) arg_number (total_args - 1) nil_null
      function_method
  in
  _deref_str_null None problem_str tags

(** dereference strings for an undefined value coming from the given procedure *)
let deref_str_undef (proc_name, loc) =
  let tags = Tags.create () in
  let proc_name_str = Typ.Procname.to_simplified_string proc_name in
  Tags.update tags Tags.call_procedure proc_name_str ;
  { tags
  ; value_pre= Some (pointer_or_object ())
  ; value_post= None
  ; problem_str=
      "could be assigned by a call to skip function " ^ proc_name_str
      ^ at_line_tag tags Tags.call_line loc ^ " and is dereferenced or freed" }

(** dereference strings for a freed pointer dereference *)
let deref_str_freed ra =
  let tags = Tags.create () in
  let freed_or_closed_by_call =
    let freed_or_closed =
      match ra.PredSymb.ra_res with
      | PredSymb.Rmemory _
       -> "freed"
      | PredSymb.Rfile
       -> "closed"
      | PredSymb.Rignore
       -> "freed"
      | PredSymb.Rlock
       -> "locked"
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
    | Some PredSymb.DAuninit
     -> "uninitialized "
    | Some PredSymb.DAaddr_stack_var
     -> "deallocated stack "
    | Some PredSymb.DAminusone
     -> "-1 "
    | None
     -> ""
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
    | Some n
     -> let n_str = IntLit.to_string n in
        Tags.update tags Tags.array_size n_str ; Some ("of size " ^ n_str)
    | None
     -> None
  in
  let index_str =
    match index_opt with
    | Some n
     -> let n_str = IntLit.to_string n in
        Tags.update tags Tags.array_index n_str ; "index " ^ n_str
    | None
     -> "an index"
  in
  { tags
  ; value_pre= Some "array"
  ; value_post= size_str_opt
  ; problem_str= "could be accessed with " ^ index_str ^ " out of bounds" }

(** dereference strings for an uninitialized access whose lhs has the given attribute *)
let deref_str_uninitialized alloc_att_opt =
  let tags = Tags.create () in
  let creation_str =
    match alloc_att_opt with
    | Some Sil.Apred (Aresource ({ra_kind= Racquire} as ra), _)
     -> "after allocation " ^ by_call_to_ra tags ra
    | _
     -> "after declaration"
  in
  { tags
  ; value_pre= Some "value"
  ; value_post= None
  ; problem_str= "was not initialized " ^ creation_str ^ " and is used" }

(** Java unchecked exceptions errors *)
let java_unchecked_exn_desc proc_name exn_name pre_str : error_desc =
  { no_desc with
    descriptions=
      [ MF.monospaced_to_string (Typ.Procname.to_string proc_name)
      ; ("can throw " ^ MF.monospaced_to_string (Typ.Name.name exn_name))
      ; ("whenever " ^ pre_str) ] }

let desc_context_leak pname context_typ fieldname leak_path : error_desc =
  let fld_str = Typ.Fieldname.to_string fieldname in
  let leak_root = "Static field " ^ fld_str ^ " |->\n" in
  let leak_path_entry_to_str acc entry =
    let entry_str =
      match entry with
      | Some fld, _
       -> Typ.Fieldname.to_string fld
      | None, typ
       -> Typ.to_string typ
    in
    (* intentionally omit space; [typ_to_string] adds an extra space *)
    acc ^ entry_str ^ " |->\n"
  in
  let context_str = Typ.to_string context_typ in
  let path_str =
    let path_prefix =
      if List.is_empty leak_path then "Leaked "
      else List.fold ~f:leak_path_entry_to_str ~init:"" leak_path ^ "Leaked "
    in
    path_prefix ^ context_str
  in
  let preamble =
    let pname_str =
      match pname with
      | Typ.Procname.Java pname_java
       -> MF.monospaced_to_string
            (Printf.sprintf "%s.%s" (Typ.Procname.java_get_class_name pname_java)
               (Typ.Procname.java_get_method pname_java))
      | _
       -> ""
    in
    "Context " ^ context_str ^ " may leak during method " ^ pname_str ^ ":\n"
  in
  {no_desc with descriptions= [(preamble ^ MF.code_to_string (leak_root ^ path_str))]}

let desc_double_lock pname_opt object_str loc =
  let mutex_str = Format.sprintf "Mutex %s" object_str in
  let tags = Tags.create () in
  let msg = "could be locked and is locked again" in
  let msg = add_by_call_to_opt msg tags pname_opt in
  Tags.update tags Tags.double_lock object_str ;
  let descriptions = [mutex_str; msg; at_line tags loc] in
  {no_desc with descriptions; tags= !tags}

let desc_unsafe_guarded_by_access accessed_fld guarded_by_str loc =
  let line_info = at_line (Tags.create ()) loc in
  let accessed_fld_str = Typ.Fieldname.to_string accessed_fld in
  let annot_str = Printf.sprintf "@GuardedBy(\"%s\")" guarded_by_str in
  let syncronized_str =
    MF.monospaced_to_string (Printf.sprintf "synchronized(%s)" guarded_by_str)
  in
  let msg =
    Format.asprintf
      "The field %a is annotated with %a, but the lock %a is not held during the access to the field %s. Since the current method is non-private, it can be called from outside the current class without synchronization. Consider wrapping the access in a %s block or making the method private."
      MF.pp_monospaced accessed_fld_str MF.pp_monospaced annot_str MF.pp_monospaced guarded_by_str
      line_info syncronized_str
  in
  {no_desc with descriptions= [msg]}

let desc_fragment_retains_view fragment_typ fieldname fld_typ pname : error_desc =
  (* TODO: try advice *)
  let problem =
    Printf.sprintf "Fragment %s does not nullify View field %s (type %s) in %s."
      (format_typ fragment_typ) (format_field fieldname) (format_typ fld_typ) (format_method pname)
  in
  let consequences =
    "If this Fragment is placed on the back stack, a reference to this (probably dead) View will be retained."
  in
  let advice =
    "In general, it is a good idea to initialize View's in onCreateView, then nullify them in onDestroyView."
  in
  {no_desc with descriptions= [problem; consequences; advice]}

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

let dereference_string deref_str value_str access_opt loc =
  let tags = deref_str.tags in
  Tags.update tags Tags.value value_str ;
  let is_call_access = match access_opt with Some Returned_from_call _ -> true | _ -> false in
  let value_desc =
    String.concat ~sep:""
      [ (match deref_str.value_pre with Some s -> s ^ " " | _ -> "")
      ; (if is_call_access then "returned by " else "")
      ; MF.monospaced_to_string value_str
      ; (match deref_str.value_post with Some s -> " " ^ MF.monospaced_to_string s | _ -> "") ]
  in
  let access_desc =
    match access_opt with
    | None
     -> []
    | Some Last_accessed (n, _)
     -> let line_str = string_of_int n in
        Tags.update tags Tags.accessed_line line_str ; [("last accessed on line " ^ line_str)]
    | Some Last_assigned (n, _)
     -> let line_str = string_of_int n in
        Tags.update tags Tags.assigned_line line_str ; [("last assigned on line " ^ line_str)]
    | Some Returned_from_call _
     -> []
    | Some Initialized_automatically
     -> ["initialized automatically"]
  in
  let problem_desc =
    let nullable_text =
      MF.monospaced_to_string
        (if Config.curr_language_is Config.Java then "@Nullable" else "__nullable")
    in
    let problem_str =
      match (Tags.get !tags Tags.nullable_src, Tags.get !tags Tags.weak_captured_var_src) with
      | Some nullable_src, _
       -> if String.equal nullable_src value_str then "is annotated with " ^ nullable_text
            ^ " and is dereferenced without a null check"
          else "is indirectly marked " ^ nullable_text ^ " (source: "
            ^ MF.monospaced_to_string nullable_src ^ ") and is dereferenced without a null check"
      | None, Some weak_var_str
       -> if String.equal weak_var_str value_str then
            "is a weak pointer captured in the block and is dereferenced without a null check"
          else "is equal to the variable " ^ MF.monospaced_to_string weak_var_str
            ^ ", a weak pointer captured in the block, and is dereferenced without a null check"
      | None, None
       -> deref_str.problem_str
    in
    [(problem_str ^ " " ^ at_line tags loc)]
  in
  {no_desc with descriptions= value_desc :: access_desc @ problem_desc; tags= !tags}

let parameter_field_not_null_checked_desc (desc: error_desc) exp =
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
      | Exp.Lfield (exp', field, _)
       -> exp_to_string exp' ^ " -> " ^ Typ.Fieldname.to_string field
      | Exp.Lvar pvar
       -> Mangled.to_string (Pvar.get_name pvar)
      | _
       -> ""
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
  | Exp.Lvar var
   -> parameter_not_nullable_desc var
  | Exp.Lfield _
   -> field_not_nullable_desc exp
  | _
   -> desc

let has_tag (desc: error_desc) tag =
  List.exists ~f:(fun (tag', _) -> String.equal tag tag') desc.tags

let is_parameter_not_null_checked_desc desc = has_tag desc Tags.parameter_not_null_checked

let is_field_not_null_checked_desc desc = has_tag desc Tags.field_not_null_checked

let is_parameter_field_not_null_checked_desc desc =
  is_parameter_not_null_checked_desc desc || is_field_not_null_checked_desc desc

let is_double_lock_desc desc = has_tag desc Tags.double_lock

let desc_allocation_mismatch alloc dealloc =
  let tags = Tags.create () in
  let using is_alloc (primitive_pname, called_pname, loc) =
    let tag_fun, tag_call, tag_line =
      if is_alloc then (Tags.alloc_function, Tags.alloc_call, Tags.alloc_line)
      else (Tags.dealloc_function, Tags.dealloc_call, Tags.dealloc_line)
    in
    Tags.update tags tag_fun (Typ.Procname.to_simplified_string primitive_pname) ;
    Tags.update tags tag_call (Typ.Procname.to_simplified_string called_pname) ;
    Tags.update tags tag_line (string_of_int loc.Location.line) ;
    let by_call =
      if Typ.Procname.equal primitive_pname called_pname then ""
      else " by call to "
        ^ MF.monospaced_to_string (Typ.Procname.to_simplified_string called_pname)
    in
    "using " ^ MF.monospaced_to_string (Typ.Procname.to_simplified_string primitive_pname)
    ^ by_call ^ " " ^ at_line (Tags.create ()) (* ignore the tag *) loc
  in
  let description =
    Format.sprintf "%s %s is deallocated %s" mem_dyn_allocated (using true alloc)
      (using false dealloc)
  in
  {no_desc with descriptions= [description]; tags= !tags}

let desc_comparing_floats_for_equality loc =
  let tags = Tags.create () in
  {no_desc with descriptions= [("Comparing floats for equality " ^ at_line tags loc)]; tags= !tags}

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

let desc_unreachable_code_after loc =
  let tags = Tags.create () in
  let description = "Unreachable code after statement " ^ at_line tags loc in
  {no_desc with descriptions= [description]}

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
  Tags.update tags Tags.type1 typ_str1 ;
  Tags.update tags Tags.type2 typ_str2 ;
  let in_expression =
    match exp_str_opt with
    | Some exp_str
     -> Tags.update tags Tags.value exp_str ;
        " in expression " ^ MF.monospaced_to_string exp_str ^ " "
    | None
     -> " "
  in
  let at_line' () =
    match pname_opt with
    | Some proc_name
     -> "in " ^ call_to_at_line tags proc_name loc
    | None
     -> at_line tags loc
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
  let description = Format.sprintf "%s %s. %s" desc (at_line tags loc) sugg in
  {no_desc with descriptions= [description]; tags= !tags}

let desc_leak hpred_type_opt value_str_opt resource_opt resource_action_opt loc bucket_opt =
  let tags = Tags.create () in
  let () =
    match bucket_opt with Some bucket -> Tags.update tags Tags.bucket bucket | None -> ()
  in
  let xxx_allocated_to =
    let value_str, _to, _on =
      match value_str_opt with
      | None
       -> ("", "", "")
      | Some s
       -> Tags.update tags Tags.value s ; (MF.monospaced_to_string s, " to ", " on ")
    in
    let typ_str =
      match hpred_type_opt with
      | Some Exp.Sizeof {typ= {desc= Tstruct name}} when Typ.Name.is_class name
       -> " of type " ^ MF.monospaced_to_string (Typ.Name.name name) ^ " "
      | _
       -> " "
    in
    let desc_str =
      match resource_opt with
      | Some PredSymb.Rmemory _
       -> mem_dyn_allocated ^ _to ^ value_str
      | Some PredSymb.Rfile
       -> "resource" ^ typ_str ^ "acquired" ^ _to ^ value_str
      | Some PredSymb.Rlock
       -> lock_acquired ^ _on ^ value_str
      | Some PredSymb.Rignore | None
       -> if is_none value_str_opt then "memory" else value_str
    in
    if String.equal desc_str "" then [] else [desc_str]
  in
  let by_call_to =
    match resource_action_opt with Some ra -> [by_call_to_ra tags ra] | None -> []
  in
  let is_not_rxxx_after =
    let rxxx =
      match resource_opt with
      | Some PredSymb.Rmemory _
       -> reachable
      | Some PredSymb.Rfile | Some PredSymb.Rlock
       -> released
      | Some PredSymb.Rignore | None
       -> reachable
    in
    [("is not " ^ rxxx ^ " after " ^ _line tags loc)]
  in
  let bucket_str =
    match bucket_opt with Some bucket when Config.show_buckets -> bucket | _ -> ""
  in
  { no_desc with
    descriptions= bucket_str :: xxx_allocated_to @ by_call_to @ is_not_rxxx_after; tags= !tags }

let desc_buffer_overrun bucket desc =
  let err_desc = {no_desc with descriptions= [desc]} in
  error_desc_set_bucket err_desc bucket Config.show_buckets

(** kind of precondition not met *)
type pnm_kind = Pnm_bounds | Pnm_dangling

let desc_precondition_not_met kind proc_name loc =
  let tags = Tags.create () in
  let kind_str =
    match kind with
    | None
     -> []
    | Some Pnm_bounds
     -> ["possible array out of bounds"]
    | Some Pnm_dangling
     -> ["possible dangling pointer dereference"]
  in
  { no_desc with
    descriptions= kind_str @ [("in " ^ call_to_at_line tags proc_name loc)]; tags= !tags }

let desc_null_test_after_dereference expr_str line loc =
  let tags = Tags.create () in
  Tags.update tags Tags.dereferenced_line (string_of_int line) ;
  Tags.update tags Tags.value expr_str ;
  let description =
    Format.asprintf "Pointer %a was dereferenced at line %d and is tested for null %s"
      MF.pp_monospaced expr_str line (at_line tags loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}

let desc_return_expression_required typ_str loc =
  let tags = Tags.create () in
  Tags.update tags Tags.value typ_str ;
  let description =
    Format.sprintf "Return statement requires an expression of type %s %s" typ_str
      (at_line tags loc)
  in
  {no_desc with descriptions= [description]; tags= !tags}

let desc_retain_cycle cycle loc cycle_dotty =
  Logging.d_strln "Proposition with retain cycle:" ;
  let ct = ref 1 in
  let tags = Tags.create () in
  let str_cycle = ref "" in
  let remove_old s =
    match Str.split_delim (Str.regexp_string "&old_") s with [_; s'] -> s' | _ -> s
  in
  let do_edge ((se, _), f, _) =
    match se with
    | Sil.Eexp (Exp.Lvar pvar, _) when Pvar.equal pvar Sil.block_pvar
     -> str_cycle
        := !str_cycle ^ " (" ^ string_of_int !ct ^ ") a block capturing "
           ^ MF.monospaced_to_string (Typ.Fieldname.to_string f) ^ "; " ;
        ct := !ct + 1
    | Sil.Eexp ((Exp.Lvar pvar as e), _)
     -> let e_str = Exp.to_string e in
        let e_str = if Pvar.is_seed pvar then remove_old e_str else e_str in
        str_cycle
        := !str_cycle ^ " (" ^ string_of_int !ct ^ ") object " ^ e_str ^ " retaining "
           ^ MF.monospaced_to_string (e_str ^ "." ^ Typ.Fieldname.to_string f) ^ ", " ;
        ct := !ct + 1
    | Sil.Eexp (Exp.Sizeof {typ}, _)
     -> let step =
          " (" ^ string_of_int !ct ^ ") an object of "
          ^ MF.monospaced_to_string (Typ.to_string typ)
          ^ " retaining another object via instance variable "
          ^ MF.monospaced_to_string (Typ.Fieldname.to_string f) ^ ", "
        in
        str_cycle := !str_cycle ^ step ;
        ct := !ct + 1
    | _
     -> ()
  in
  List.iter ~f:do_edge cycle ;
  let desc =
    Format.sprintf "Retain cycle involving the following objects: %s  %s" !str_cycle
      (at_line tags loc)
  in
  {no_desc with descriptions= [desc]; tags= !tags; dotty= cycle_dotty}

let registered_observer_being_deallocated_str obj_str =
  "Object " ^ obj_str
  ^ " is registered in a notification center but not being removed before deallocation"

let desc_registered_observer_being_deallocated pvar loc =
  let tags = Tags.create () in
  let obj_str = MF.monospaced_to_string (Pvar.to_string pvar) in
  { no_desc with
    descriptions=
      [ ( registered_observer_being_deallocated_str obj_str ^ at_line tags loc
        ^ ". Being still registered as observer of the notification "
        ^ "center, the deallocated object " ^ obj_str ^ " may be notified in the future." ) ]
  ; tags= !tags }

let desc_return_statement_missing loc =
  let tags = Tags.create () in
  {no_desc with descriptions= [("Return statement missing " ^ at_line tags loc)]; tags= !tags}

let desc_return_value_ignored proc_name loc =
  let tags = Tags.create () in
  {no_desc with descriptions= [("after " ^ call_to_at_line tags proc_name loc)]; tags= !tags}

let desc_unary_minus_applied_to_unsigned_expression expr_str_opt typ_str loc =
  let tags = Tags.create () in
  let expression =
    match expr_str_opt with
    | Some s
     -> Tags.update tags Tags.value s ; "expression " ^ s
    | None
     -> "an expression"
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

let desc_stack_variable_address_escape expr_str addr_dexp_str loc =
  let tags = Tags.create () in
  Tags.update tags Tags.value expr_str ;
  let escape_to_str =
    match addr_dexp_str with
    | Some s
     -> Tags.update tags Tags.escape_to s ;
        "to " ^ s ^ " "
    | None
     -> ""
  in
  let description =
    Format.asprintf "Address of stack variable %a escapes %s%s" MF.pp_monospaced expr_str
      escape_to_str (at_line tags loc)
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
