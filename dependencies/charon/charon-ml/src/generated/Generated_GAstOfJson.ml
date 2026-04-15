(** WARNING: this file is partially auto-generated. Do not edit `GAstOfJson.ml`
    by hand. Edit `GAstOfJson.template.ml` instead, or improve the code
    generation tool so avoid the need for hand-writing things.

    `GAstOfJson.template.ml` contains the manual definitions and some `(*
    __REPLACEn__ *)` comments. These comments are replaced by auto-generated
    definitions by running `make generate-ml` in the crate root. The
    code-generation code is in `charon/src/bin/generate-ml`. *)

open Yojson.Basic
open OfJsonBasic
open Identifiers
open Meta
open Values
open Types
open Scalars
open Expressions
open GAst
module FileId = IdGen ()
module HashConsId = IdGen ()

(** The default logger *)
let log = Logging.llbc_of_json_logger

type id_to_file_map = file FileId.Map.t

type of_json_ctx = {
  id_to_file_map : id_to_file_map;
  ty_hashcons_map : ty HashConsId.Map.t ref;
  tref_hashcons_map : trait_ref HashConsId.Map.t ref;
}

let empty_of_json_ctx : of_json_ctx =
  {
    id_to_file_map = FileId.Map.empty;
    ty_hashcons_map = ref HashConsId.Map.empty;
    tref_hashcons_map = ref HashConsId.Map.empty;
  }

let hash_consed_val_of_json (map : 'a HashConsId.Map.t ref)
    (of_json : of_json_ctx -> json -> ('a, string) result) (ctx : of_json_ctx)
    (js : json) : ('a, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Untagged", json) ] -> of_json ctx json
    | `Assoc [ ("HashConsedValue", `List [ `Int id; json ]) ] ->
        let* v = of_json ctx json in
        let id = HashConsId.of_int id in
        map := HashConsId.Map.add id v !map;
        Ok v
    | `Assoc [ ("Deduplicated", `Int id) ] -> begin
        let id = HashConsId.of_int id in
        match HashConsId.Map.find_opt id !map with
        | Some v -> Ok v
        | None ->
            Error
              "Hash-consing key not found; there is a serialization mismatch \
               between Rust and OCaml"
      end
    | _ -> Error "")

let path_buf_of_json = string_of_json

let big_int_of_json _ (js : json) : (big_int, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Int i -> Ok (Z.of_int i)
    | `String is -> Ok (Z.of_string is)
    | _ -> Error "")

let rec ___ = ()

and abort_kind_of_json (ctx : of_json_ctx) (js : json) :
    (abort_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Panic", panic) ] ->
        let* panic = option_of_json name_of_json ctx panic in
        Ok (Panic panic)
    | `String "UndefinedBehavior" -> Ok UndefinedBehavior
    | `String "UnwindTerminate" -> Ok UnwindTerminate
    | _ -> Error "")

and aggregate_kind_of_json (ctx : of_json_ctx) (js : json) :
    (aggregate_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Adt", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = type_decl_ref_of_json ctx x_0 in
        let* x_1 = option_of_json variant_id_of_json ctx x_1 in
        let* x_2 = option_of_json field_id_of_json ctx x_2 in
        Ok (AggregatedAdt (x_0, x_1, x_2))
    | `Assoc [ ("Array", `List [ x_0; x_1 ]) ] ->
        let* x_0 = ty_of_json ctx x_0 in
        let* x_1 = box_of_json constant_expr_of_json ctx x_1 in
        Ok (AggregatedArray (x_0, x_1))
    | `Assoc [ ("RawPtr", `List [ x_0; x_1 ]) ] ->
        let* x_0 = ty_of_json ctx x_0 in
        let* x_1 = ref_kind_of_json ctx x_1 in
        Ok (AggregatedRawPtr (x_0, x_1))
    | _ -> Error "")

and alignment_modifier_of_json (ctx : of_json_ctx) (js : json) :
    (alignment_modifier, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Align", align) ] ->
        let* align = int_of_json ctx align in
        Ok (Align align)
    | `Assoc [ ("Pack", pack) ] ->
        let* pack = int_of_json ctx pack in
        Ok (Pack pack)
    | _ -> Error "")

and assertion_of_json (ctx : of_json_ctx) (js : json) :
    (assertion, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [ ("cond", cond); ("expected", expected); ("check_kind", check_kind) ]
      ->
        let* cond = operand_of_json ctx cond in
        let* expected = bool_of_json ctx expected in
        let* check_kind =
          option_of_json builtin_assert_kind_of_json ctx check_kind
        in
        Ok ({ cond; expected; check_kind } : assertion)
    | _ -> Error "")

and attr_info_of_json (ctx : of_json_ctx) (js : json) :
    (attr_info, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("attributes", attributes);
          ("inline", inline);
          ("rename", rename);
          ("public", public);
        ] ->
        let* attributes = list_of_json attribute_of_json ctx attributes in
        let* inline = option_of_json inline_attr_of_json ctx inline in
        let* rename = option_of_json string_of_json ctx rename in
        let* public = bool_of_json ctx public in
        Ok ({ attributes; inline; rename; public } : attr_info)
    | _ -> Error "")

and attribute_of_json (ctx : of_json_ctx) (js : json) :
    (attribute, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Opaque" -> Ok AttrOpaque
    | `String "Exclude" -> Ok AttrExclude
    | `Assoc [ ("Rename", rename) ] ->
        let* rename = string_of_json ctx rename in
        Ok (AttrRename rename)
    | `Assoc [ ("VariantsPrefix", variants_prefix) ] ->
        let* variants_prefix = string_of_json ctx variants_prefix in
        Ok (AttrVariantsPrefix variants_prefix)
    | `Assoc [ ("VariantsSuffix", variants_suffix) ] ->
        let* variants_suffix = string_of_json ctx variants_suffix in
        Ok (AttrVariantsSuffix variants_suffix)
    | `Assoc [ ("DocComment", doc_comment) ] ->
        let* doc_comment = string_of_json ctx doc_comment in
        Ok (AttrDocComment doc_comment)
    | `Assoc [ ("Unknown", unknown) ] ->
        let* unknown = raw_attribute_of_json ctx unknown in
        Ok (AttrUnknown unknown)
    | _ -> Error "")

and binop_of_json (ctx : of_json_ctx) (js : json) : (binop, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "BitXor" -> Ok BitXor
    | `String "BitAnd" -> Ok BitAnd
    | `String "BitOr" -> Ok BitOr
    | `String "Eq" -> Ok Eq
    | `String "Lt" -> Ok Lt
    | `String "Le" -> Ok Le
    | `String "Ne" -> Ok Ne
    | `String "Ge" -> Ok Ge
    | `String "Gt" -> Ok Gt
    | `Assoc [ ("Add", add) ] ->
        let* add = overflow_mode_of_json ctx add in
        Ok (Add add)
    | `Assoc [ ("Sub", sub) ] ->
        let* sub = overflow_mode_of_json ctx sub in
        Ok (Sub sub)
    | `Assoc [ ("Mul", mul) ] ->
        let* mul = overflow_mode_of_json ctx mul in
        Ok (Mul mul)
    | `Assoc [ ("Div", div) ] ->
        let* div = overflow_mode_of_json ctx div in
        Ok (Div div)
    | `Assoc [ ("Rem", rem) ] ->
        let* rem = overflow_mode_of_json ctx rem in
        Ok (Rem rem)
    | `String "AddChecked" -> Ok AddChecked
    | `String "SubChecked" -> Ok SubChecked
    | `String "MulChecked" -> Ok MulChecked
    | `Assoc [ ("Shl", shl) ] ->
        let* shl = overflow_mode_of_json ctx shl in
        Ok (Shl shl)
    | `Assoc [ ("Shr", shr) ] ->
        let* shr = overflow_mode_of_json ctx shr in
        Ok (Shr shr)
    | `String "Offset" -> Ok Offset
    | `String "Cmp" -> Ok Cmp
    | _ -> Error "")

and binder_of_json :
    'a0.
    (of_json_ctx -> json -> ('a0, string) result) ->
    of_json_ctx ->
    json ->
    ('a0 binder, string) result =
 fun arg0_of_json ctx js ->
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("params", params); ("skip_binder", skip_binder); ("kind", _) ]
      ->
        let* binder_params = generic_params_of_json ctx params in
        let* binder_value = arg0_of_json ctx skip_binder in
        Ok ({ binder_params; binder_value } : _ binder)
    | _ -> Error "")

and binder_kind_of_json (ctx : of_json_ctx) (js : json) :
    (binder_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("TraitType", `List [ x_0; x_1 ]) ] ->
        let* x_0 = trait_decl_id_of_json ctx x_0 in
        let* x_1 = trait_item_name_of_json ctx x_1 in
        Ok (BKTraitType (x_0, x_1))
    | `Assoc [ ("TraitMethod", `List [ x_0; x_1 ]) ] ->
        let* x_0 = trait_decl_id_of_json ctx x_0 in
        let* x_1 = trait_item_name_of_json ctx x_1 in
        Ok (BKTraitMethod (x_0, x_1))
    | `String "InherentImplBlock" -> Ok BKInherentImplBlock
    | `String "Dyn" -> Ok BKDyn
    | `String "Other" -> Ok BKOther
    | _ -> Error "")

and borrow_kind_of_json (ctx : of_json_ctx) (js : json) :
    (borrow_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Shared" -> Ok BShared
    | `String "Mut" -> Ok BMut
    | `String "TwoPhaseMut" -> Ok BTwoPhaseMut
    | `String "Shallow" -> Ok BShallow
    | `String "UniqueImmutable" -> Ok BUniqueImmutable
    | _ -> Error "")

and builtin_assert_kind_of_json (ctx : of_json_ctx) (js : json) :
    (builtin_assert_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("BoundsCheck", `Assoc [ ("len", len); ("index", index) ]) ] ->
        let* len = operand_of_json ctx len in
        let* index = operand_of_json ctx index in
        Ok (BoundsCheck (len, index))
    | `Assoc [ ("Overflow", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = binop_of_json ctx x_0 in
        let* x_1 = operand_of_json ctx x_1 in
        let* x_2 = operand_of_json ctx x_2 in
        Ok (Overflow (x_0, x_1, x_2))
    | `Assoc [ ("OverflowNeg", overflow_neg) ] ->
        let* overflow_neg = operand_of_json ctx overflow_neg in
        Ok (OverflowNeg overflow_neg)
    | `Assoc [ ("DivisionByZero", division_by_zero) ] ->
        let* division_by_zero = operand_of_json ctx division_by_zero in
        Ok (DivisionByZero division_by_zero)
    | `Assoc [ ("RemainderByZero", remainder_by_zero) ] ->
        let* remainder_by_zero = operand_of_json ctx remainder_by_zero in
        Ok (RemainderByZero remainder_by_zero)
    | `Assoc
        [
          ( "MisalignedPointerDereference",
            `Assoc [ ("required", required); ("found", found) ] );
        ] ->
        let* required = operand_of_json ctx required in
        let* found = operand_of_json ctx found in
        Ok (MisalignedPointerDereference (required, found))
    | `String "NullPointerDereference" -> Ok NullPointerDereference
    | `Assoc [ ("InvalidEnumConstruction", invalid_enum_construction) ] ->
        let* invalid_enum_construction =
          operand_of_json ctx invalid_enum_construction
        in
        Ok (InvalidEnumConstruction invalid_enum_construction)
    | _ -> Error "")

and builtin_fun_id_of_json (ctx : of_json_ctx) (js : json) :
    (builtin_fun_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "BoxNew" -> Ok BoxNew
    | `String "ArrayToSliceShared" -> Ok ArrayToSliceShared
    | `String "ArrayToSliceMut" -> Ok ArrayToSliceMut
    | `String "ArrayRepeat" -> Ok ArrayRepeat
    | `Assoc [ ("Index", index) ] ->
        let* index = builtin_index_op_of_json ctx index in
        Ok (Index index)
    | `Assoc [ ("PtrFromParts", ptr_from_parts) ] ->
        let* ptr_from_parts = ref_kind_of_json ctx ptr_from_parts in
        Ok (PtrFromParts ptr_from_parts)
    | _ -> Error "")

and builtin_impl_data_of_json (ctx : of_json_ctx) (js : json) :
    (builtin_impl_data, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Sized" -> Ok BuiltinSized
    | `String "MetaSized" -> Ok BuiltinMetaSized
    | `String "Tuple" -> Ok BuiltinTuple
    | `String "Pointee" -> Ok BuiltinPointee
    | `String "DiscriminantKind" -> Ok BuiltinDiscriminantKind
    | `String "Auto" -> Ok BuiltinAuto
    | `String "NoopDestruct" -> Ok BuiltinNoopDestruct
    | `String "UntrackedDestruct" -> Ok BuiltinUntrackedDestruct
    | `String "Fn" -> Ok BuiltinFn
    | `String "FnMut" -> Ok BuiltinFnMut
    | `String "FnOnce" -> Ok BuiltinFnOnce
    | `String "Copy" -> Ok BuiltinCopy
    | `String "Clone" -> Ok BuiltinClone
    | _ -> Error "")

and builtin_index_op_of_json (ctx : of_json_ctx) (js : json) :
    (builtin_index_op, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("is_array", is_array);
          ("mutability", mutability);
          ("is_range", is_range);
        ] ->
        let* is_array = bool_of_json ctx is_array in
        let* mutability = ref_kind_of_json ctx mutability in
        let* is_range = bool_of_json ctx is_range in
        Ok ({ is_array; mutability; is_range } : builtin_index_op)
    | _ -> Error "")

and builtin_ty_of_json (ctx : of_json_ctx) (js : json) :
    (builtin_ty, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Box" -> Ok TBox
    | `String "Str" -> Ok TStr
    | _ -> Error "")

and byte_of_json (ctx : of_json_ctx) (js : json) : (byte, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Uninit" -> Ok Uninit
    | `Assoc [ ("Value", value) ] ->
        let* value = int_of_json ctx value in
        Ok (Value value)
    | `Assoc [ ("Provenance", `List [ x_0; x_1 ]) ] ->
        let* x_0 = provenance_of_json ctx x_0 in
        let* x_1 = int_of_json ctx x_1 in
        Ok (Provenance (x_0, x_1))
    | _ -> Error "")

and call_of_json (ctx : of_json_ctx) (js : json) : (call, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("func", func); ("args", args); ("dest", dest) ] ->
        let* func = fn_operand_of_json ctx func in
        let* args = list_of_json operand_of_json ctx args in
        let* dest = place_of_json ctx dest in
        Ok ({ func; args; dest } : call)
    | _ -> Error "")

and cast_kind_of_json (ctx : of_json_ctx) (js : json) :
    (cast_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Scalar", `List [ x_0; x_1 ]) ] ->
        let* x_0 = literal_type_of_json ctx x_0 in
        let* x_1 = literal_type_of_json ctx x_1 in
        Ok (CastScalar (x_0, x_1))
    | `Assoc [ ("RawPtr", `List [ x_0; x_1 ]) ] ->
        let* x_0 = ty_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        Ok (CastRawPtr (x_0, x_1))
    | `Assoc [ ("FnPtr", `List [ x_0; x_1 ]) ] ->
        let* x_0 = ty_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        Ok (CastFnPtr (x_0, x_1))
    | `Assoc [ ("Unsize", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = ty_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        let* x_2 = unsizing_metadata_of_json ctx x_2 in
        Ok (CastUnsize (x_0, x_1, x_2))
    | `Assoc [ ("Transmute", `List [ x_0; x_1 ]) ] ->
        let* x_0 = ty_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        Ok (CastTransmute (x_0, x_1))
    | `Assoc [ ("Concretize", `List [ x_0; x_1 ]) ] ->
        let* x_0 = ty_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        Ok (CastConcretize (x_0, x_1))
    | _ -> Error "")

and cli_options_of_json (ctx : of_json_ctx) (js : json) :
    (cli_options, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("ullbc", ullbc);
          ("precise_drops", precise_drops);
          ("skip_borrowck", skip_borrowck);
          ("mir", mir);
          ("rustc_args", rustc_args);
          ("monomorphize", monomorphize);
          ("monomorphize_mut", monomorphize_mut);
          ("start_from", start_from);
          ("start_from_if_exists", start_from_if_exists);
          ("start_from_attribute", start_from_attribute);
          ("start_from_pub", start_from_pub);
          ("include", include_);
          ("opaque", opaque);
          ("exclude", exclude);
          ("extract_opaque_bodies", extract_opaque_bodies);
          ("translate_all_methods", translate_all_methods);
          ("lift_associated_types", lift_associated_types);
          ("hide_marker_traits", hide_marker_traits);
          ("remove_adt_clauses", remove_adt_clauses);
          ("hide_allocator", hide_allocator);
          ("remove_unused_self_clauses", remove_unused_self_clauses);
          ("desugar_drops", desugar_drops);
          ("ops_to_function_calls", ops_to_function_calls);
          ("index_to_function_calls", index_to_function_calls);
          ("treat_box_as_builtin", treat_box_as_builtin);
          ("raw_consts", raw_consts);
          ("unsized_strings", unsized_strings);
          ("reconstruct_fallible_operations", reconstruct_fallible_operations);
          ("reconstruct_asserts", reconstruct_asserts);
          ("unbind_item_vars", unbind_item_vars);
          ("print_original_ullbc", print_original_ullbc);
          ("print_ullbc", print_ullbc);
          ("print_built_llbc", print_built_llbc);
          ("print_llbc", print_llbc);
          ("dest_dir", dest_dir);
          ("dest_file", dest_file);
          ("no_dedup_serialized_ast", no_dedup_serialized_ast);
          ("no_serialize", no_serialize);
          ("abort_on_error", abort_on_error);
          ("error_on_warnings", error_on_warnings);
          ("preset", preset);
        ] ->
        let* ullbc = bool_of_json ctx ullbc in
        let* precise_drops = bool_of_json ctx precise_drops in
        let* skip_borrowck = bool_of_json ctx skip_borrowck in
        let* mir = option_of_json mir_level_of_json ctx mir in
        let* rustc_args = list_of_json string_of_json ctx rustc_args in
        let* monomorphize = bool_of_json ctx monomorphize in
        let* monomorphize_mut =
          option_of_json monomorphize_mut_of_json ctx monomorphize_mut
        in
        let* start_from = list_of_json string_of_json ctx start_from in
        let* start_from_if_exists =
          list_of_json string_of_json ctx start_from_if_exists
        in
        let* start_from_attribute =
          option_of_json string_of_json ctx start_from_attribute
        in
        let* start_from_pub = bool_of_json ctx start_from_pub in
        let* included = list_of_json string_of_json ctx include_ in
        let* opaque = list_of_json string_of_json ctx opaque in
        let* exclude = list_of_json string_of_json ctx exclude in
        let* extract_opaque_bodies = bool_of_json ctx extract_opaque_bodies in
        let* translate_all_methods = bool_of_json ctx translate_all_methods in
        let* lift_associated_types =
          list_of_json string_of_json ctx lift_associated_types
        in
        let* hide_marker_traits = bool_of_json ctx hide_marker_traits in
        let* remove_adt_clauses = bool_of_json ctx remove_adt_clauses in
        let* hide_allocator = bool_of_json ctx hide_allocator in
        let* remove_unused_self_clauses =
          bool_of_json ctx remove_unused_self_clauses
        in
        let* desugar_drops = bool_of_json ctx desugar_drops in
        let* ops_to_function_calls = bool_of_json ctx ops_to_function_calls in
        let* index_to_function_calls =
          bool_of_json ctx index_to_function_calls
        in
        let* treat_box_as_builtin = bool_of_json ctx treat_box_as_builtin in
        let* raw_consts = bool_of_json ctx raw_consts in
        let* unsized_strings = bool_of_json ctx unsized_strings in
        let* reconstruct_fallible_operations =
          bool_of_json ctx reconstruct_fallible_operations
        in
        let* reconstruct_asserts = bool_of_json ctx reconstruct_asserts in
        let* unbind_item_vars = bool_of_json ctx unbind_item_vars in
        let* print_original_ullbc = bool_of_json ctx print_original_ullbc in
        let* print_ullbc = bool_of_json ctx print_ullbc in
        let* print_built_llbc = bool_of_json ctx print_built_llbc in
        let* print_llbc = bool_of_json ctx print_llbc in
        let* dest_dir = option_of_json path_buf_of_json ctx dest_dir in
        let* dest_file = option_of_json path_buf_of_json ctx dest_file in
        let* no_dedup_serialized_ast =
          bool_of_json ctx no_dedup_serialized_ast
        in
        let* no_serialize = bool_of_json ctx no_serialize in
        let* abort_on_error = bool_of_json ctx abort_on_error in
        let* error_on_warnings = bool_of_json ctx error_on_warnings in
        let* preset = option_of_json preset_of_json ctx preset in
        Ok
          ({
             ullbc;
             precise_drops;
             skip_borrowck;
             mir;
             rustc_args;
             monomorphize;
             monomorphize_mut;
             start_from;
             start_from_if_exists;
             start_from_attribute;
             start_from_pub;
             included;
             opaque;
             exclude;
             extract_opaque_bodies;
             translate_all_methods;
             lift_associated_types;
             hide_marker_traits;
             remove_adt_clauses;
             hide_allocator;
             remove_unused_self_clauses;
             desugar_drops;
             ops_to_function_calls;
             index_to_function_calls;
             treat_box_as_builtin;
             raw_consts;
             unsized_strings;
             reconstruct_fallible_operations;
             reconstruct_asserts;
             unbind_item_vars;
             print_original_ullbc;
             print_ullbc;
             print_built_llbc;
             print_llbc;
             dest_dir;
             dest_file;
             no_dedup_serialized_ast;
             no_serialize;
             abort_on_error;
             error_on_warnings;
             preset;
           }
            : cli_options)
    | _ -> Error "")

and closure_info_of_json (ctx : of_json_ctx) (js : json) :
    (closure_info, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("kind", kind);
          ("fn_once_impl", fn_once_impl);
          ("fn_mut_impl", fn_mut_impl);
          ("fn_impl", fn_impl);
          ("signature", signature);
        ] ->
        let* kind = closure_kind_of_json ctx kind in
        let* fn_once_impl =
          region_binder_of_json trait_impl_ref_of_json ctx fn_once_impl
        in
        let* fn_mut_impl =
          option_of_json
            (region_binder_of_json trait_impl_ref_of_json)
            ctx fn_mut_impl
        in
        let* fn_impl =
          option_of_json
            (region_binder_of_json trait_impl_ref_of_json)
            ctx fn_impl
        in
        let* signature = region_binder_of_json fun_sig_of_json ctx signature in
        Ok
          ({ kind; fn_once_impl; fn_mut_impl; fn_impl; signature }
            : closure_info)
    | _ -> Error "")

and closure_kind_of_json (ctx : of_json_ctx) (js : json) :
    (closure_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Fn" -> Ok Fn
    | `String "FnMut" -> Ok FnMut
    | `String "FnOnce" -> Ok FnOnce
    | _ -> Error "")

and const_generic_param_of_json (ctx : of_json_ctx) (js : json) :
    (const_generic_param, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("index", index); ("name", name); ("ty", ty) ] ->
        let* index = const_generic_var_id_of_json ctx index in
        let* name = string_of_json ctx name in
        let* ty = ty_of_json ctx ty in
        Ok ({ index; name; ty } : const_generic_param)
    | _ -> Error "")

and const_generic_var_id_of_json (ctx : of_json_ctx) (js : json) :
    (const_generic_var_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> ConstGenericVarId.id_of_json ctx x
    | _ -> Error "")

and constant_expr_of_json (ctx : of_json_ctx) (js : json) :
    (constant_expr, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("kind", kind); ("ty", ty) ] ->
        let* kind = constant_expr_kind_of_json ctx kind in
        let* ty = ty_of_json ctx ty in
        Ok ({ kind; ty } : constant_expr)
    | _ -> Error "")

and constant_expr_kind_of_json (ctx : of_json_ctx) (js : json) :
    (constant_expr_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Literal", literal) ] ->
        let* literal = literal_of_json ctx literal in
        Ok (CLiteral literal)
    | `Assoc [ ("Adt", `List [ x_0; x_1 ]) ] ->
        let* x_0 = option_of_json variant_id_of_json ctx x_0 in
        let* x_1 = list_of_json constant_expr_of_json ctx x_1 in
        Ok (CAdt (x_0, x_1))
    | `Assoc [ ("Array", array) ] ->
        let* array = list_of_json constant_expr_of_json ctx array in
        Ok (CArray array)
    | `Assoc [ ("Global", global) ] ->
        let* global = global_decl_ref_of_json ctx global in
        Ok (CGlobal global)
    | `Assoc [ ("TraitConst", `List [ x_0; x_1 ]) ] ->
        let* x_0 = trait_ref_of_json ctx x_0 in
        let* x_1 = trait_item_name_of_json ctx x_1 in
        Ok (CTraitConst (x_0, x_1))
    | `Assoc [ ("VTableRef", v_table_ref) ] ->
        let* v_table_ref = trait_ref_of_json ctx v_table_ref in
        Ok (CVTableRef v_table_ref)
    | `Assoc [ ("Ref", `List [ x_0; x_1 ]) ] ->
        let* x_0 = box_of_json constant_expr_of_json ctx x_0 in
        let* x_1 = option_of_json unsizing_metadata_of_json ctx x_1 in
        Ok (CRef (x_0, x_1))
    | `Assoc [ ("Ptr", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = ref_kind_of_json ctx x_0 in
        let* x_1 = box_of_json constant_expr_of_json ctx x_1 in
        let* x_2 = option_of_json unsizing_metadata_of_json ctx x_2 in
        Ok (CPtr (x_0, x_1, x_2))
    | `Assoc [ ("Var", var) ] ->
        let* var = de_bruijn_var_of_json const_generic_var_id_of_json ctx var in
        Ok (CVar var)
    | `Assoc [ ("FnDef", fn_def) ] ->
        let* fn_def = fn_ptr_of_json ctx fn_def in
        Ok (CFnDef fn_def)
    | `Assoc [ ("FnPtr", fn_ptr) ] ->
        let* fn_ptr = fn_ptr_of_json ctx fn_ptr in
        Ok (CFnPtr fn_ptr)
    | `Assoc [ ("PtrNoProvenance", ptr_no_provenance) ] ->
        let* ptr_no_provenance = big_int_of_json ctx ptr_no_provenance in
        Ok (CPtrNoProvenance ptr_no_provenance)
    | `Assoc [ ("RawMemory", raw_memory) ] ->
        let* raw_memory = list_of_json byte_of_json ctx raw_memory in
        Ok (CRawMemory raw_memory)
    | `Assoc [ ("Opaque", opaque) ] ->
        let* opaque = string_of_json ctx opaque in
        Ok (COpaque opaque)
    | _ -> Error "")

and copy_non_overlapping_of_json (ctx : of_json_ctx) (js : json) :
    (copy_non_overlapping, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("src", src); ("dst", dst); ("count", count) ] ->
        let* src = operand_of_json ctx src in
        let* dst = operand_of_json ctx dst in
        let* count = operand_of_json ctx count in
        Ok ({ src; dst; count } : copy_non_overlapping)
    | _ -> Error "")

and de_bruijn_id_of_json (ctx : of_json_ctx) (js : json) :
    (de_bruijn_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> int_of_json ctx x
    | _ -> Error "")

and de_bruijn_var_of_json :
    'a0.
    (of_json_ctx -> json -> ('a0, string) result) ->
    of_json_ctx ->
    json ->
    ('a0 de_bruijn_var, string) result =
 fun arg0_of_json ctx js ->
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Bound", `List [ x_0; x_1 ]) ] ->
        let* x_0 = de_bruijn_id_of_json ctx x_0 in
        let* x_1 = arg0_of_json ctx x_1 in
        Ok (Bound (x_0, x_1))
    | `Assoc [ ("Free", free) ] ->
        let* free = arg0_of_json ctx free in
        Ok (Free free)
    | _ -> Error "")

and declaration_group_of_json (ctx : of_json_ctx) (js : json) :
    (declaration_group, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Type", type_) ] ->
        let* type_ =
          g_declaration_group_of_json type_decl_id_of_json ctx type_
        in
        Ok (TypeGroup type_)
    | `Assoc [ ("Fun", fun_) ] ->
        let* fun_ = g_declaration_group_of_json fun_decl_id_of_json ctx fun_ in
        Ok (FunGroup fun_)
    | `Assoc [ ("Global", global) ] ->
        let* global =
          g_declaration_group_of_json global_decl_id_of_json ctx global
        in
        Ok (GlobalGroup global)
    | `Assoc [ ("TraitDecl", trait_decl) ] ->
        let* trait_decl =
          g_declaration_group_of_json trait_decl_id_of_json ctx trait_decl
        in
        Ok (TraitDeclGroup trait_decl)
    | `Assoc [ ("TraitImpl", trait_impl) ] ->
        let* trait_impl =
          g_declaration_group_of_json trait_impl_id_of_json ctx trait_impl
        in
        Ok (TraitImplGroup trait_impl)
    | `Assoc [ ("Mixed", mixed) ] ->
        let* mixed = g_declaration_group_of_json item_id_of_json ctx mixed in
        Ok (MixedGroup mixed)
    | _ -> Error "")

and disambiguator_of_json (ctx : of_json_ctx) (js : json) :
    (disambiguator, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> Disambiguator.id_of_json ctx x
    | _ -> Error "")

and discriminant_layout_of_json (ctx : of_json_ctx) (js : json) :
    (discriminant_layout, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("offset", offset); ("tag_ty", tag_ty); ("encoding", encoding) ]
      ->
        let* offset = int_of_json ctx offset in
        let* tag_ty = integer_type_of_json ctx tag_ty in
        let* encoding = tag_encoding_of_json ctx encoding in
        Ok ({ offset; tag_ty; encoding } : discriminant_layout)
    | _ -> Error "")

and drop_kind_of_json (ctx : of_json_ctx) (js : json) :
    (drop_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Precise" -> Ok Precise
    | `String "Conditional" -> Ok Conditional
    | _ -> Error "")

and dyn_predicate_of_json (ctx : of_json_ctx) (js : json) :
    (dyn_predicate, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("binder", binder) ] ->
        let* binder = binder_of_json ty_of_json ctx binder in
        Ok ({ binder } : dyn_predicate)
    | _ -> Error "")

and error_of_json (ctx : of_json_ctx) (js : json) : (error, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("span", span); ("msg", msg) ] ->
        let* span = span_of_json ctx span in
        let* msg = string_of_json ctx msg in
        Ok ({ span; msg } : error)
    | _ -> Error "")

and field_of_json (ctx : of_json_ctx) (js : json) : (field, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [ ("span", span); ("attr_info", attr_info); ("name", name); ("ty", ty) ]
      ->
        let* span = span_of_json ctx span in
        let* attr_info = attr_info_of_json ctx attr_info in
        let* field_name = option_of_json string_of_json ctx name in
        let* field_ty = ty_of_json ctx ty in
        Ok ({ span; attr_info; field_name; field_ty } : field)
    | _ -> Error "")

and field_id_of_json (ctx : of_json_ctx) (js : json) : (field_id, string) result
    =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> FieldId.id_of_json ctx x
    | _ -> Error "")

and field_proj_kind_of_json (ctx : of_json_ctx) (js : json) :
    (field_proj_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Adt", `List [ x_0; x_1 ]) ] ->
        let* x_0 = type_decl_id_of_json ctx x_0 in
        let* x_1 = option_of_json variant_id_of_json ctx x_1 in
        Ok (ProjAdt (x_0, x_1))
    | `Assoc [ ("Tuple", tuple) ] ->
        let* tuple = int_of_json ctx tuple in
        Ok (ProjTuple tuple)
    | _ -> Error "")

and file_of_json (ctx : of_json_ctx) (js : json) : (file, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [ ("name", name); ("crate_name", crate_name); ("contents", contents) ]
      ->
        let* name = file_name_of_json ctx name in
        let* crate_name = string_of_json ctx crate_name in
        let* contents = option_of_json string_of_json ctx contents in
        Ok ({ name; crate_name; contents } : file)
    | _ -> Error "")

and file_id_of_json (ctx : of_json_ctx) (js : json) : (file_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | json ->
        let* file_id = FileId.id_of_json ctx json in
        let file = FileId.Map.find file_id ctx.id_to_file_map in
        Ok file
    | _ -> Error "")

and file_name_of_json (ctx : of_json_ctx) (js : json) :
    (file_name, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Virtual", virtual_) ] ->
        let* virtual_ = path_buf_of_json ctx virtual_ in
        Ok (Virtual virtual_)
    | `Assoc [ ("Local", local) ] ->
        let* local = path_buf_of_json ctx local in
        Ok (Local local)
    | `Assoc [ ("NotReal", not_real) ] ->
        let* not_real = string_of_json ctx not_real in
        Ok (NotReal not_real)
    | _ -> Error "")

and float_type_of_json (ctx : of_json_ctx) (js : json) :
    (float_type, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "F16" -> Ok F16
    | `String "F32" -> Ok F32
    | `String "F64" -> Ok F64
    | `String "F128" -> Ok F128
    | _ -> Error "")

and float_value_of_json (ctx : of_json_ctx) (js : json) :
    (float_value, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("value", value); ("ty", ty) ] ->
        let* float_value = string_of_json ctx value in
        let* float_ty = float_type_of_json ctx ty in
        Ok ({ float_value; float_ty } : float_value)
    | _ -> Error "")

and fn_operand_of_json (ctx : of_json_ctx) (js : json) :
    (fn_operand, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Regular", regular) ] ->
        let* regular = fn_ptr_of_json ctx regular in
        Ok (FnOpRegular regular)
    | `Assoc [ ("Dynamic", dynamic) ] ->
        let* dynamic = operand_of_json ctx dynamic in
        Ok (FnOpDynamic dynamic)
    | _ -> Error "")

and fn_ptr_of_json (ctx : of_json_ctx) (js : json) : (fn_ptr, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("kind", kind); ("generics", generics) ] ->
        let* kind = box_of_json fn_ptr_kind_of_json ctx kind in
        let* generics = box_of_json generic_args_of_json ctx generics in
        Ok ({ kind; generics } : fn_ptr)
    | _ -> Error "")

and fn_ptr_kind_of_json (ctx : of_json_ctx) (js : json) :
    (fn_ptr_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Fun", fun_) ] ->
        let* fun_ = fun_id_of_json ctx fun_ in
        Ok (FunId fun_)
    | `Assoc [ ("Trait", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = trait_ref_of_json ctx x_0 in
        let* x_1 = trait_item_name_of_json ctx x_1 in
        let* x_2 = fun_decl_id_of_json ctx x_2 in
        Ok (TraitMethod (x_0, x_1, x_2))
    | _ -> Error "")

and fun_decl_id_of_json (ctx : of_json_ctx) (js : json) :
    (fun_decl_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> FunDeclId.id_of_json ctx x
    | _ -> Error "")

and fun_decl_ref_of_json (ctx : of_json_ctx) (js : json) :
    (fun_decl_ref, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("id", id); ("generics", generics) ] ->
        let* id = fun_decl_id_of_json ctx id in
        let* generics = box_of_json generic_args_of_json ctx generics in
        Ok ({ id; generics } : fun_decl_ref)
    | _ -> Error "")

and fun_id_of_json (ctx : of_json_ctx) (js : json) : (fun_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Regular", regular) ] ->
        let* regular = fun_decl_id_of_json ctx regular in
        Ok (FRegular regular)
    | `Assoc [ ("Builtin", builtin) ] ->
        let* builtin = builtin_fun_id_of_json ctx builtin in
        Ok (FBuiltin builtin)
    | _ -> Error "")

and fun_sig_of_json (ctx : of_json_ctx) (js : json) : (fun_sig, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [ ("is_unsafe", is_unsafe); ("inputs", inputs); ("output", output) ] ->
        let* is_unsafe = bool_of_json ctx is_unsafe in
        let* inputs = list_of_json ty_of_json ctx inputs in
        let* output = ty_of_json ctx output in
        Ok ({ is_unsafe; inputs; output } : fun_sig)
    | _ -> Error "")

and g_declaration_group_of_json :
    'a0.
    (of_json_ctx -> json -> ('a0, string) result) ->
    of_json_ctx ->
    json ->
    ('a0 g_declaration_group, string) result =
 fun arg0_of_json ctx js ->
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("NonRec", non_rec) ] ->
        let* non_rec = arg0_of_json ctx non_rec in
        Ok (NonRecGroup non_rec)
    | `Assoc [ ("Rec", rec_) ] ->
        let* rec_ = list_of_json arg0_of_json ctx rec_ in
        Ok (RecGroup rec_)
    | _ -> Error "")

and gexpr_body_of_json :
    'a0.
    (of_json_ctx -> json -> ('a0, string) result) ->
    of_json_ctx ->
    json ->
    ('a0 gexpr_body, string) result =
 fun arg0_of_json ctx js ->
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("span", span);
          ("bound_body_regions", bound_body_regions);
          ("locals", locals);
          ("body", body);
          ("comments", _);
        ] ->
        let* span = span_of_json ctx span in
        let* bound_body_regions = int_of_json ctx bound_body_regions in
        let* locals = locals_of_json ctx locals in
        let* body = arg0_of_json ctx body in
        Ok ({ span; bound_body_regions; locals; body } : _ gexpr_body)
    | _ -> Error "")

and generic_args_of_json (ctx : of_json_ctx) (js : json) :
    (generic_args, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("regions", regions);
          ("types", types);
          ("const_generics", const_generics);
          ("trait_refs", trait_refs);
        ] ->
        let* regions =
          index_map_of_json region_id_of_json region_of_json ctx regions
        in
        let* types =
          index_map_of_json type_var_id_of_json ty_of_json ctx types
        in
        let* const_generics =
          index_map_of_json const_generic_var_id_of_json constant_expr_of_json
            ctx const_generics
        in
        let* trait_refs =
          index_map_of_json trait_clause_id_of_json trait_ref_of_json ctx
            trait_refs
        in
        Ok ({ regions; types; const_generics; trait_refs } : generic_args)
    | _ -> Error "")

and generic_params_of_json (ctx : of_json_ctx) (js : json) :
    (generic_params, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("regions", regions);
          ("types", types);
          ("const_generics", const_generics);
          ("trait_clauses", trait_clauses);
          ("regions_outlive", regions_outlive);
          ("types_outlive", types_outlive);
          ("trait_type_constraints", trait_type_constraints);
        ] ->
        let* regions =
          index_map_of_json region_id_of_json region_param_of_json ctx regions
        in
        let* types =
          index_map_of_json type_var_id_of_json type_param_of_json ctx types
        in
        let* const_generics =
          index_map_of_json const_generic_var_id_of_json
            const_generic_param_of_json ctx const_generics
        in
        let* trait_clauses =
          index_map_of_json trait_clause_id_of_json trait_param_of_json ctx
            trait_clauses
        in
        let* regions_outlive =
          list_of_json
            (region_binder_of_json
               (outlives_pred_of_json region_of_json region_of_json))
            ctx regions_outlive
        in
        let* types_outlive =
          list_of_json
            (region_binder_of_json
               (outlives_pred_of_json ty_of_json region_of_json))
            ctx types_outlive
        in
        let* trait_type_constraints =
          index_map_of_json trait_type_constraint_id_of_json
            (region_binder_of_json trait_type_constraint_of_json)
            ctx trait_type_constraints
        in
        Ok
          ({
             regions;
             types;
             const_generics;
             trait_clauses;
             regions_outlive;
             types_outlive;
             trait_type_constraints;
           }
            : generic_params)
    | _ -> Error "")

and global_decl_of_json (ctx : of_json_ctx) (js : json) :
    (global_decl, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("def_id", def_id);
          ("item_meta", item_meta);
          ("generics", generics);
          ("ty", ty);
          ("src", src);
          ("global_kind", global_kind);
          ("init", init);
        ] ->
        let* def_id = global_decl_id_of_json ctx def_id in
        let* item_meta = item_meta_of_json ctx item_meta in
        let* generics = generic_params_of_json ctx generics in
        let* ty = ty_of_json ctx ty in
        let* src = item_source_of_json ctx src in
        let* global_kind = global_kind_of_json ctx global_kind in
        let* init = fun_decl_id_of_json ctx init in
        Ok
          ({ def_id; item_meta; generics; ty; src; global_kind; init }
            : global_decl)
    | _ -> Error "")

and global_decl_id_of_json (ctx : of_json_ctx) (js : json) :
    (global_decl_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> GlobalDeclId.id_of_json ctx x
    | _ -> Error "")

and global_decl_ref_of_json (ctx : of_json_ctx) (js : json) :
    (global_decl_ref, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("id", id); ("generics", generics) ] ->
        let* id = global_decl_id_of_json ctx id in
        let* generics = box_of_json generic_args_of_json ctx generics in
        Ok ({ id; generics } : global_decl_ref)
    | _ -> Error "")

and global_kind_of_json (ctx : of_json_ctx) (js : json) :
    (global_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Static" -> Ok Static
    | `String "NamedConst" -> Ok NamedConst
    | `String "AnonConst" -> Ok AnonConst
    | _ -> Error "")

and hash_consed_of_json :
    'a0.
    (of_json_ctx -> json -> ('a0, string) result) ->
    of_json_ctx ->
    json ->
    ('a0 hash_consed, string) result =
 fun arg0_of_json ctx js ->
  combine_error_msgs js __FUNCTION__
    (match js with
    | json -> Error "use `hash_consed_val_of_json` instead"
    | _ -> Error "")

and impl_elem_of_json (ctx : of_json_ctx) (js : json) :
    (impl_elem, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Ty", ty) ] ->
        let* ty = binder_of_json ty_of_json ctx ty in
        Ok (ImplElemTy ty)
    | `Assoc [ ("Trait", trait) ] ->
        let* trait = trait_impl_id_of_json ctx trait in
        Ok (ImplElemTrait trait)
    | _ -> Error "")

and index_map_of_json :
    'a0 'a1.
    (of_json_ctx -> json -> ('a0, string) result) ->
    (of_json_ctx -> json -> ('a1, string) result) ->
    of_json_ctx ->
    json ->
    ('a1 list, string) result =
 fun arg0_of_json arg1_of_json ctx js ->
  combine_error_msgs js __FUNCTION__
    (match js with
    | json ->
        let* list = list_of_json (option_of_json arg1_of_json) ctx json in
        Ok (List.filter_map (fun x -> x) list)
    | _ -> Error "")

and index_vec_of_json :
    'a0 'a1.
    (of_json_ctx -> json -> ('a0, string) result) ->
    (of_json_ctx -> json -> ('a1, string) result) ->
    of_json_ctx ->
    json ->
    ('a1 list, string) result =
 fun arg0_of_json arg1_of_json ctx js ->
  combine_error_msgs js __FUNCTION__
    (match js with
    | json -> list_of_json arg1_of_json ctx json
    | _ -> Error "")

and inline_attr_of_json (ctx : of_json_ctx) (js : json) :
    (inline_attr, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Hint" -> Ok Hint
    | `String "Never" -> Ok Never
    | `String "Always" -> Ok Always
    | _ -> Error "")

and int_ty_of_json (ctx : of_json_ctx) (js : json) : (int_ty, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Isize" -> Ok Isize
    | `String "I8" -> Ok I8
    | `String "I16" -> Ok I16
    | `String "I32" -> Ok I32
    | `String "I64" -> Ok I64
    | `String "I128" -> Ok I128
    | _ -> Error "")

and integer_type_of_json (ctx : of_json_ctx) (js : json) :
    (integer_type, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Signed", signed) ] ->
        let* signed = int_ty_of_json ctx signed in
        Ok (Signed signed)
    | `Assoc [ ("Unsigned", unsigned) ] ->
        let* unsigned = u_int_ty_of_json ctx unsigned in
        Ok (Unsigned unsigned)
    | _ -> Error "")

and item_id_of_json (ctx : of_json_ctx) (js : json) : (item_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Type", type_) ] ->
        let* type_ = type_decl_id_of_json ctx type_ in
        Ok (IdType type_)
    | `Assoc [ ("TraitDecl", trait_decl) ] ->
        let* trait_decl = trait_decl_id_of_json ctx trait_decl in
        Ok (IdTraitDecl trait_decl)
    | `Assoc [ ("TraitImpl", trait_impl) ] ->
        let* trait_impl = trait_impl_id_of_json ctx trait_impl in
        Ok (IdTraitImpl trait_impl)
    | `Assoc [ ("Fun", fun_) ] ->
        let* fun_ = fun_decl_id_of_json ctx fun_ in
        Ok (IdFun fun_)
    | `Assoc [ ("Global", global) ] ->
        let* global = global_decl_id_of_json ctx global in
        Ok (IdGlobal global)
    | _ -> Error "")

and item_meta_of_json (ctx : of_json_ctx) (js : json) :
    (item_meta, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("name", name);
          ("span", span);
          ("source_text", source_text);
          ("attr_info", attr_info);
          ("is_local", is_local);
          ("opacity", _);
          ("lang_item", lang_item);
        ] ->
        let* name = name_of_json ctx name in
        let* span = span_of_json ctx span in
        let* source_text = option_of_json string_of_json ctx source_text in
        let* attr_info = attr_info_of_json ctx attr_info in
        let* is_local = bool_of_json ctx is_local in
        let* lang_item = option_of_json string_of_json ctx lang_item in
        Ok
          ({ name; span; source_text; attr_info; is_local; lang_item }
            : item_meta)
    | _ -> Error "")

and item_source_of_json (ctx : of_json_ctx) (js : json) :
    (item_source, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "TopLevel" -> Ok TopLevelItem
    | `Assoc [ ("Closure", `Assoc [ ("info", info) ]) ] ->
        let* info = closure_info_of_json ctx info in
        Ok (ClosureItem info)
    | `Assoc
        [
          ( "TraitDecl",
            `Assoc
              [
                ("trait_ref", trait_ref);
                ("item_name", item_name);
                ("has_default", has_default);
              ] );
        ] ->
        let* trait_ref = trait_decl_ref_of_json ctx trait_ref in
        let* item_name = trait_item_name_of_json ctx item_name in
        let* has_default = bool_of_json ctx has_default in
        Ok (TraitDeclItem (trait_ref, item_name, has_default))
    | `Assoc
        [
          ( "TraitImpl",
            `Assoc
              [
                ("impl_ref", impl_ref);
                ("trait_ref", trait_ref);
                ("item_name", item_name);
                ("reuses_default", reuses_default);
              ] );
        ] ->
        let* impl_ref = trait_impl_ref_of_json ctx impl_ref in
        let* trait_ref = trait_decl_ref_of_json ctx trait_ref in
        let* item_name = trait_item_name_of_json ctx item_name in
        let* reuses_default = bool_of_json ctx reuses_default in
        Ok (TraitImplItem (impl_ref, trait_ref, item_name, reuses_default))
    | `Assoc
        [
          ( "VTableTy",
            `Assoc
              [
                ("dyn_predicate", dyn_predicate);
                ("field_map", field_map);
                ("supertrait_map", supertrait_map);
              ] );
        ] ->
        let* dyn_predicate = dyn_predicate_of_json ctx dyn_predicate in
        let* field_map =
          index_vec_of_json field_id_of_json v_table_field_of_json ctx field_map
        in
        let* supertrait_map =
          index_map_of_json trait_clause_id_of_json
            (option_of_json field_id_of_json)
            ctx supertrait_map
        in
        Ok (VTableTyItem (dyn_predicate, field_map, supertrait_map))
    | `Assoc [ ("VTableInstance", `Assoc [ ("impl_ref", impl_ref) ]) ] ->
        let* impl_ref = trait_impl_ref_of_json ctx impl_ref in
        Ok (VTableInstanceItem impl_ref)
    | `String "VTableMethodShim" -> Ok VTableMethodShimItem
    | `String "VTableInstanceMono" -> Ok VTableInstanceMonoItem
    | `Assoc [ ("VTableMethodPreShim", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = trait_decl_id_of_json ctx x_0 in
        let* x_1 = trait_item_name_of_json ctx x_1 in
        let* x_2 = list_of_json ty_of_json ctx x_2 in
        Ok (VTableMethodPreShimItem (x_0, x_1, x_2))
    | _ -> Error "")

and layout_of_json (ctx : of_json_ctx) (js : json) : (layout, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("size", size);
          ("align", align);
          ("discriminant_layout", discriminant_layout);
          ("uninhabited", uninhabited);
          ("variant_layouts", variant_layouts);
        ] ->
        let* size = option_of_json int_of_json ctx size in
        let* align = option_of_json int_of_json ctx align in
        let* discriminant_layout =
          option_of_json discriminant_layout_of_json ctx discriminant_layout
        in
        let* uninhabited = bool_of_json ctx uninhabited in
        let* variant_layouts =
          index_vec_of_json variant_id_of_json variant_layout_of_json ctx
            variant_layouts
        in
        Ok
          ({ size; align; discriminant_layout; uninhabited; variant_layouts }
            : layout)
    | _ -> Error "")

and lifetime_mutability_of_json (ctx : of_json_ctx) (js : json) :
    (lifetime_mutability, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Mutable" -> Ok LtMutable
    | `String "Shared" -> Ok LtShared
    | `String "Unknown" -> Ok LtUnknown
    | _ -> Error "")

and literal_of_json (ctx : of_json_ctx) (js : json) : (literal, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Scalar", scalar) ] ->
        let* scalar = scalar_value_of_json ctx scalar in
        Ok (VScalar scalar)
    | `Assoc [ ("Float", float_) ] ->
        let* float_ = float_value_of_json ctx float_ in
        Ok (VFloat float_)
    | `Assoc [ ("Bool", bool_) ] ->
        let* bool_ = bool_of_json ctx bool_ in
        Ok (VBool bool_)
    | `Assoc [ ("Char", char_) ] ->
        let* char_ = char_of_json ctx char_ in
        Ok (VChar char_)
    | `Assoc [ ("ByteStr", byte_str) ] ->
        let* byte_str = list_of_json int_of_json ctx byte_str in
        Ok (VByteStr byte_str)
    | `Assoc [ ("Str", str) ] ->
        let* str = string_of_json ctx str in
        Ok (VStr str)
    | _ -> Error "")

and literal_type_of_json (ctx : of_json_ctx) (js : json) :
    (literal_type, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Int", int) ] ->
        let* int = int_ty_of_json ctx int in
        Ok (TInt int)
    | `Assoc [ ("UInt", u_int) ] ->
        let* u_int = u_int_ty_of_json ctx u_int in
        Ok (TUInt u_int)
    | `Assoc [ ("Float", float_) ] ->
        let* float_ = float_type_of_json ctx float_ in
        Ok (TFloat float_)
    | `String "Bool" -> Ok TBool
    | `String "Char" -> Ok TChar
    | _ -> Error "")

and loc_of_json (ctx : of_json_ctx) (js : json) : (loc, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("line", line); ("col", col) ] ->
        let* line = int_of_json ctx line in
        let* col = int_of_json ctx col in
        Ok ({ line; col } : loc)
    | _ -> Error "")

and local_of_json (ctx : of_json_ctx) (js : json) : (local, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("index", index); ("name", name); ("span", span); ("ty", ty) ] ->
        let* index = local_id_of_json ctx index in
        let* name = option_of_json string_of_json ctx name in
        let* span = span_of_json ctx span in
        let* local_ty = ty_of_json ctx ty in
        Ok ({ index; name; span; local_ty } : local)
    | _ -> Error "")

and local_id_of_json (ctx : of_json_ctx) (js : json) : (local_id, string) result
    =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> LocalId.id_of_json ctx x
    | _ -> Error "")

and locals_of_json (ctx : of_json_ctx) (js : json) : (locals, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("arg_count", arg_count); ("locals", locals) ] ->
        let* arg_count = int_of_json ctx arg_count in
        let* locals =
          index_vec_of_json local_id_of_json local_of_json ctx locals
        in
        Ok ({ arg_count; locals } : locals)
    | _ -> Error "")

and mir_level_of_json (ctx : of_json_ctx) (js : json) :
    (mir_level, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Built" -> Ok Built
    | `String "Promoted" -> Ok Promoted
    | `String "Elaborated" -> Ok Elaborated
    | `String "Optimized" -> Ok Optimized
    | _ -> Error "")

and monomorphize_mut_of_json (ctx : of_json_ctx) (js : json) :
    (monomorphize_mut, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "All" -> Ok All
    | `String "ExceptTypes" -> Ok ExceptTypes
    | _ -> Error "")

and name_of_json (ctx : of_json_ctx) (js : json) : (name, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> list_of_json path_elem_of_json ctx x
    | _ -> Error "")

and nullop_of_json (ctx : of_json_ctx) (js : json) : (nullop, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "SizeOf" -> Ok SizeOf
    | `String "AlignOf" -> Ok AlignOf
    | `Assoc [ ("OffsetOf", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = type_decl_ref_of_json ctx x_0 in
        let* x_1 = option_of_json variant_id_of_json ctx x_1 in
        let* x_2 = field_id_of_json ctx x_2 in
        Ok (OffsetOf (x_0, x_1, x_2))
    | `String "UbChecks" -> Ok UbChecks
    | `String "OverflowChecks" -> Ok OverflowChecks
    | `String "ContractChecks" -> Ok ContractChecks
    | _ -> Error "")

and operand_of_json (ctx : of_json_ctx) (js : json) : (operand, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Copy", copy) ] ->
        let* copy = place_of_json ctx copy in
        Ok (Copy copy)
    | `Assoc [ ("Move", move) ] ->
        let* move = place_of_json ctx move in
        Ok (Move move)
    | `Assoc [ ("Const", const) ] ->
        let* const = box_of_json constant_expr_of_json ctx const in
        Ok (Constant const)
    | _ -> Error "")

and outlives_pred_of_json :
    'a0 'a1.
    (of_json_ctx -> json -> ('a0, string) result) ->
    (of_json_ctx -> json -> ('a1, string) result) ->
    of_json_ctx ->
    json ->
    (('a0, 'a1) outlives_pred, string) result =
 fun arg0_of_json arg1_of_json ctx js ->
  combine_error_msgs js __FUNCTION__
    (match js with
    | `List [ x_0; x_1 ] ->
        let* x_0 = arg0_of_json ctx x_0 in
        let* x_1 = arg1_of_json ctx x_1 in
        Ok (x_0, x_1)
    | _ -> Error "")

and overflow_mode_of_json (ctx : of_json_ctx) (js : json) :
    (overflow_mode, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Panic" -> Ok OPanic
    | `String "UB" -> Ok OUB
    | `String "Wrap" -> Ok OWrap
    | _ -> Error "")

and path_elem_of_json (ctx : of_json_ctx) (js : json) :
    (path_elem, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Ident", `List [ x_0; x_1 ]) ] ->
        let* x_0 = string_of_json ctx x_0 in
        let* x_1 = disambiguator_of_json ctx x_1 in
        Ok (PeIdent (x_0, x_1))
    | `Assoc [ ("Impl", impl) ] ->
        let* impl = impl_elem_of_json ctx impl in
        Ok (PeImpl impl)
    | `Assoc [ ("Instantiated", instantiated) ] ->
        let* instantiated =
          box_of_json (binder_of_json generic_args_of_json) ctx instantiated
        in
        Ok (PeInstantiated instantiated)
    | _ -> Error "")

and place_of_json (ctx : of_json_ctx) (js : json) : (place, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("kind", kind); ("ty", ty) ] ->
        let* kind = place_kind_of_json ctx kind in
        let* ty = ty_of_json ctx ty in
        Ok ({ kind; ty } : place)
    | _ -> Error "")

and place_kind_of_json (ctx : of_json_ctx) (js : json) :
    (place_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Local", local) ] ->
        let* local = local_id_of_json ctx local in
        Ok (PlaceLocal local)
    | `Assoc [ ("Projection", `List [ x_0; x_1 ]) ] ->
        let* x_0 = box_of_json place_of_json ctx x_0 in
        let* x_1 = projection_elem_of_json ctx x_1 in
        Ok (PlaceProjection (x_0, x_1))
    | `Assoc [ ("Global", global) ] ->
        let* global = global_decl_ref_of_json ctx global in
        Ok (PlaceGlobal global)
    | _ -> Error "")

and preset_of_json (ctx : of_json_ctx) (js : json) : (preset, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "OldDefaults" -> Ok OldDefaults
    | `String "RawMir" -> Ok RawMir
    | `String "Aeneas" -> Ok Aeneas
    | `String "Eurydice" -> Ok Eurydice
    | `String "Soteria" -> Ok Soteria
    | `String "Tests" -> Ok Tests
    | _ -> Error "")

and projection_elem_of_json (ctx : of_json_ctx) (js : json) :
    (projection_elem, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Deref" -> Ok Deref
    | `Assoc [ ("Field", `List [ x_0; x_1 ]) ] ->
        let* x_0 = field_proj_kind_of_json ctx x_0 in
        let* x_1 = field_id_of_json ctx x_1 in
        Ok (Field (x_0, x_1))
    | `String "PtrMetadata" -> Ok PtrMetadata
    | `Assoc
        [ ("Index", `Assoc [ ("offset", offset); ("from_end", from_end) ]) ] ->
        let* offset = box_of_json operand_of_json ctx offset in
        let* from_end = bool_of_json ctx from_end in
        Ok (ProjIndex (offset, from_end))
    | `Assoc
        [
          ( "Subslice",
            `Assoc [ ("from", from); ("to", to_); ("from_end", from_end) ] );
        ] ->
        let* from = box_of_json operand_of_json ctx from in
        let* to_ = box_of_json operand_of_json ctx to_ in
        let* from_end = bool_of_json ctx from_end in
        Ok (Subslice (from, to_, from_end))
    | _ -> Error "")

and provenance_of_json (ctx : of_json_ctx) (js : json) :
    (provenance, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Global", global) ] ->
        let* global = global_decl_ref_of_json ctx global in
        Ok (ProvGlobal global)
    | `Assoc [ ("Function", function_) ] ->
        let* function_ = fun_decl_ref_of_json ctx function_ in
        Ok (ProvFunction function_)
    | `String "Unknown" -> Ok ProvUnknown
    | _ -> Error "")

and ptr_metadata_of_json (ctx : of_json_ctx) (js : json) :
    (ptr_metadata, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "None" -> Ok NoMetadata
    | `String "Length" -> Ok Length
    | `Assoc [ ("VTable", v_table) ] ->
        let* v_table = type_decl_ref_of_json ctx v_table in
        Ok (VTable v_table)
    | `Assoc [ ("InheritFrom", inherit_from) ] ->
        let* inherit_from = ty_of_json ctx inherit_from in
        Ok (InheritFrom inherit_from)
    | _ -> Error "")

and raw_attribute_of_json (ctx : of_json_ctx) (js : json) :
    (raw_attribute, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("path", path); ("args", args) ] ->
        let* path = string_of_json ctx path in
        let* args = option_of_json string_of_json ctx args in
        Ok ({ path; args } : raw_attribute)
    | _ -> Error "")

and ref_kind_of_json (ctx : of_json_ctx) (js : json) : (ref_kind, string) result
    =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Mut" -> Ok RMut
    | `String "Shared" -> Ok RShared
    | _ -> Error "")

and region_of_json (ctx : of_json_ctx) (js : json) : (region, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Var", var) ] ->
        let* var = de_bruijn_var_of_json region_id_of_json ctx var in
        Ok (RVar var)
    | `String "Static" -> Ok RStatic
    | `Assoc [ ("Body", body) ] ->
        let* body = region_id_of_json ctx body in
        Ok (RBody body)
    | `String "Erased" -> Ok RErased
    | _ -> Error "")

and region_binder_of_json :
    'a0.
    (of_json_ctx -> json -> ('a0, string) result) ->
    of_json_ctx ->
    json ->
    ('a0 region_binder, string) result =
 fun arg0_of_json ctx js ->
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("regions", regions); ("skip_binder", skip_binder) ] ->
        let* binder_regions =
          index_map_of_json region_id_of_json region_param_of_json ctx regions
        in
        let* binder_value = arg0_of_json ctx skip_binder in
        Ok ({ binder_regions; binder_value } : _ region_binder)
    | _ -> Error "")

and region_id_of_json (ctx : of_json_ctx) (js : json) :
    (region_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> RegionId.id_of_json ctx x
    | _ -> Error "")

and region_param_of_json (ctx : of_json_ctx) (js : json) :
    (region_param, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("index", index); ("name", name); ("mutability", mutability) ] ->
        let* index = region_id_of_json ctx index in
        let* name = option_of_json string_of_json ctx name in
        let* mutability = lifetime_mutability_of_json ctx mutability in
        Ok ({ index; name; mutability } : region_param)
    | _ -> Error "")

and repr_algorithm_of_json (ctx : of_json_ctx) (js : json) :
    (repr_algorithm, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Rust" -> Ok Rust
    | `String "C" -> Ok C
    | _ -> Error "")

and repr_options_of_json (ctx : of_json_ctx) (js : json) :
    (repr_options, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("repr_algo", repr_algo);
          ("align_modif", align_modif);
          ("transparent", transparent);
          ("explicit_discr_type", explicit_discr_type);
        ] ->
        let* repr_algo = repr_algorithm_of_json ctx repr_algo in
        let* align_modif =
          option_of_json alignment_modifier_of_json ctx align_modif
        in
        let* transparent = bool_of_json ctx transparent in
        let* explicit_discr_type = bool_of_json ctx explicit_discr_type in
        Ok
          ({ repr_algo; align_modif; transparent; explicit_discr_type }
            : repr_options)
    | _ -> Error "")

and rvalue_of_json (ctx : of_json_ctx) (js : json) : (rvalue, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Use", use) ] ->
        let* use = operand_of_json ctx use in
        Ok (Use use)
    | `Assoc
        [
          ( "Ref",
            `Assoc
              [
                ("place", place); ("kind", kind); ("ptr_metadata", ptr_metadata);
              ] );
        ] ->
        let* place = place_of_json ctx place in
        let* kind = borrow_kind_of_json ctx kind in
        let* ptr_metadata = operand_of_json ctx ptr_metadata in
        Ok (RvRef (place, kind, ptr_metadata))
    | `Assoc
        [
          ( "RawPtr",
            `Assoc
              [
                ("place", place); ("kind", kind); ("ptr_metadata", ptr_metadata);
              ] );
        ] ->
        let* place = place_of_json ctx place in
        let* kind = ref_kind_of_json ctx kind in
        let* ptr_metadata = operand_of_json ctx ptr_metadata in
        Ok (RawPtr (place, kind, ptr_metadata))
    | `Assoc [ ("BinaryOp", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = binop_of_json ctx x_0 in
        let* x_1 = operand_of_json ctx x_1 in
        let* x_2 = operand_of_json ctx x_2 in
        Ok (BinaryOp (x_0, x_1, x_2))
    | `Assoc [ ("UnaryOp", `List [ x_0; x_1 ]) ] ->
        let* x_0 = unop_of_json ctx x_0 in
        let* x_1 = operand_of_json ctx x_1 in
        Ok (UnaryOp (x_0, x_1))
    | `Assoc [ ("NullaryOp", `List [ x_0; x_1 ]) ] ->
        let* x_0 = nullop_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        Ok (NullaryOp (x_0, x_1))
    | `Assoc [ ("Discriminant", discriminant) ] ->
        let* discriminant = place_of_json ctx discriminant in
        Ok (Discriminant discriminant)
    | `Assoc [ ("Aggregate", `List [ x_0; x_1 ]) ] ->
        let* x_0 = aggregate_kind_of_json ctx x_0 in
        let* x_1 = list_of_json operand_of_json ctx x_1 in
        Ok (Aggregate (x_0, x_1))
    | `Assoc [ ("Len", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = place_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        let* x_2 = option_of_json (box_of_json constant_expr_of_json) ctx x_2 in
        Ok (Len (x_0, x_1, x_2))
    | `Assoc [ ("Repeat", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = operand_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        let* x_2 = box_of_json constant_expr_of_json ctx x_2 in
        Ok (Repeat (x_0, x_1, x_2))
    | `Assoc [ ("ShallowInitBox", `List [ x_0; x_1 ]) ] ->
        let* x_0 = operand_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        Ok (ShallowInitBox (x_0, x_1))
    | _ -> Error "")

and scalar_value_of_json (ctx : of_json_ctx) (js : json) :
    (scalar_value, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Unsigned", `List [ x_0; x_1 ]) ] ->
        let* x_0 = u_int_ty_of_json ctx x_0 in
        let* x_1 = big_int_of_json ctx x_1 in
        Ok (UnsignedScalar (x_0, x_1))
    | `Assoc [ ("Signed", `List [ x_0; x_1 ]) ] ->
        let* x_0 = int_ty_of_json ctx x_0 in
        let* x_1 = big_int_of_json ctx x_1 in
        Ok (SignedScalar (x_0, x_1))
    | _ -> Error "")

and span_of_json (ctx : of_json_ctx) (js : json) : (span, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("data", data); ("generated_from_span", generated_from_span) ] ->
        let* data = span_data_of_json ctx data in
        let* generated_from_span =
          option_of_json span_data_of_json ctx generated_from_span
        in
        Ok ({ data; generated_from_span } : span)
    | _ -> Error "")

and span_data_of_json (ctx : of_json_ctx) (js : json) :
    (span_data, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("file_id", file_id); ("beg", beg); ("end", end_) ] ->
        let* file = file_id_of_json ctx file_id in
        let* beg_loc = loc_of_json ctx beg in
        let* end_loc = loc_of_json ctx end_ in
        Ok ({ file; beg_loc; end_loc } : span_data)
    | _ -> Error "")

and tag_encoding_of_json (ctx : of_json_ctx) (js : json) :
    (tag_encoding, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Direct" -> Ok Direct
    | `Assoc [ ("Niche", `Assoc [ ("untagged_variant", untagged_variant) ]) ] ->
        let* untagged_variant = variant_id_of_json ctx untagged_variant in
        Ok (Niche untagged_variant)
    | _ -> Error "")

and target_info_of_json (ctx : of_json_ctx) (js : json) :
    (target_info, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("target_pointer_size", target_pointer_size);
          ("is_little_endian", is_little_endian);
        ] ->
        let* target_pointer_size = int_of_json ctx target_pointer_size in
        let* is_little_endian = bool_of_json ctx is_little_endian in
        Ok ({ target_pointer_size; is_little_endian } : target_info)
    | _ -> Error "")

and trait_assoc_const_of_json (ctx : of_json_ctx) (js : json) :
    (trait_assoc_const, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("name", name);
          ("attr_info", attr_info);
          ("ty", ty);
          ("default", default);
        ] ->
        let* name = trait_item_name_of_json ctx name in
        let* attr_info = attr_info_of_json ctx attr_info in
        let* ty = ty_of_json ctx ty in
        let* default = option_of_json global_decl_ref_of_json ctx default in
        Ok ({ name; attr_info; ty; default } : trait_assoc_const)
    | _ -> Error "")

and trait_assoc_ty_of_json (ctx : of_json_ctx) (js : json) :
    (trait_assoc_ty, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("name", name);
          ("attr_info", attr_info);
          ("default", default);
          ("implied_clauses", implied_clauses);
        ] ->
        let* name = trait_item_name_of_json ctx name in
        let* attr_info = attr_info_of_json ctx attr_info in
        let* default = option_of_json trait_assoc_ty_impl_of_json ctx default in
        let* implied_clauses =
          index_map_of_json trait_clause_id_of_json trait_param_of_json ctx
            implied_clauses
        in
        Ok ({ name; attr_info; default; implied_clauses } : trait_assoc_ty)
    | _ -> Error "")

and trait_assoc_ty_impl_of_json (ctx : of_json_ctx) (js : json) :
    (trait_assoc_ty_impl, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("value", value); ("implied_trait_refs", _) ] ->
        let* value = ty_of_json ctx value in
        Ok ({ value } : trait_assoc_ty_impl)
    | _ -> Error "")

and trait_clause_id_of_json (ctx : of_json_ctx) (js : json) :
    (trait_clause_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> TraitClauseId.id_of_json ctx x
    | _ -> Error "")

and trait_decl_of_json (ctx : of_json_ctx) (js : json) :
    (trait_decl, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("def_id", def_id);
          ("item_meta", item_meta);
          ("generics", generics);
          ("implied_clauses", implied_clauses);
          ("consts", consts);
          ("types", types);
          ("methods", methods);
          ("vtable", vtable);
        ] ->
        let* def_id = trait_decl_id_of_json ctx def_id in
        let* item_meta = item_meta_of_json ctx item_meta in
        let* generics = generic_params_of_json ctx generics in
        let* implied_clauses =
          index_map_of_json trait_clause_id_of_json trait_param_of_json ctx
            implied_clauses
        in
        let* consts = list_of_json trait_assoc_const_of_json ctx consts in
        let* types =
          list_of_json (binder_of_json trait_assoc_ty_of_json) ctx types
        in
        let* methods =
          list_of_json (binder_of_json trait_method_of_json) ctx methods
        in
        let* vtable = option_of_json type_decl_ref_of_json ctx vtable in
        Ok
          ({
             def_id;
             item_meta;
             generics;
             implied_clauses;
             consts;
             types;
             methods;
             vtable;
           }
            : trait_decl)
    | _ -> Error "")

and trait_decl_id_of_json (ctx : of_json_ctx) (js : json) :
    (trait_decl_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> TraitDeclId.id_of_json ctx x
    | _ -> Error "")

and trait_decl_ref_of_json (ctx : of_json_ctx) (js : json) :
    (trait_decl_ref, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("id", id); ("generics", generics) ] ->
        let* id = trait_decl_id_of_json ctx id in
        let* generics = box_of_json generic_args_of_json ctx generics in
        Ok ({ id; generics } : trait_decl_ref)
    | _ -> Error "")

and trait_impl_of_json (ctx : of_json_ctx) (js : json) :
    (trait_impl, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("def_id", def_id);
          ("item_meta", item_meta);
          ("impl_trait", impl_trait);
          ("generics", generics);
          ("implied_trait_refs", implied_trait_refs);
          ("consts", consts);
          ("types", types);
          ("methods", methods);
          ("vtable", vtable);
        ] ->
        let* def_id = trait_impl_id_of_json ctx def_id in
        let* item_meta = item_meta_of_json ctx item_meta in
        let* impl_trait = trait_decl_ref_of_json ctx impl_trait in
        let* generics = generic_params_of_json ctx generics in
        let* implied_trait_refs =
          index_map_of_json trait_clause_id_of_json trait_ref_of_json ctx
            implied_trait_refs
        in
        let* consts =
          list_of_json
            (pair_of_json trait_item_name_of_json global_decl_ref_of_json)
            ctx consts
        in
        let* types =
          list_of_json
            (pair_of_json trait_item_name_of_json
               (binder_of_json trait_assoc_ty_impl_of_json))
            ctx types
        in
        let* methods =
          list_of_json
            (pair_of_json trait_item_name_of_json
               (binder_of_json fun_decl_ref_of_json))
            ctx methods
        in
        let* vtable = option_of_json global_decl_ref_of_json ctx vtable in
        Ok
          ({
             def_id;
             item_meta;
             impl_trait;
             generics;
             implied_trait_refs;
             consts;
             types;
             methods;
             vtable;
           }
            : trait_impl)
    | _ -> Error "")

and trait_impl_id_of_json (ctx : of_json_ctx) (js : json) :
    (trait_impl_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> TraitImplId.id_of_json ctx x
    | _ -> Error "")

and trait_impl_ref_of_json (ctx : of_json_ctx) (js : json) :
    (trait_impl_ref, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("id", id); ("generics", generics) ] ->
        let* id = trait_impl_id_of_json ctx id in
        let* generics = box_of_json generic_args_of_json ctx generics in
        Ok ({ id; generics } : trait_impl_ref)
    | _ -> Error "")

and trait_item_name_of_json (ctx : of_json_ctx) (js : json) :
    (trait_item_name, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> string_of_json ctx x
    | _ -> Error "")

and trait_method_of_json (ctx : of_json_ctx) (js : json) :
    (trait_method, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("name", name); ("attr_info", attr_info); ("item", item) ] ->
        let* name = trait_item_name_of_json ctx name in
        let* attr_info = attr_info_of_json ctx attr_info in
        let* item = fun_decl_ref_of_json ctx item in
        Ok ({ name; attr_info; item } : trait_method)
    | _ -> Error "")

and trait_param_of_json (ctx : of_json_ctx) (js : json) :
    (trait_param, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("clause_id", clause_id);
          ("span", span);
          ("origin", _);
          ("trait_", trait);
        ] ->
        let* clause_id = trait_clause_id_of_json ctx clause_id in
        let* span = option_of_json span_of_json ctx span in
        let* trait = region_binder_of_json trait_decl_ref_of_json ctx trait in
        Ok ({ clause_id; span; trait } : trait_param)
    | _ -> Error "")

and trait_ref_of_json (ctx : of_json_ctx) (js : json) :
    (trait_ref, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | json ->
        hash_consed_val_of_json ctx.tref_hashcons_map trait_ref_contents_of_json
          ctx json
    | _ -> Error "")

and trait_ref_contents_of_json (ctx : of_json_ctx) (js : json) :
    (trait_ref_contents, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("kind", kind); ("trait_decl_ref", trait_decl_ref) ] ->
        let* kind = trait_ref_kind_of_json ctx kind in
        let* trait_decl_ref =
          region_binder_of_json trait_decl_ref_of_json ctx trait_decl_ref
        in
        Ok ({ kind; trait_decl_ref } : trait_ref_contents)
    | _ -> Error "")

and trait_ref_kind_of_json (ctx : of_json_ctx) (js : json) :
    (trait_ref_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("TraitImpl", trait_impl) ] ->
        let* trait_impl = trait_impl_ref_of_json ctx trait_impl in
        Ok (TraitImpl trait_impl)
    | `Assoc [ ("Clause", clause) ] ->
        let* clause =
          de_bruijn_var_of_json trait_clause_id_of_json ctx clause
        in
        Ok (Clause clause)
    | `Assoc [ ("ParentClause", `List [ x_0; x_1 ]) ] ->
        let* x_0 = box_of_json trait_ref_of_json ctx x_0 in
        let* x_1 = trait_clause_id_of_json ctx x_1 in
        Ok (ParentClause (x_0, x_1))
    | `Assoc [ ("ItemClause", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = box_of_json trait_ref_of_json ctx x_0 in
        let* x_1 = trait_item_name_of_json ctx x_1 in
        let* x_2 = trait_clause_id_of_json ctx x_2 in
        Ok (ItemClause (x_0, x_1, x_2))
    | `String "SelfId" -> Ok Self
    | `Assoc
        [
          ( "BuiltinOrAuto",
            `Assoc
              [
                ("builtin_data", builtin_data);
                ("parent_trait_refs", parent_trait_refs);
                ("types", types);
              ] );
        ] ->
        let* builtin_data = builtin_impl_data_of_json ctx builtin_data in
        let* parent_trait_refs =
          index_map_of_json trait_clause_id_of_json trait_ref_of_json ctx
            parent_trait_refs
        in
        let* types =
          list_of_json
            (pair_of_json trait_item_name_of_json trait_assoc_ty_impl_of_json)
            ctx types
        in
        Ok (BuiltinOrAuto (builtin_data, parent_trait_refs, types))
    | `String "Dyn" -> Ok Dyn
    | `Assoc [ ("Unknown", unknown) ] ->
        let* unknown = string_of_json ctx unknown in
        Ok (UnknownTrait unknown)
    | _ -> Error "")

and trait_type_constraint_of_json (ctx : of_json_ctx) (js : json) :
    (trait_type_constraint, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("trait_ref", trait_ref); ("type_name", type_name); ("ty", ty) ]
      ->
        let* trait_ref = trait_ref_of_json ctx trait_ref in
        let* type_name = trait_item_name_of_json ctx type_name in
        let* ty = ty_of_json ctx ty in
        Ok ({ trait_ref; type_name; ty } : trait_type_constraint)
    | _ -> Error "")

and trait_type_constraint_id_of_json (ctx : of_json_ctx) (js : json) :
    (trait_type_constraint_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> TraitTypeConstraintId.id_of_json ctx x
    | _ -> Error "")

and ty_of_json (ctx : of_json_ctx) (js : json) : (ty, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | json ->
        hash_consed_val_of_json ctx.ty_hashcons_map ty_kind_of_json ctx json
    | _ -> Error "")

and ty_kind_of_json (ctx : of_json_ctx) (js : json) : (ty_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Adt", adt) ] ->
        let* adt = type_decl_ref_of_json ctx adt in
        Ok (TAdt adt)
    | `Assoc [ ("TypeVar", type_var) ] ->
        let* type_var =
          de_bruijn_var_of_json type_var_id_of_json ctx type_var
        in
        Ok (TVar type_var)
    | `Assoc [ ("Literal", literal) ] ->
        let* literal = literal_type_of_json ctx literal in
        Ok (TLiteral literal)
    | `String "Never" -> Ok TNever
    | `Assoc [ ("Ref", `List [ x_0; x_1; x_2 ]) ] ->
        let* x_0 = region_of_json ctx x_0 in
        let* x_1 = ty_of_json ctx x_1 in
        let* x_2 = ref_kind_of_json ctx x_2 in
        Ok (TRef (x_0, x_1, x_2))
    | `Assoc [ ("RawPtr", `List [ x_0; x_1 ]) ] ->
        let* x_0 = ty_of_json ctx x_0 in
        let* x_1 = ref_kind_of_json ctx x_1 in
        Ok (TRawPtr (x_0, x_1))
    | `Assoc [ ("TraitType", `List [ x_0; x_1 ]) ] ->
        let* x_0 = trait_ref_of_json ctx x_0 in
        let* x_1 = trait_item_name_of_json ctx x_1 in
        Ok (TTraitType (x_0, x_1))
    | `Assoc [ ("DynTrait", dyn_trait) ] ->
        let* dyn_trait = dyn_predicate_of_json ctx dyn_trait in
        Ok (TDynTrait dyn_trait)
    | `Assoc [ ("FnPtr", fn_ptr) ] ->
        let* fn_ptr = region_binder_of_json fun_sig_of_json ctx fn_ptr in
        Ok (TFnPtr fn_ptr)
    | `Assoc [ ("FnDef", fn_def) ] ->
        let* fn_def = region_binder_of_json fn_ptr_of_json ctx fn_def in
        Ok (TFnDef fn_def)
    | `Assoc [ ("PtrMetadata", ptr_metadata) ] ->
        let* ptr_metadata = ty_of_json ctx ptr_metadata in
        Ok (TPtrMetadata ptr_metadata)
    | `Assoc [ ("Array", `List [ x_0; x_1 ]) ] ->
        let* x_0 = ty_of_json ctx x_0 in
        let* x_1 = box_of_json constant_expr_of_json ctx x_1 in
        Ok (TArray (x_0, x_1))
    | `Assoc [ ("Slice", slice) ] ->
        let* slice = ty_of_json ctx slice in
        Ok (TSlice slice)
    | `Assoc [ ("Error", error) ] ->
        let* error = string_of_json ctx error in
        Ok (TError error)
    | _ -> Error "")

and type_decl_of_json (ctx : of_json_ctx) (js : json) :
    (type_decl, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("def_id", def_id);
          ("item_meta", item_meta);
          ("generics", generics);
          ("src", src);
          ("kind", kind);
          ("layout", layout);
          ("ptr_metadata", ptr_metadata);
          ("repr", repr);
        ] ->
        let* def_id = type_decl_id_of_json ctx def_id in
        let* item_meta = item_meta_of_json ctx item_meta in
        let* generics = generic_params_of_json ctx generics in
        let* src = item_source_of_json ctx src in
        let* kind = type_decl_kind_of_json ctx kind in
        let* layout = option_of_json layout_of_json ctx layout in
        let* ptr_metadata = ptr_metadata_of_json ctx ptr_metadata in
        let* repr = option_of_json repr_options_of_json ctx repr in
        Ok
          ({
             def_id;
             item_meta;
             generics;
             src;
             kind;
             layout;
             ptr_metadata;
             repr;
           }
            : type_decl)
    | _ -> Error "")

and type_decl_id_of_json (ctx : of_json_ctx) (js : json) :
    (type_decl_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> TypeDeclId.id_of_json ctx x
    | _ -> Error "")

and type_decl_kind_of_json (ctx : of_json_ctx) (js : json) :
    (type_decl_kind, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Struct", struct_) ] ->
        let* struct_ =
          index_vec_of_json field_id_of_json field_of_json ctx struct_
        in
        Ok (Struct struct_)
    | `Assoc [ ("Enum", enum) ] ->
        let* enum =
          index_vec_of_json variant_id_of_json variant_of_json ctx enum
        in
        Ok (Enum enum)
    | `Assoc [ ("Union", union) ] ->
        let* union =
          index_vec_of_json field_id_of_json field_of_json ctx union
        in
        Ok (Union union)
    | `String "Opaque" -> Ok Opaque
    | `Assoc [ ("Alias", alias) ] ->
        let* alias = ty_of_json ctx alias in
        Ok (Alias alias)
    | `Assoc [ ("Error", error) ] ->
        let* error = string_of_json ctx error in
        Ok (TDeclError error)
    | _ -> Error "")

and type_decl_ref_of_json (ctx : of_json_ctx) (js : json) :
    (type_decl_ref, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("id", id); ("generics", generics) ] ->
        let* id = type_id_of_json ctx id in
        let* generics = box_of_json generic_args_of_json ctx generics in
        Ok ({ id; generics } : type_decl_ref)
    | _ -> Error "")

and type_id_of_json (ctx : of_json_ctx) (js : json) : (type_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Adt", adt) ] ->
        let* adt = type_decl_id_of_json ctx adt in
        Ok (TAdtId adt)
    | `String "Tuple" -> Ok TTuple
    | `Assoc [ ("Builtin", builtin) ] ->
        let* builtin = builtin_ty_of_json ctx builtin in
        Ok (TBuiltin builtin)
    | _ -> Error "")

and type_param_of_json (ctx : of_json_ctx) (js : json) :
    (type_param, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("index", index); ("name", name) ] ->
        let* index = type_var_id_of_json ctx index in
        let* name = string_of_json ctx name in
        Ok ({ index; name } : type_param)
    | _ -> Error "")

and type_var_id_of_json (ctx : of_json_ctx) (js : json) :
    (type_var_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> TypeVarId.id_of_json ctx x
    | _ -> Error "")

and u_int_ty_of_json (ctx : of_json_ctx) (js : json) : (u_int_ty, string) result
    =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Usize" -> Ok Usize
    | `String "U8" -> Ok U8
    | `String "U16" -> Ok U16
    | `String "U32" -> Ok U32
    | `String "U64" -> Ok U64
    | `String "U128" -> Ok U128
    | _ -> Error "")

and unop_of_json (ctx : of_json_ctx) (js : json) : (unop, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Not" -> Ok Not
    | `Assoc [ ("Neg", neg) ] ->
        let* neg = overflow_mode_of_json ctx neg in
        Ok (Neg neg)
    | `Assoc [ ("Cast", cast) ] ->
        let* cast = cast_kind_of_json ctx cast in
        Ok (Cast cast)
    | _ -> Error "")

and unsizing_metadata_of_json (ctx : of_json_ctx) (js : json) :
    (unsizing_metadata, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc [ ("Length", length) ] ->
        let* length = box_of_json constant_expr_of_json ctx length in
        Ok (MetaLength length)
    | `Assoc [ ("VTable", `List [ x_0; x_1 ]) ] ->
        let* x_0 = trait_ref_of_json ctx x_0 in
        let* x_1 = box_of_json constant_expr_of_json ctx x_1 in
        Ok (MetaVTable (x_0, x_1))
    | `Assoc [ ("VTableUpcast", v_table_upcast) ] ->
        let* v_table_upcast =
          list_of_json field_id_of_json ctx v_table_upcast
        in
        Ok (MetaVTableUpcast v_table_upcast)
    | `String "Unknown" -> Ok MetaUnknown
    | _ -> Error "")

and v_table_field_of_json (ctx : of_json_ctx) (js : json) :
    (v_table_field, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `String "Size" -> Ok VTableSize
    | `String "Align" -> Ok VTableAlign
    | `String "Drop" -> Ok VTableDrop
    | `Assoc [ ("Method", method_) ] ->
        let* method_ = trait_item_name_of_json ctx method_ in
        Ok (VTableMethod method_)
    | `Assoc [ ("SuperTrait", super_trait) ] ->
        let* super_trait = trait_clause_id_of_json ctx super_trait in
        Ok (VTableSuperTrait super_trait)
    | _ -> Error "")

and variant_of_json (ctx : of_json_ctx) (js : json) : (variant, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("span", span);
          ("attr_info", attr_info);
          ("name", name);
          ("fields", fields);
          ("discriminant", discriminant);
        ] ->
        let* span = span_of_json ctx span in
        let* attr_info = attr_info_of_json ctx attr_info in
        let* variant_name = string_of_json ctx name in
        let* fields =
          index_vec_of_json field_id_of_json field_of_json ctx fields
        in
        let* discriminant = literal_of_json ctx discriminant in
        Ok ({ span; attr_info; variant_name; fields; discriminant } : variant)
    | _ -> Error "")

and variant_id_of_json (ctx : of_json_ctx) (js : json) :
    (variant_id, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | x -> VariantId.id_of_json ctx x
    | _ -> Error "")

and variant_layout_of_json (ctx : of_json_ctx) (js : json) :
    (variant_layout, string) result =
  combine_error_msgs js __FUNCTION__
    (match js with
    | `Assoc
        [
          ("field_offsets", field_offsets);
          ("uninhabited", uninhabited);
          ("tag", tag);
        ] ->
        let* field_offsets =
          index_vec_of_json field_id_of_json int_of_json ctx field_offsets
        in
        let* uninhabited = bool_of_json ctx uninhabited in
        let* tag = option_of_json scalar_value_of_json ctx tag in
        Ok ({ field_offsets; uninhabited; tag } : variant_layout)
    | _ -> Error "")
