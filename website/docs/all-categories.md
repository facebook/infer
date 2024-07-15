---
title: List of all categories of issue types
---

Here are all the categories that issue types might belong to in Infer. Some issue types have no associated category at the moment. This usually indicates that the issue type is not yet mature enough to be used.

## Concurrency

Concurrent accesses to the same resource conflict in a way that can give incorrect results, block progress, or result in undefined behaviour.

Issue types in this category:
- [DEADLOCK](/docs/next/all-issue-types#deadlock)
- [GUARDEDBY_VIOLATION](/docs/next/all-issue-types#guardedby_violation)
- [INTERFACE_NOT_THREAD_SAFE](/docs/next/all-issue-types#interface_not_thread_safe)
- [LOCK_CONSISTENCY_VIOLATION](/docs/next/all-issue-types#lock_consistency_violation)
- [THREAD_SAFETY_VIOLATION](/docs/next/all-issue-types#thread_safety_violation)

## Logic error

Something that does not make sense and the sign of a potential programming error.

Issue types in this category:
- [DEAD_STORE](/docs/next/all-issue-types#dead_store)
- [PULSE_TRANSITIVE_ACCESS](/docs/next/all-issue-types#pulse_transitive_access)

## Memory error

Incorrect handling of pointers that isn't a null pointer dereference, but can still result in undefined behaviour and crashes.

Issue types in this category:
- [CXX_REF_CAPTURED_IN_BLOCK](/docs/next/all-issue-types#cxx_ref_captured_in_block)
- [NIL_MESSAGING_TO_NON_POD](/docs/next/all-issue-types#nil_messaging_to_non_pod)
- [NIL_MESSAGING_TO_NON_POD_LATENT](/docs/next/all-issue-types#nil_messaging_to_non_pod_latent)
- [PULSE_REFERENCE_STABILITY](/docs/next/all-issue-types#pulse_reference_stability)
- [PULSE_UNINITIALIZED_VALUE](/docs/next/all-issue-types#pulse_uninitialized_value)
- [STACK_VARIABLE_ADDRESS_ESCAPE](/docs/next/all-issue-types#stack_variable_address_escape)
- [STATIC_INITIALIZATION_ORDER_FIASCO](/docs/next/all-issue-types#static_initialization_order_fiasco)
- [STRONG_SELF_NOT_CHECKED](/docs/next/all-issue-types#strong_self_not_checked)
- [USE_AFTER_DELETE](/docs/next/all-issue-types#use_after_delete)
- [USE_AFTER_DELETE_LATENT](/docs/next/all-issue-types#use_after_delete_latent)
- [USE_AFTER_FREE](/docs/next/all-issue-types#use_after_free)
- [USE_AFTER_FREE_LATENT](/docs/next/all-issue-types#use_after_free_latent)
- [USE_AFTER_LIFETIME](/docs/next/all-issue-types#use_after_lifetime)
- [USE_AFTER_LIFETIME_LATENT](/docs/next/all-issue-types#use_after_lifetime_latent)
- [VECTOR_INVALIDATION](/docs/next/all-issue-types#vector_invalidation)
- [VECTOR_INVALIDATION_LATENT](/docs/next/all-issue-types#vector_invalidation_latent)

## Null pointer dereference

The null pointer is used where a valid pointer is required, causing a memory fault and a crash. For example, it is dereferenced.

Issue types in this category:
- [NIL_BLOCK_CALL](/docs/next/all-issue-types#nil_block_call)
- [NIL_BLOCK_CALL_LATENT](/docs/next/all-issue-types#nil_block_call_latent)
- [NULLPTR_DEREFERENCE](/docs/next/all-issue-types#nullptr_dereference)
- [NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS](/docs/next/all-issue-types#nullptr_dereference_in_nullsafe_class)
- [NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS_LATENT](/docs/next/all-issue-types#nullptr_dereference_in_nullsafe_class_latent)
- [NULLPTR_DEREFERENCE_LATENT](/docs/next/all-issue-types#nullptr_dereference_latent)
- [NULL_DEREFERENCE](/docs/next/all-issue-types#null_dereference)

## Perf regression

Unnecessary (or blocking) computation is performed, potentially causing a performance or responsiveness regression.

Issue types in this category:
- [CHECKERS_ALLOCATES_MEMORY](/docs/next/all-issue-types#checkers_allocates_memory)
- [CHECKERS_ANNOTATION_REACHABILITY_ERROR](/docs/next/all-issue-types#checkers_annotation_reachability_error)
- [CONFIG_IMPACT](/docs/next/all-issue-types#config_impact)
- [INEFFICIENT_KEYSET_ITERATOR](/docs/next/all-issue-types#inefficient_keyset_iterator)
- [IPC_ON_UI_THREAD](/docs/next/all-issue-types#ipc_on_ui_thread)
- [PULSE_CONST_REFABLE](/docs/next/all-issue-types#pulse_const_refable)
- [PULSE_READONLY_SHARED_PTR_PARAM](/docs/next/all-issue-types#pulse_readonly_shared_ptr_param)
- [PULSE_UNNECESSARY_COPY](/docs/next/all-issue-types#pulse_unnecessary_copy)
- [PULSE_UNNECESSARY_COPY_ASSIGNMENT](/docs/next/all-issue-types#pulse_unnecessary_copy_assignment)
- [PULSE_UNNECESSARY_COPY_ASSIGNMENT_CONST](/docs/next/all-issue-types#pulse_unnecessary_copy_assignment_const)
- [PULSE_UNNECESSARY_COPY_ASSIGNMENT_MOVABLE](/docs/next/all-issue-types#pulse_unnecessary_copy_assignment_movable)
- [PULSE_UNNECESSARY_COPY_INTERMEDIATE](/docs/next/all-issue-types#pulse_unnecessary_copy_intermediate)
- [PULSE_UNNECESSARY_COPY_INTERMEDIATE_CONST](/docs/next/all-issue-types#pulse_unnecessary_copy_intermediate_const)
- [PULSE_UNNECESSARY_COPY_MOVABLE](/docs/next/all-issue-types#pulse_unnecessary_copy_movable)
- [PULSE_UNNECESSARY_COPY_OPTIONAL](/docs/next/all-issue-types#pulse_unnecessary_copy_optional)
- [PULSE_UNNECESSARY_COPY_OPTIONAL_CONST](/docs/next/all-issue-types#pulse_unnecessary_copy_optional_const)
- [PULSE_UNNECESSARY_COPY_RETURN](/docs/next/all-issue-types#pulse_unnecessary_copy_return)
- [REGEX_OP_ON_UI_THREAD](/docs/next/all-issue-types#regex_op_on_ui_thread)
- [STRICT_MODE_VIOLATION](/docs/next/all-issue-types#strict_mode_violation)

## Resource leak

A resource (for example memory, or a file descriptor) has been manually allocated but not released, possibly creating memory pressure over time or even incorrect results.

Issue types in this category:
- [BIABDUCTION_MEMORY_LEAK](/docs/next/all-issue-types#biabduction_memory_leak)
- [BIABDUCTION_RETAIN_CYCLE](/docs/next/all-issue-types#biabduction_retain_cycle)
- [CAPTURED_STRONG_SELF](/docs/next/all-issue-types#captured_strong_self)
- [CHECKERS_FRAGMENT_RETAINS_VIEW](/docs/next/all-issue-types#checkers_fragment_retains_view)
- [MEMORY_LEAK_C](/docs/next/all-issue-types#memory_leak_c)
- [MEMORY_LEAK_CPP](/docs/next/all-issue-types#memory_leak_cpp)
- [MIXED_SELF_WEAKSELF](/docs/next/all-issue-types#mixed_self_weakself)
- [PULSE_RESOURCE_LEAK](/docs/next/all-issue-types#pulse_resource_leak)
- [PULSE_UNAWAITED_AWAITABLE](/docs/next/all-issue-types#pulse_unawaited_awaitable)
- [PULSE_UNFINISHED_BUILDER](/docs/next/all-issue-types#pulse_unfinished_builder)
- [RESOURCE_LEAK](/docs/next/all-issue-types#resource_leak)
- [RETAIN_CYCLE](/docs/next/all-issue-types#retain_cycle)
- [RETAIN_CYCLE_NO_WEAK_INFO](/docs/next/all-issue-types#retain_cycle_no_weak_info)
- [SELF_IN_BLOCK_PASSED_TO_INIT](/docs/next/all-issue-types#self_in_block_passed_to_init)

## Runtime exception

A runtime exception can occur and potentially crash the program.

Issue types in this category:
- [BAD_ARG](/docs/next/all-issue-types#bad_arg)
- [BAD_ARG_LATENT](/docs/next/all-issue-types#bad_arg_latent)
- [BAD_GENERATOR](/docs/next/all-issue-types#bad_generator)
- [BAD_GENERATOR_LATENT](/docs/next/all-issue-types#bad_generator_latent)
- [BAD_KEY](/docs/next/all-issue-types#bad_key)
- [BAD_KEY_LATENT](/docs/next/all-issue-types#bad_key_latent)
- [BAD_MAP](/docs/next/all-issue-types#bad_map)
- [BAD_MAP_LATENT](/docs/next/all-issue-types#bad_map_latent)
- [BAD_RECORD](/docs/next/all-issue-types#bad_record)
- [BAD_RECORD_LATENT](/docs/next/all-issue-types#bad_record_latent)
- [MISSING_REQUIRED_PROP](/docs/next/all-issue-types#missing_required_prop)
- [MUTUAL_RECURSION_CYCLE](/docs/next/all-issue-types#mutual_recursion_cycle)
- [NIL_INSERTION_INTO_COLLECTION](/docs/next/all-issue-types#nil_insertion_into_collection)
- [NIL_INSERTION_INTO_COLLECTION_LATENT](/docs/next/all-issue-types#nil_insertion_into_collection_latent)
- [NO_MATCHING_BRANCH_IN_TRY](/docs/next/all-issue-types#no_matching_branch_in_try)
- [NO_MATCHING_BRANCH_IN_TRY_LATENT](/docs/next/all-issue-types#no_matching_branch_in_try_latent)
- [NO_MATCHING_CASE_CLAUSE](/docs/next/all-issue-types#no_matching_case_clause)
- [NO_MATCHING_CASE_CLAUSE_LATENT](/docs/next/all-issue-types#no_matching_case_clause_latent)
- [NO_MATCHING_ELSE_CLAUSE](/docs/next/all-issue-types#no_matching_else_clause)
- [NO_MATCHING_ELSE_CLAUSE_LATENT](/docs/next/all-issue-types#no_matching_else_clause_latent)
- [NO_MATCHING_FUNCTION_CLAUSE](/docs/next/all-issue-types#no_matching_function_clause)
- [NO_MATCHING_FUNCTION_CLAUSE_LATENT](/docs/next/all-issue-types#no_matching_function_clause_latent)
- [NO_MATCH_OF_RHS](/docs/next/all-issue-types#no_match_of_rhs)
- [NO_MATCH_OF_RHS_LATENT](/docs/next/all-issue-types#no_match_of_rhs_latent)
- [NO_TRUE_BRANCH_IN_IF](/docs/next/all-issue-types#no_true_branch_in_if)
- [NO_TRUE_BRANCH_IN_IF_LATENT](/docs/next/all-issue-types#no_true_branch_in_if_latent)
- [NULL_ARGUMENT](/docs/next/all-issue-types#null_argument)
- [NULL_ARGUMENT_LATENT](/docs/next/all-issue-types#null_argument_latent)
- [OPTIONAL_EMPTY_ACCESS](/docs/next/all-issue-types#optional_empty_access)
- [OPTIONAL_EMPTY_ACCESS_LATENT](/docs/next/all-issue-types#optional_empty_access_latent)
- [PULSE_CANNOT_INSTANTIATE_ABSTRACT_CLASS](/docs/next/all-issue-types#pulse_cannot_instantiate_abstract_class)
- [PULSE_DICT_MISSING_KEY](/docs/next/all-issue-types#pulse_dict_missing_key)
- [PULSE_DYNAMIC_TYPE_MISMATCH](/docs/next/all-issue-types#pulse_dynamic_type_mismatch)
- [PULSE_UNINITIALIZED_CONST](/docs/next/all-issue-types#pulse_uninitialized_const)

## Sensitive data flow

Sensitive data is flowing where it shouldn't.

Issue types in this category:
- [DATA_FLOW_TO_SINK](/docs/next/all-issue-types#data_flow_to_sink)
- [SCOPE_LEAKAGE](/docs/next/all-issue-types#scope_leakage)
- [SENSITIVE_DATA_FLOW](/docs/next/all-issue-types#sensitive_data_flow)
- [TAINT_ERROR](/docs/next/all-issue-types#taint_error)
- [TOPL_ERROR](/docs/next/all-issue-types#topl_error)
- [TOPL_ERROR_LATENT](/docs/next/all-issue-types#topl_error_latent)

## Ungated code

Code must be under a gating mechanism but isn't.

Issue types in this category:
- [CONFIG_IMPACT_STRICT](/docs/next/all-issue-types#config_impact_strict)

