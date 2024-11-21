"use strict";(self.webpackChunk=self.webpackChunk||[]).push([[9383],{1483:(e,s,n)=>{n.r(s),n.d(s,{assets:()=>_,contentTitle:()=>t,default:()=>h,frontMatter:()=>r,metadata:()=>c,toc:()=>a});var l=n(4848),i=n(8453);const r={title:"List of all categories of issue types"},t=void 0,c={id:"all-categories",title:"List of all categories of issue types",description:"Here are all the categories that issue types might belong to in Infer. Some issue types have no associated category at the moment. This usually indicates that the issue type is not yet mature enough to be used.",source:"@site/docs/all-categories.md",sourceDirName:".",slug:"/all-categories",permalink:"/docs/next/all-categories",draft:!1,unlisted:!1,tags:[],version:"current",frontMatter:{title:"List of all categories of issue types"},sidebar:"docs",previous:{title:"List of all checkers",permalink:"/docs/next/all-checkers"},next:{title:"List of all issue types",permalink:"/docs/next/all-issue-types"}},_={},a=[{value:"Concurrency",id:"concurrency",level:2},{value:"Logic error",id:"logic-error",level:2},{value:"Memory error",id:"memory-error",level:2},{value:"Null pointer dereference",id:"null-pointer-dereference",level:2},{value:"Perf regression",id:"perf-regression",level:2},{value:"Resource leak",id:"resource-leak",level:2},{value:"Runtime exception",id:"runtime-exception",level:2},{value:"Sensitive data flow",id:"sensitive-data-flow",level:2},{value:"Ungated code",id:"ungated-code",level:2}];function d(e){const s={a:"a",h2:"h2",li:"li",p:"p",ul:"ul",...(0,i.R)(),...e.components};return(0,l.jsxs)(l.Fragment,{children:[(0,l.jsx)(s.p,{children:"Here are all the categories that issue types might belong to in Infer. Some issue types have no associated category at the moment. This usually indicates that the issue type is not yet mature enough to be used."}),"\n",(0,l.jsx)(s.h2,{id:"concurrency",children:"Concurrency"}),"\n",(0,l.jsx)(s.p,{children:"Concurrent accesses to the same resource conflict in a way that can give incorrect results, block progress, or result in undefined behaviour."}),"\n",(0,l.jsx)(s.p,{children:"Issue types in this category:"}),"\n",(0,l.jsxs)(s.ul,{children:["\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#deadlock",children:"DEADLOCK"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#dispatch_once_in_static_init",children:"DISPATCH_ONCE_IN_STATIC_INIT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#guardedby_violation",children:"GUARDEDBY_VIOLATION"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#interface_not_thread_safe",children:"INTERFACE_NOT_THREAD_SAFE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#lock_consistency_violation",children:"LOCK_CONSISTENCY_VIOLATION"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#thread_safety_violation",children:"THREAD_SAFETY_VIOLATION"})}),"\n"]}),"\n",(0,l.jsx)(s.h2,{id:"logic-error",children:"Logic error"}),"\n",(0,l.jsx)(s.p,{children:"Something that does not make sense and the sign of a potential programming error."}),"\n",(0,l.jsx)(s.p,{children:"Issue types in this category:"}),"\n",(0,l.jsxs)(s.ul,{children:["\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#dead_store",children:"DEAD_STORE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_transitive_access",children:"PULSE_TRANSITIVE_ACCESS"})}),"\n"]}),"\n",(0,l.jsx)(s.h2,{id:"memory-error",children:"Memory error"}),"\n",(0,l.jsx)(s.p,{children:"Incorrect handling of pointers that isn't a null pointer dereference, but can still result in undefined behaviour and crashes."}),"\n",(0,l.jsx)(s.p,{children:"Issue types in this category:"}),"\n",(0,l.jsxs)(s.ul,{children:["\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#cxx_ref_captured_in_block",children:"CXX_REF_CAPTURED_IN_BLOCK"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#cxx_string_captured_in_block",children:"CXX_STRING_CAPTURED_IN_BLOCK"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nil_messaging_to_non_pod",children:"NIL_MESSAGING_TO_NON_POD"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nil_messaging_to_non_pod_latent",children:"NIL_MESSAGING_TO_NON_POD_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nsstring_internal_ptr_captured_in_block",children:"NSSTRING_INTERNAL_PTR_CAPTURED_IN_BLOCK"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_reference_stability",children:"PULSE_REFERENCE_STABILITY"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_uninitialized_value",children:"PULSE_UNINITIALIZED_VALUE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#stack_variable_address_escape",children:"STACK_VARIABLE_ADDRESS_ESCAPE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#static_initialization_order_fiasco",children:"STATIC_INITIALIZATION_ORDER_FIASCO"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#strong_self_not_checked",children:"STRONG_SELF_NOT_CHECKED"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#use_after_delete",children:"USE_AFTER_DELETE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#use_after_delete_latent",children:"USE_AFTER_DELETE_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#use_after_free",children:"USE_AFTER_FREE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#use_after_free_latent",children:"USE_AFTER_FREE_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#use_after_lifetime",children:"USE_AFTER_LIFETIME"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#use_after_lifetime_latent",children:"USE_AFTER_LIFETIME_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#vector_invalidation",children:"VECTOR_INVALIDATION"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#vector_invalidation_latent",children:"VECTOR_INVALIDATION_LATENT"})}),"\n"]}),"\n",(0,l.jsx)(s.h2,{id:"null-pointer-dereference",children:"Null pointer dereference"}),"\n",(0,l.jsx)(s.p,{children:"The null pointer is used where a valid pointer is required, causing a memory fault and a crash. For example, it is dereferenced."}),"\n",(0,l.jsx)(s.p,{children:"Issue types in this category:"}),"\n",(0,l.jsxs)(s.ul,{children:["\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#block_parameter_not_null_checked",children:"BLOCK_PARAMETER_NOT_NULL_CHECKED"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#compared_to_null_and_dereferenced",children:"COMPARED_TO_NULL_AND_DEREFERENCED"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nil_block_call",children:"NIL_BLOCK_CALL"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nil_block_call_latent",children:"NIL_BLOCK_CALL_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nullptr_dereference",children:"NULLPTR_DEREFERENCE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nullptr_dereference_in_nullsafe_class",children:"NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nullptr_dereference_in_nullsafe_class_latent",children:"NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nullptr_dereference_latent",children:"NULLPTR_DEREFERENCE_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#null_dereference",children:"NULL_DEREFERENCE"})}),"\n"]}),"\n",(0,l.jsx)(s.h2,{id:"perf-regression",children:"Perf regression"}),"\n",(0,l.jsx)(s.p,{children:"Unnecessary (or blocking) computation is performed, potentially causing a performance or responsiveness regression."}),"\n",(0,l.jsx)(s.p,{children:"Issue types in this category:"}),"\n",(0,l.jsxs)(s.ul,{children:["\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#checkers_allocates_memory",children:"CHECKERS_ALLOCATES_MEMORY"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#checkers_annotation_reachability_error",children:"CHECKERS_ANNOTATION_REACHABILITY_ERROR"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#config_impact",children:"CONFIG_IMPACT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#inefficient_keyset_iterator",children:"INEFFICIENT_KEYSET_ITERATOR"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#ipc_on_ui_thread",children:"IPC_ON_UI_THREAD"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_const_refable",children:"PULSE_CONST_REFABLE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_readonly_shared_ptr_param",children:"PULSE_READONLY_SHARED_PTR_PARAM"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy",children:"PULSE_UNNECESSARY_COPY"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_assignment",children:"PULSE_UNNECESSARY_COPY_ASSIGNMENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_assignment_const",children:"PULSE_UNNECESSARY_COPY_ASSIGNMENT_CONST"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_assignment_movable",children:"PULSE_UNNECESSARY_COPY_ASSIGNMENT_MOVABLE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_intermediate",children:"PULSE_UNNECESSARY_COPY_INTERMEDIATE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_intermediate_const",children:"PULSE_UNNECESSARY_COPY_INTERMEDIATE_CONST"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_movable",children:"PULSE_UNNECESSARY_COPY_MOVABLE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_optional",children:"PULSE_UNNECESSARY_COPY_OPTIONAL"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_optional_const",children:"PULSE_UNNECESSARY_COPY_OPTIONAL_CONST"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_return",children:"PULSE_UNNECESSARY_COPY_RETURN"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unnecessary_copy_thrift_assignment",children:"PULSE_UNNECESSARY_COPY_THRIFT_ASSIGNMENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#regex_op_on_ui_thread",children:"REGEX_OP_ON_UI_THREAD"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#strict_mode_violation",children:"STRICT_MODE_VIOLATION"})}),"\n"]}),"\n",(0,l.jsx)(s.h2,{id:"resource-leak",children:"Resource leak"}),"\n",(0,l.jsx)(s.p,{children:"A resource (for example memory, or a file descriptor) has been manually allocated but not released, possibly creating memory pressure over time or even incorrect results."}),"\n",(0,l.jsx)(s.p,{children:"Issue types in this category:"}),"\n",(0,l.jsxs)(s.ul,{children:["\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#biabduction_memory_leak",children:"BIABDUCTION_MEMORY_LEAK"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#biabduction_retain_cycle",children:"BIABDUCTION_RETAIN_CYCLE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#captured_strong_self",children:"CAPTURED_STRONG_SELF"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#checkers_fragment_retains_view",children:"CHECKERS_FRAGMENT_RETAINS_VIEW"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#memory_leak_c",children:"MEMORY_LEAK_C"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#memory_leak_cpp",children:"MEMORY_LEAK_CPP"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#mixed_self_weakself",children:"MIXED_SELF_WEAKSELF"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_resource_leak",children:"PULSE_RESOURCE_LEAK"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unawaited_awaitable",children:"PULSE_UNAWAITED_AWAITABLE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_unfinished_builder",children:"PULSE_UNFINISHED_BUILDER"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#resource_leak",children:"RESOURCE_LEAK"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#retain_cycle",children:"RETAIN_CYCLE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#retain_cycle_no_weak_info",children:"RETAIN_CYCLE_NO_WEAK_INFO"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#self_in_block_passed_to_init",children:"SELF_IN_BLOCK_PASSED_TO_INIT"})}),"\n"]}),"\n",(0,l.jsx)(s.h2,{id:"runtime-exception",children:"Runtime exception"}),"\n",(0,l.jsx)(s.p,{children:"A runtime exception can occur and potentially crash the program."}),"\n",(0,l.jsx)(s.p,{children:"Issue types in this category:"}),"\n",(0,l.jsxs)(s.ul,{children:["\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_arg",children:"BAD_ARG"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_arg_latent",children:"BAD_ARG_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_generator",children:"BAD_GENERATOR"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_generator_latent",children:"BAD_GENERATOR_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_key",children:"BAD_KEY"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_key_latent",children:"BAD_KEY_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_map",children:"BAD_MAP"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_map_latent",children:"BAD_MAP_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_record",children:"BAD_RECORD"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#bad_record_latent",children:"BAD_RECORD_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#missing_required_prop",children:"MISSING_REQUIRED_PROP"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#mutual_recursion_cycle",children:"MUTUAL_RECURSION_CYCLE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nil_insertion_into_collection",children:"NIL_INSERTION_INTO_COLLECTION"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#nil_insertion_into_collection_latent",children:"NIL_INSERTION_INTO_COLLECTION_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_matching_branch_in_try",children:"NO_MATCHING_BRANCH_IN_TRY"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_matching_branch_in_try_latent",children:"NO_MATCHING_BRANCH_IN_TRY_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_matching_case_clause",children:"NO_MATCHING_CASE_CLAUSE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_matching_case_clause_latent",children:"NO_MATCHING_CASE_CLAUSE_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_matching_else_clause",children:"NO_MATCHING_ELSE_CLAUSE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_matching_else_clause_latent",children:"NO_MATCHING_ELSE_CLAUSE_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_matching_function_clause",children:"NO_MATCHING_FUNCTION_CLAUSE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_matching_function_clause_latent",children:"NO_MATCHING_FUNCTION_CLAUSE_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_match_of_rhs",children:"NO_MATCH_OF_RHS"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_match_of_rhs_latent",children:"NO_MATCH_OF_RHS_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_true_branch_in_if",children:"NO_TRUE_BRANCH_IN_IF"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#no_true_branch_in_if_latent",children:"NO_TRUE_BRANCH_IN_IF_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#null_argument",children:"NULL_ARGUMENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#null_argument_latent",children:"NULL_ARGUMENT_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#optional_empty_access",children:"OPTIONAL_EMPTY_ACCESS"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#optional_empty_access_latent",children:"OPTIONAL_EMPTY_ACCESS_LATENT"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_cannot_instantiate_abstract_class",children:"PULSE_CANNOT_INSTANTIATE_ABSTRACT_CLASS"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_dict_missing_key",children:"PULSE_DICT_MISSING_KEY"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_dynamic_type_mismatch",children:"PULSE_DYNAMIC_TYPE_MISMATCH"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_uninitialized_const",children:"PULSE_UNINITIALIZED_CONST"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#pulse_uninitialized_method",children:"PULSE_UNINITIALIZED_METHOD"})}),"\n"]}),"\n",(0,l.jsx)(s.h2,{id:"sensitive-data-flow",children:"Sensitive data flow"}),"\n",(0,l.jsx)(s.p,{children:"Sensitive data is flowing where it shouldn't."}),"\n",(0,l.jsx)(s.p,{children:"Issue types in this category:"}),"\n",(0,l.jsxs)(s.ul,{children:["\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#data_flow_to_sink",children:"DATA_FLOW_TO_SINK"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#lineage_flow",children:"LINEAGE_FLOW"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#scope_leakage",children:"SCOPE_LEAKAGE"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#sensitive_data_flow",children:"SENSITIVE_DATA_FLOW"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#taint_error",children:"TAINT_ERROR"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#topl_error",children:"TOPL_ERROR"})}),"\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#topl_error_latent",children:"TOPL_ERROR_LATENT"})}),"\n"]}),"\n",(0,l.jsx)(s.h2,{id:"ungated-code",children:"Ungated code"}),"\n",(0,l.jsx)(s.p,{children:"Code must be under a gating mechanism but isn't."}),"\n",(0,l.jsx)(s.p,{children:"Issue types in this category:"}),"\n",(0,l.jsxs)(s.ul,{children:["\n",(0,l.jsx)(s.li,{children:(0,l.jsx)(s.a,{href:"/docs/next/all-issue-types#config_impact_strict",children:"CONFIG_IMPACT_STRICT"})}),"\n"]})]})}function h(e={}){const{wrapper:s}={...(0,i.R)(),...e.components};return s?(0,l.jsx)(s,{...e,children:(0,l.jsx)(d,{...e})}):d(e)}},8453:(e,s,n)=>{n.d(s,{R:()=>t,x:()=>c});var l=n(6540);const i={},r=l.createContext(i);function t(e){const s=l.useContext(r);return l.useMemo((function(){return"function"==typeof e?e(s):{...s,...e}}),[s,e])}function c(e){let s;return s=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:t(e.components),l.createElement(r.Provider,{value:s},e.children)}}}]);