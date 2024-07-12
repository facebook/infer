---
title: "Pulse"
description: "General-purpose memory and value analysis engine."
---

General-purpose memory and value analysis engine.

Activate with `--pulse`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: No
- Erlang: Experimental
- Hack: Yes
- Java: Yes
- Python: No

## What is Infer:Pulse?

Pulse is an interprocedural memory safety analysis. Pulse can detect, for instance, [Null dereferences](/docs/next/all-issue-types#nullptr_dereference) in Java. Errors are only reported when all conditions on the erroneous path are true regardless of input. Pulse should gradually replace the original [biabduction](/docs/next/checker-biabduction) analysis of Infer. An example of a Null dereference found by Pulse is given below.

```java
class Person {
    Person emergencyContact;
    String address;

    Person getEmergencyContact() {
        return this.emergencyContact;
    }
}

class Registry {
    void create() {
        Person p = new Person();
        Person c = p.getEmergencyContact();
        // Null dereference here
        System.out.println(c.address);
    }

    void printContact(Person p) {
        // No null dereference, as we don't know anything about `p`
        System.out.println(p.getEmergencyContact().address);
    }
}
```

How to run pulse for Java:
```bash
infer run --pulse -- javac Test.java
```

Pulse reports a Null dereference on this file on `create()`, as it tries to access the field `address` of object `c`, and `c` has value `null`. In contrast, Pulse gives no report for `printContact(Person p)`, as we cannot be sure that `p.getEmergencyContact()` will return `null`. But, thanks to the fact that the analysis is *inter-procedural*, Pulse will report a Null dereference on calls to `printContact(p)` when it detects that `p` is null.

## Latent Issues

When an error can occur only on some values of the parameters of the current function, Pulse does not report an issue. Such issues are called *latent*. But, if Pulse then sees a call site at which all the conditions for the error are satisfied then the error becomes *manifest* and is reported. This example (in C) illustrates how latent issues are created and then reported when they become manifest:

```c
// for more realism, imagine that this function does other things as well
void set_to_null_if_positive(int n, int* p) {
  if (n > 0) {
    p = NULL;
  }
}

void latent_null_dereference(int n, int* p) {
  set_to_null_if_positive(n, p);
  *p = 42; // NULL dereference! but only if n > 0 so no report yet
}

void manifest_error(int *p) {
  // no way to avoid the bug here => Pulse reports an error
  latent_null_dereference(1, p);
}
```

## Unknown Functions

In order to avoid false positives, Pulse makes optimistic assumptions about calls to unknown functions. Unknown functions (or unknown methods) are functions for which Infer didn't find any code. For example, it could be because the function belongs to a third-party library and we know only its signature, or because a function is made through a function pointer that Pulse wasn't able to resolve to a concrete function. In either case, Pulse will scramble the parts of the state reachable from the parameters of the call. In general, this helps avoid false positives but note that this may cause false negatives as well as false positives:

```c
void unknown(int* p); // third-party code that does [*p = 5]
                      // Infer doesn't have access to that code

void false_negative() {
  int* x = (int*) malloc(sizeof(int));
  if (x) {
    // unknown call to x makes Pulse forget that x was allocated, in case it frees x
    unknown(x);
  }
} // no memory leak reported: false negative!

void false_positive(int *x) {
  unknown(x); // this sets *x to 5
  if (x != 5) {
    // unreachable
    int* p = NULL;
    *p = 42; // false positive reported here
  }
}
```

You can check if a given function called any unknown functions by inspecting its Pulse summary. For example, for the code above:
```console
$ infer --pulse-only -- clang -c unknown_code.c
  No issues found
$ infer debug --procedures --procedures-filter 'false_negative' --procedures-summary
...
    skipped_calls={ unknown -> call to skipped function occurs here }
```

## Pulse x Nullsafe

Nullsafe is a type checker for `@Nullable` annotations for Java. Classes following the Nullsafe discipline are annotated with `@Nullsafe`.

Consider the classes `Person` and `Registry` from the previous example. Assuming that class `Person` is annotated with `@Nullsafe`. In this case, we also annotate `getEmergencyContact()` with `@Nullable`, to make explicit that this method can return the `null` value. There is still the risk that classes depending on `Person` have Null dereferences. In this case, Pulse would report a Null dereference on `Registry`. It could also be the case that class `Registry` is annotated with `@Nullsafe`. By default Pulse reports on `@Nullsafe` files too, see the `--pulse-nullsafe-report-npe` option (Facebook-specific: Pulse does not report on `@Nullsafe` files).

```java
@Nullsafe(Nullsafe.Mode.LOCAL)
class Person {
    Person emergencyContact;
    String address;

    @Nullable Person getEmergencyContact() {
        return this.emergencyContact;
    }
}

class Registry {
    ... // Pulse reports here
}
```


## List of Issue Types

The following issue types are reported by this checker:
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
- [BAD_RETURN](/docs/next/all-issue-types#bad_return)
- [BAD_RETURN_LATENT](/docs/next/all-issue-types#bad_return_latent)
- [CONFIG_USAGE](/docs/next/all-issue-types#config_usage)
- [CONSTANT_ADDRESS_DEREFERENCE](/docs/next/all-issue-types#constant_address_dereference)
- [CONSTANT_ADDRESS_DEREFERENCE_LATENT](/docs/next/all-issue-types#constant_address_dereference_latent)
- [DATA_FLOW_TO_SINK](/docs/next/all-issue-types#data_flow_to_sink)
- [MEMORY_LEAK_C](/docs/next/all-issue-types#memory_leak_c)
- [MEMORY_LEAK_CPP](/docs/next/all-issue-types#memory_leak_cpp)
- [MUTUAL_RECURSION_CYCLE](/docs/next/all-issue-types#mutual_recursion_cycle)
- [NIL_BLOCK_CALL](/docs/next/all-issue-types#nil_block_call)
- [NIL_BLOCK_CALL_LATENT](/docs/next/all-issue-types#nil_block_call_latent)
- [NIL_INSERTION_INTO_COLLECTION](/docs/next/all-issue-types#nil_insertion_into_collection)
- [NIL_INSERTION_INTO_COLLECTION_LATENT](/docs/next/all-issue-types#nil_insertion_into_collection_latent)
- [NIL_MESSAGING_TO_NON_POD](/docs/next/all-issue-types#nil_messaging_to_non_pod)
- [NIL_MESSAGING_TO_NON_POD_LATENT](/docs/next/all-issue-types#nil_messaging_to_non_pod_latent)
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
- [NULLPTR_DEREFERENCE](/docs/next/all-issue-types#nullptr_dereference)
- [NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS](/docs/next/all-issue-types#nullptr_dereference_in_nullsafe_class)
- [NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS_LATENT](/docs/next/all-issue-types#nullptr_dereference_in_nullsafe_class_latent)
- [NULLPTR_DEREFERENCE_LATENT](/docs/next/all-issue-types#nullptr_dereference_latent)
- [NULL_ARGUMENT](/docs/next/all-issue-types#null_argument)
- [NULL_ARGUMENT_LATENT](/docs/next/all-issue-types#null_argument_latent)
- [OPTIONAL_EMPTY_ACCESS](/docs/next/all-issue-types#optional_empty_access)
- [OPTIONAL_EMPTY_ACCESS_LATENT](/docs/next/all-issue-types#optional_empty_access_latent)
- [PULSE_CANNOT_INSTANTIATE_ABSTRACT_CLASS](/docs/next/all-issue-types#pulse_cannot_instantiate_abstract_class)
- [PULSE_CONST_REFABLE](/docs/next/all-issue-types#pulse_const_refable)
- [PULSE_DICT_MISSING_KEY](/docs/next/all-issue-types#pulse_dict_missing_key)
- [PULSE_DYNAMIC_TYPE_MISMATCH](/docs/next/all-issue-types#pulse_dynamic_type_mismatch)
- [PULSE_READONLY_SHARED_PTR_PARAM](/docs/next/all-issue-types#pulse_readonly_shared_ptr_param)
- [PULSE_REFERENCE_STABILITY](/docs/next/all-issue-types#pulse_reference_stability)
- [PULSE_RESOURCE_LEAK](/docs/next/all-issue-types#pulse_resource_leak)
- [PULSE_TRANSITIVE_ACCESS](/docs/next/all-issue-types#pulse_transitive_access)
- [PULSE_UNAWAITED_AWAITABLE](/docs/next/all-issue-types#pulse_unawaited_awaitable)
- [PULSE_UNFINISHED_BUILDER](/docs/next/all-issue-types#pulse_unfinished_builder)
- [PULSE_UNINITIALIZED_CONST](/docs/next/all-issue-types#pulse_uninitialized_const)
- [PULSE_UNINITIALIZED_VALUE](/docs/next/all-issue-types#pulse_uninitialized_value)
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
- [RETAIN_CYCLE](/docs/next/all-issue-types#retain_cycle)
- [RETAIN_CYCLE_NO_WEAK_INFO](/docs/next/all-issue-types#retain_cycle_no_weak_info)
- [SENSITIVE_DATA_FLOW](/docs/next/all-issue-types#sensitive_data_flow)
- [STACK_VARIABLE_ADDRESS_ESCAPE](/docs/next/all-issue-types#stack_variable_address_escape)
- [TAINT_ERROR](/docs/next/all-issue-types#taint_error)
- [USE_AFTER_DELETE](/docs/next/all-issue-types#use_after_delete)
- [USE_AFTER_DELETE_LATENT](/docs/next/all-issue-types#use_after_delete_latent)
- [USE_AFTER_FREE](/docs/next/all-issue-types#use_after_free)
- [USE_AFTER_FREE_LATENT](/docs/next/all-issue-types#use_after_free_latent)
- [USE_AFTER_LIFETIME](/docs/next/all-issue-types#use_after_lifetime)
- [USE_AFTER_LIFETIME_LATENT](/docs/next/all-issue-types#use_after_lifetime_latent)
- [VECTOR_INVALIDATION](/docs/next/all-issue-types#vector_invalidation)
- [VECTOR_INVALIDATION_LATENT](/docs/next/all-issue-types#vector_invalidation_latent)
