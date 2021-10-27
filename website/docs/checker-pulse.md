---
title: "Pulse"
description: "Memory and lifetime analysis."
---

Memory and lifetime analysis.

Activate with `--pulse`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: No
- Erlang: Yes
- Java: Yes

### What is Infer:Pulse?
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

Pulse reports a Null dereference on this file on `create()`, as it tries to access the field `address` of object `c`, and `c` has value `null`. In contrast, Pulse gives no report for `printContact(Person p)`, as we cannot be sure that `p.getEmergencyContact()` will return `null`. Pulse then labels this error as latent and only reports if there is a call to `printContact(Person p)` satisfying the condition for Null dereference.

### Pulse x Nullsafe

[Nullsafe](/docs/next/checker-eradicate) is a type checker for `@Nullable` annotations for Java. Classes following the Nullsafe discipline are annotated with `@Nullsafe`.

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
- [BAD_KEY](/docs/next/all-issue-types#bad_key)
- [BAD_MAP](/docs/next/all-issue-types#bad_map)
- [BAD_RECORD](/docs/next/all-issue-types#bad_record)
- [CONSTANT_ADDRESS_DEREFERENCE](/docs/next/all-issue-types#constant_address_dereference)
- [MEMORY_LEAK](/docs/next/all-issue-types#memory_leak)
- [NIL_BLOCK_CALL](/docs/next/all-issue-types#nil_block_call)
- [NIL_INSERTION_INTO_COLLECTION](/docs/next/all-issue-types#nil_insertion_into_collection)
- [NIL_MESSAGING_TO_NON_POD](/docs/next/all-issue-types#nil_messaging_to_non_pod)
- [NO_MATCHING_BRANCH_IN_TRY](/docs/next/all-issue-types#no_matching_branch_in_try)
- [NO_MATCHING_CASE_CLAUSE](/docs/next/all-issue-types#no_matching_case_clause)
- [NO_MATCHING_FUNCTION_CLAUSE](/docs/next/all-issue-types#no_matching_function_clause)
- [NO_MATCH_OF_RHS](/docs/next/all-issue-types#no_match_of_rhs)
- [NO_TRUE_BRANCH_IN_IF](/docs/next/all-issue-types#no_true_branch_in_if)
- [NULLPTR_DEREFERENCE](/docs/next/all-issue-types#nullptr_dereference)
- [OPTIONAL_EMPTY_ACCESS](/docs/next/all-issue-types#optional_empty_access)
- [PULSE_UNINITIALIZED_VALUE](/docs/next/all-issue-types#pulse_uninitialized_value)
- [STACK_VARIABLE_ADDRESS_ESCAPE](/docs/next/all-issue-types#stack_variable_address_escape)
- [USE_AFTER_DELETE](/docs/next/all-issue-types#use_after_delete)
- [USE_AFTER_FREE](/docs/next/all-issue-types#use_after_free)
- [USE_AFTER_LIFETIME](/docs/next/all-issue-types#use_after_lifetime)
- [VECTOR_INVALIDATION](/docs/next/all-issue-types#vector_invalidation)
