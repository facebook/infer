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
