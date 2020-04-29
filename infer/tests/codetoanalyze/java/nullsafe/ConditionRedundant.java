/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.nullsafe_default;

import com.facebook.infer.annotation.Assertions;
import com.google.common.base.Preconditions;
import javax.annotation.Nullable;

public class ConditionRedundant {

  String fieldNonnull = "";
  @Nullable String fieldNullable = "";

  @Nullable
  String getNullable() {
    return null;
  }

  String getNonnull() {
    return "";
  }

  void compareNEQ_NonnullIsBAD(String s) {
    if (s != null) { // BAD: condition redundant
      // Do something
    }
  }

  void compareNEQ_NullableIsOK(@Nullable String s) {
    if (s != null) { // OK: comparing with nullable
      // Do something
    }
  }

  void compareEQ_NonnullIsBAD(String s) {
    if (s == null) { // BAD: condition redundant
      // Do something
    }
  }

  void compareEQ_NullableIsOK(@Nullable String s) {
    if (s == null) { // OK: comparing with nullable
      // Do something
    }
  }

  // `if` is not essential, we test all comparisons expressions

  void outsideOfIfCompareNonnullIsBAD(String s) {
    boolean b = s != null; // BAD: condition redundant
  }

  void outsideOfIfCompareNullableIsOK(@Nullable String s) {
    boolean b = s != null; // OK: comparing with nullable
  }

  // comparing with nonnull is redundant even if it is a part of expression

  void conjunctionBothNonnullIsBAD(String s1, String s2) {
    if (s1 != null && s2 != null) { // BAD: both clauses are redudant
      // Do something
    }
  }

  void conjunctionOneNonnullIsBAD(@Nullable String s1, String s2) {
    if (s1 != null && s2 != null) { // BAD: one clause is redundant
      // Do something
    }
  }

  void conjunctionBothNullableIsOK(@Nullable String s1, @Nullable String s2) {
    if (s1 != null && s2 != null) { // OK: both clauses make sense
      // Do something
    }
  }

  void disjunctionBothNonnullIsBAD(String s1, String s2) {
    if (s1 != null || s2 != null) { // BAD: both clauses are redudant
      // Do something
    }
  }

  void disjunctionOneNonnullIsBAD(@Nullable String s1, String s2) {
    if (s1 != null || s2 != null) { // BAD: one clause is redundant
      // Do something
    }
  }

  void disjunctionBothNullableIsOK(@Nullable String s1, @Nullable String s2) {
    if (s1 != null || s2 != null) { // OK: both clauses make sense
      // Do something
    }
  }

  // Adding some irrelevant conditions does not make the issue go away

  void irrelevantConditionWithNonnullIsBAD(String s1, @Nullable String s2, int someInt) {
    if (someInt == 1 || s1 == null || s2 != null) { // BAD: check for s1 is redundant
      // Do something
    }
  }

  void irrelevantConditionAllNullablesIsOK(@Nullable String s1, @Nullable String s2, int someInt) {
    if (someInt == 1 || s1 == null || s2 != null) { // OK: all clauses maeke sense
      // Do something
    }
  }

  // Comparing expressions (not local variables) with null

  void ternary_NonnullInBothBranchesIsBAD(String s1, String s2, int someInt) {
    // BAD: comparing nonnull with null is redundant
    if ((someInt == 1 ? s1 : s2) == null) {
      // Do something
    }
  }

  void ternary_NullableInBothBranchesIsOK(@Nullable String s1, @Nullable String s2, int someInt) {
    // OK: the result is nullable
    if ((someInt == 1 ? s1 : s2) == null) {
      // Do something
    }
  }

  void ternary_NonnullInOneBranch_FirstBranch_IsOK(String s1, @Nullable String s2, int someInt) {
    // OK: the result is nullable
    if ((someInt == 1 ? s1 : s2) == null) {
      // Do something
    }
  }

  // But if we just swap the order, we have a FP.
  // (CFG extracts this expression to a form when one of flows contain only nonnull, hence the
  // report).
  // TODO(T54065455) Don't report in this case
  void FP_ternary_NonnullInOneBranch_SecondBranch_ShouldBeOK(
      @Nullable String s1, String s2, int someInt) {
    if ((someInt == 1 ? s1 : s2) == null) { // FP: expression can be null
      // Do something
    }
  }

  void testFlowSensitivity(@Nullable String nullable1, @Nullable String nullable2) {
    if (nullable1 != null) { // OK: comparing nullable with null
      if (nullable1 != null) { // BAD: now nullable1 is nonnull
        if (nullable2 != null) { // OK: nullable2 can still be null
          if (nullable1 != null) { // BAD: nullable1 is still nonnull
            // Do something
          }
        }
      }
    }
  }

  // Test comparison with functions

  void comparingNonnullFunctionIsBAD() {
    if (getNonnull() != null) { // BAD: comparing with nonnull
      // do something
    }
  }

  void comparingNullableFunctionIsOK() {
    if (getNullable() != null) { // OK: comparing with nullable
      // do something
    }
  }

  // Test comparison with fields

  void comparingNonnullFieldIsBAD() {
    if (fieldNonnull != null) { // BAD: condition redundant
      // Do something
    }
  }

  void comparingNullableFieldIsOK() {
    if (fieldNullable != null) { // OK: comparing with nullable
      // Do something
    }
  }

  void comparingNullableFieldThatIsAlreadyCheckedIsBAD() {
    if (fieldNullable != null) { // OK: comparing with nullable
      if (fieldNullable != null) {
        // BAD: at this point we already know field is not nullable
      }
    }
  }

  // Test assertions that are modelled in Nullsafe

  void checkNotNull_NonnullIsBAD(String s) {
    Preconditions.checkNotNull(s, "BAD: we already know it is not nullable");
  }

  void checkNotNull_NullableIsOK(@Nullable String s) {
    Preconditions.checkNotNull(s, "totally legit check");
  }

  void checkArgument_NonnullIsBAd(String s) {
    Preconditions.checkArgument(s != null, "BAD: we know it is not null");
  }

  void checkArgument_NullableIsOK(@Nullable String s) {
    Preconditions.checkArgument(s != null, "totally legit check");
  }

  void assertNotNull_NonnullIsBAD(String s) {
    Assertions.assertNotNull(s, "BAD: we know it is not null");
  }

  void assertNotNull_NullableIsOK(@Nullable String s) {
    Assertions.assertNotNull(s, "totally legit check");
  }

  // Test nullability inference in try-catch

  static void maythrow() throws java.io.IOException {}

  void comparingWithNullIfAssignedBeforeThrowableIsBAD() throws java.io.IOException {
    String s = null;
    try {
      s = "123";
      maythrow();
    } finally {
      if (s != null) { // BAD: this is redundant
        // Do something
      }
    }
  }

  void comparingWithNullIfAssignedAfterThrowableIsOK() throws java.io.IOException {
    String s = null;
    try {
      maythrow();
      s = "123";
    } finally {
      if (s != null) { // OK: if `maythrow` throws, it will indeed be null
        // Do something
      }
    }
  }
}
