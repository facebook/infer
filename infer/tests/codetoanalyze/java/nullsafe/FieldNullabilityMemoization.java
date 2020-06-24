/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe;

import javax.annotation.Nullable;

/**
 * Nullsafe has a feature: field nullability is memoized within a method. In other words, nullsafe
 * assumes:
 *
 * <ol>
 *   <li>a) There will be no tricky multithreading.
 *   <li>b) If a field is set to non-null, the method won't call another method that will nullify it
 *       back.
 * </ol>
 *
 * <p>NOTE: This feature is unsound, but assumptions a) and b) mostly hold for real codebases, so
 * here nullsafe tradeoffs theoretical unsoundness for practical usability.
 *
 * <p>This class tests basic properties of this feature.
 */
public class FieldNullabilityMemoization {
  private @Nullable Object nullable;

  void dereferenceIsBAD() {
    nullable.toString();
  }

  void dereferenceViaLocalVarIsBAD() {
    Object a = nullable;
    a.toString();
  }

  void dereferenceAfterCheckIsOK() {
    if (nullable != null) {
      // Theoretically, a different thread could modify the field right here.
      // But practically, if such things can happen, we have much bigger problems than nullability.
      nullable.toString();
    }
  }

  void dereferenceAfterCheckViaLocalVarIsOK() {
    if (nullable != null) {
      // Theoretically, a different thread could modify the field right here.
      // But practically, if such things can happen, we have much bigger problems than nullability.
      Object a = nullable;
      a.toString();
    }
  }

  void dereferenceAfterNonnullAssignmentIsOK() {
    nullable = "";
    // Theoretically, a different thread could modify the field right here.
    // But practically, if such things can happen, we have much bigger problems than nullability.
    nullable.toString();
  }

  void dereferenceAfterNonnullAssignmentViaLocalVarIsOK() {
    nullable = "";

    // Theoretically, a different thread could modify the field right here.
    // But practically, if such things can happen, we have much bigger problems than nullability.

    Object a = nullable;
    a.toString();
  }

  void FN_nullabilityIsPreservedEvenOnMethodCalls() {
    nullable = "";

    // Calling methods does not invalidate nullability of fields,
    // even if they theoritically can nullify the field.
    // In practice, this happens extremely rarely, but in this synthetic example
    // this will lead to an NPE.
    nullify();

    // Uncaught NPE
    nullable.toString();
  }

  private void nullify() {
    nullable = null;
  }
}
