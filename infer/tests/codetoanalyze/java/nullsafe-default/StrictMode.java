/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import com.facebook.infer.annotation.NullsafeStrict;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@NullsafeStrict
class Strict {

  public @Nullable String nullable;
  public @Nonnull String nonnull = "";

  public @Nullable String getNullable() {
    return nullable;
  }

  public String getNonnull() {
    return nonnull;
  }

  public static @Nullable String staticNullable() {
    return null;
  }

  public static String staticNonnull() {
    return "";
  }

  // 1. Inside the same class, we trust annotations.

  private void sameClass_dereferenceNullableMethodIsBad() {
    getNullable().toString();
  }

  private void sameClass_dereferenceNonnullMethodIsOK() {
    getNonnull().toString();
  }

  private void sameClass_dereferenceNullableStaticMethodIsBad() {
    staticNullable().toString();
  }

  private void sameClass_dereferenceNonnullStaticMethodIsOK() {
    staticNonnull().toString();
  }

  private void sameClass_dereferenceNullableFieldIsBad() {
    nullable.toString();
  }

  private void sameClass_dereferenceNonnullFieldIsOK() {
    nonnull.toString();
  }

  private String sameClass_convertingNullableToNonnullIsBad() {
    return getNullable();
  }

  private String sameClass_convertingNonnullToNonnullIsOK() {
    return getNonnull();
  }

  // 2. We trust annotations that came from other classes annotated as strict

  private void strictClass_dereferenceNullableMethodIsBad() {
    (new OtherStrict()).getNullable().toString();
  }

  private void strictClass_dereferenceNonnullMethodIsOK() {
    (new OtherStrict()).getNonnull().toString();
  }

  private void strictClass_dereferenceNullableStaticMethodIsBad() {
    OtherStrict.staticNullable().toString();
  }

  private void strictClass_dereferenceNonnullStaticMethodIsOK() {
    OtherStrict.staticNonnull().toString();
  }

  private void strictClass_dereferenceNullableFieldIsBad() {
    (new OtherStrict()).nullable.toString();
  }

  private void strictClass_dereferenceNonnullFieldIsOK() {
    (new OtherStrict()).nonnull.toString();
  }

  private String strictClass_convertingNullableToNonnullIsBad() {
    return (new OtherStrict()).getNullable();
  }

  private String strictClass_convertingNonnullToNonnullIsOK() {
    return (new OtherStrict()).getNonnull();
  }

  // 3. We DON'T trust annotations that came from other classes NOT annotated as strict
  // when it comes to dereferencing or converting them to nullable.

  private void nonStrictClass_dereferenceNullableMethodIsBad() {
    (new NonStrict()).getNullable().toString();
  }

  private void nonStrictClass_dereferenceNonnullMethodIsBad() {
    // even that it is declared as nonnull, can not dereference it without checking before
    (new NonStrict()).getNonnull().toString();
  }

  private void nonStrictClass_dereferenceNullableStaticMethodIsBad() {
    NonStrict.staticNullable().toString();
  }

  private void nonStrictClass_dereferenceNonnullStaticMethodIsBad() {
    // even that it is declared as nonnull, can not dereference it without checking before
    NonStrict.staticNonnull().toString();
  }

  private void nonStrictClass_dereferenceNullableFieldIsBad() {
    (new NonStrict()).nullable.toString();
  }

  private void nonStrictClass_dereferenceNonnullFieldIsBad() {
    // even that it is declared as nonnull, can not dereference it without checking before
    (new NonStrict()).nonnull.toString();
  }

  private String nonStrictClass_convertingNullableToNonnullIsBad() {
    return (new NonStrict()).getNullable();
  }

  public int usingPrimitiveTypesFromNonStrictIsOK() {
    // Of course, primitive types can not be nullable so it
    // does not matter if they are used from NonStrict
    return NonStrict.getPrimitiveTypeValue();
  }

  private String nonStrictClass_convertingNonnullToNonnullIsBad() {
    // even that it is declared as nonnull, can not convert it to nonnull it without checking before
    return (new NonStrict()).getNonnull();
  }

  // 4. We don't completely prohibit using non-strict from strict, but we do require extra checks
  // or adding defensive annotations.

  private void nonStrictClass_dereferenceNonnullFieldAfterCheckIsOK() {
    NonStrict o = new NonStrict();
    if (o.nonnull != null) {
      // This works because Nullsafe assumes that the field won't be modified between the calls
      // (e.g. in multithreading context)
      o.nonnull.toString();
    }
  }

  private void nonStrictClass_dereferenceNonnullMethodAfterCheckIsOK() {
    NonStrict o = new NonStrict();
    if (o.getNonnull() != null) {
      // This works because Nullsafe assumes that all methods are determenistic and side-effect
      // free, so the second call won't return null.
      o.getNonnull().toString();
    }
  }

  private @Nullable String nonStrictClass_convertingNonnullToNullableIsOK() {
    return (new NonStrict()).getNonnull();
  }

  // 5. Main promise of strict mode: if the function is not annotated as nullable,
  // it won't return null.
  // So, if a function is annotated as strict, no extra check on caller's side is required.
  // But strict mode DOES NOT guarantee there will be absolutely no NPE in callees:
  // a) Even if strict mode calls only strict mode, there can be assertions that will throw NPE.
  // b) We allow calling non-strict functions, and they internally can be inconsistent and throw
  // NPE.
  // c) We even allow passing values obtained from non-strict code, to other parts of non-strict
  // code (e.g. we allow glueing 2 non-strict classes together inside a strict class, which might
  // potentially lead to NPE if one of annotations is untrusted).

  private void propagatingNonnullFromNonStrictToStrictIsBad() {
    NonStrict nonStrict = new NonStrict();
    OtherStrict strict = new OtherStrict();
    nonStrict.nonnull = strict.getNonnull();
  }

  private void propagatingNonnullBetweenTwoNonStrictObjectsIsOK() {
    NonStrict nonStrict1 = new NonStrict();
    NonStrict nonStrict2 = new NonStrict();
    // Though getNonnull() is declared as non-nullable, is not strictly checked
    // so there is a possibility that it can return null, e.g. if it calls to a third-party
    // libraries. this null can leak to `nonnull` field, which might be a bad thing and lead to
    // issues.
    // This is OK for strict mode.
    nonStrict1.nonnull = nonStrict2.getNonnull();
  }
}

@NullsafeStrict
class OtherStrict {
  public @Nullable String nullable;
  public String nonnull = "";

  public @Nullable String getNullable() {
    return nullable;
  }

  public String getNonnull() {
    return nonnull;
  }

  public static @Nullable String staticNullable() {
    return null;
  }

  public static String staticNonnull() {
    return "";
  }
}

class NonStrict {
  public @Nullable String nullable;
  public String nonnull = "";

  public @Nullable String getNullable() {
    return nullable;
  }

  public String getNonnull() {
    return nonnull;
  }

  public static @Nullable String staticNullable() {
    return null;
  }

  public static String staticNonnull() {
    return "";
  }

  public static int getPrimitiveTypeValue() {
    return 0;
  }
}
