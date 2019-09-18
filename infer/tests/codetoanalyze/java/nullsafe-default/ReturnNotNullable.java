/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import com.google.common.base.Optional;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public class ReturnNotNullable {

  void returnvoid() {
    // No warning here.
  }

  Void returnVoid() {
    // This is OK too.
    return null;
  }

  // ------------------------------------------------------------
  // Converting different things to not annotated types.
  // By default, (not annotated type is treated as non nullable).

  String nullToNotAnnotatedIsBad() {
    return null;
  }

  String nullableToNotAnnotatedIsBad(@Nullable String s) {
    return s;
  }

  String notAnnotatedToNotAnnotatedIsOK(String s) {
    return s;
  }

  String nonNullToNotAnnotatedIsOK(@Nonnull String s) {
    return s;
  }

  String constantToNotAnnotatedIsOK() {
    return "abc";
  }

  // ------------------------------------------------------------
  // Converting different things to @Nonnull.
  // (Should be the same as converting to not annotated).

  @Nonnull
  String nullToNonnullIsBad() {
    return null;
  }

  @Nonnull
  String nullableToNonnullIsBad(@Nullable String s) {
    return s;
  }

  @Nonnull
  String notAnnotatedToNonnullIsOK(String s) {
    return s;
  }

  @Nonnull
  String nonNullToNonnullIsOK(@Nonnull String s) {
    return s;
  }

  @Nonnull
  String constantToNonNullIsOK() {
    return "abc";
  }

  // ------------------------------------------------------------
  // Converting different things to @Nullable.
  // This is either
  // 1. OK when inferred and annotated return types are both nullable, or
  // 2. Leads to ERADICATE_RETURN_OVER_ANNOTATED when inferred return type
  //    is not nullable, but function is still annotated with @Nullable.
  //    This often happens when the API author decides to return @Nullable
  //    (anticipating future change) even though the current implementation returns non-null.
  //    Because of this the warning is currently turned off by default and is recommended
  //    to use only in specific scenarious, like code migrations.

  @Nullable
  String nullToNullableIsOK() {
    return null;
  }

  @Nullable
  String nullableToNullableIsOK(@Nullable String s) {
    return s;
  }

  @Nullable
  String notAnnotatedNullableIsOverannotated(String s) {
    return s;
  }

  @Nullable
  String nonNullToNullableIsOverannotated(@Nonnull String s) {
    return s;
  }

  @Nullable
  String constantToNullableIsOverannotated() {
    return "abc";
  }

  // -------------------------------------------------------

  String throwException(@Nullable Exception e, boolean bad) throws Exception {
    if (bad) {
      throw (e); // no ERADICATE_RETURN_NOT_NULLABLE should be reported
    }
    return "OK";
  }

  @Nonnull
  BufferedReader nn(BufferedReader br) {
    return br;
  }

  void tryWithResources(String path) {
    try (BufferedReader br = nn(new BufferedReader(new FileReader(path)))) {
    } // no condition redundant should be reported on this line
    catch (IOException e) {
    }
  }

  Object tryWithResourcesReturnNullable(String path) throws IOException {
    try (BufferedReader br = nn(new BufferedReader(new FileReader(path)))) {
      return nullToNullableIsOK();
    }
  }

  /*
  Check that orNull is modelled and RETURN_OVER_ANNOTATED is not returned.
   */
  @Nullable
  String testOptional(Optional<String> os) {
    return os.orNull();
  }

  class E extends Exception {}

  String return_null_in_catch() {
    try {
      throw new E();
    } catch (E e) {
      return null;
    }
  }

  String return_null_in_catch_after_throw() {
    try {
      try {
        throw new E();
      } catch (E e) {
        throw e;
      }
    } catch (E e) {
      return null;
    }
  }

  URL getResourceNullable(Class cls, String name) {
    return cls.getResource(name);
  }

  @SomeAnnotationEndingWithNullable
  Object ensureWeDontConfuseSuchAnnotationsWithNullable() {
    // No warnings expected
    return new Object();
  }

  void testSomeAnnotationEndingWithNullable() {
    // No warnings expected
    ensureWeDontConfuseSuchAnnotationsWithNullable().toString();
  }

  static class ConditionalAssignment {
    @Nullable Object f1;

    static Object test(boolean b) {
      ConditionalAssignment x = new ConditionalAssignment();
      if (b) {
        x.f1 = new Object();
      }
      return x.f1; // can be null
    }
  }

  Stream<Object> methodUsesLambda(Stream<Object> stream) {
    return stream.map(x -> null); // Intentionaly not reporting here
  }

  Object $generatedReturnsNullOk() {
    return null;
  }

  int field;

  int returnsZero() {
    field = 0;
    return field;
  }
}
