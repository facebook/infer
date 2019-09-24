/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import android.text.TextUtils;
import com.facebook.infer.annotation.FalseOnNull;
import com.facebook.infer.annotation.PropagatesNullable;
import com.facebook.infer.annotation.TrueOnNull;
import com.google.common.base.Strings;
import javax.annotation.Nullable;

public class CustomAnnotations {

  // Example of API that benefits from annotating with @TrueOnNull and @FalseOnNull
  static class AnnotatedTextUtils {

    @TrueOnNull
    static boolean isEmpty(@Nullable CharSequence s) {
      return s == null || s.equals("");
    }

    @FalseOnNull
    static boolean isNotEmpty(@Nullable CharSequence s) {
      return s != null && s.length() > 0;
    }
  }

  // The same API, but not annotated
  static class NotAnnotatedTextUtils {
    static boolean isEmpty(@Nullable CharSequence s) {
      return s == null || s.equals("");
    }

    static boolean isNotEmpty(@Nullable CharSequence s) {
      return s != null && s.length() > 0;
    }
  }

  class Test {
    void testTrueOnNull(@Nullable CharSequence s) {
      // Explicitly annotated
      if (!AnnotatedTextUtils.isEmpty(s)) {
        s.toString(); // OK: if we are here, we know that s is not null
      }

      // Not annotated
      if (!NotAnnotatedTextUtils.isEmpty(s)) {
        s.toString(); // BAD: the typecker does not know s can not be null
      }

      if (AnnotatedTextUtils.isEmpty(s)) {
        s.toString(); // BAD: s can be null or an empty string
      }
    }

    void testFalseOnNull(@Nullable CharSequence s) {
      // Explicitly annotated
      if (AnnotatedTextUtils.isNotEmpty(s)) {
        s.toString(); // OK: if we are here, we know that `s` is not null
      }

      // Not annotated
      if (NotAnnotatedTextUtils.isNotEmpty(s)) {
        s.toString(); // BAD: the typecker does not know `s` can not be null
      }

      if (!AnnotatedTextUtils.isNotEmpty(s)) {
        s.toString(); // BAD: `s` can be null or an empty string
      }
    }

    void testModelledTrueOnNull(String s) {
      // TextUtils.isEmpty is modelled as TrueOnNull
      if (!TextUtils.isEmpty(s)) {
        s.toString(); // OK: if we are here, we know that `s` is not null
      }

      // Strings.isNullOrEmpty is modelled as TrueOnNull
      if (!Strings.isNullOrEmpty(s)) {
        s.toString(); // OK: if we are here, we know that `s` is not null
      }

      if (!NotAnnotatedTextUtils.isEmpty(s)) {
        s.toString(); // BAD: the typechecker can not figure this out for not modelled class
      }
    }

    void testEarlyReturn(@Nullable CharSequence s1, @Nullable CharSequence s2) {
      if (AnnotatedTextUtils.isEmpty(s1) || NotAnnotatedTextUtils.isEmpty(s2)) {
        return;
      }

      s1.toString(); // OK: if `s1` was null, we would no rech this point
      s2.toString(); // BAD: typechecker can not figure this for not annotated class
    }
  }

  // Tests for the annotation @PropagatesNullable
  class TestPropagatesNullable {

    class TestOneParameter {

      // means return null iff s is null
      String propagatesNullable(@PropagatesNullable String s) {
        return s;
      }

      @Nullable
      String nullable(@Nullable String s) {
        return s;
      }

      void test(@Nullable String sNullable, String sNonnull) {
        // null constant
        propagatesNullable(null).length(); // BAD: will be NPE
        nullable(null).length(); // BAD: result might be null

        // nullable
        propagatesNullable(sNullable).length(); // BAD: result can be null
        nullable(sNullable).length(); // BAD: result can be null

        // string literal
        propagatesNullable("").length(); // OK: result can not be null
        nullable("").length(); // BAD: typechecker can not figure out that this is safe

        // nonnull
        propagatesNullable(sNonnull).length(); // OK: result can not be null
        nullable(sNonnull).length(); // BAD: typechecker can not figure out that this is safe

        // flow sensitive nonnull FALSE POSITIVE
        if (sNullable != null) {
          // here we know that sNullable is not null, so it should be safe, but it is not the case
          // TODO(T53770056) fix it.
          propagatesNullable(sNullable).length();
          nullable(sNullable).length();
        }
      }

      // limitation: we currently cannot check the body, and just trust the annotation
      String cannotCheckBody(@PropagatesNullable String s) {
        return null; // nothing is reported here
      }

      void illustrateFalseNegativeAsCannotCheckBody() {
        cannotCheckBody("").length(); // this is an NPE but is not found
      }
    }

    class TestSecondParameter {
      // means return null iff s2 is null
      String propagatesNullable(@Nullable String s1, @PropagatesNullable String s2) {
        return s2;
      }

      @Nullable
      String nullable(@Nullable String s1, @Nullable String s2) {
        return s2;
      }

      // Let's ensure that @PropagatesNullable applies only to the parameter
      // that was annotated with it, and not to other params.
      void test(@Nullable String sNullable, String sNonnull) {
        // Both nullable
        propagatesNullable(sNullable, sNullable).length(); // BAD: result can be null
        nullable(sNullable, sNullable).length(); // BAD: result can be null

        // First is nonnull, second is nullable
        propagatesNullable(sNonnull, sNullable).length(); // BAD: result can be null
        nullable(sNonnull, sNullable).length(); // BAD: result can be null

        // First is nullable, second is nonnull
        propagatesNullable(sNullable, sNonnull).length(); // OK: result can not be null
        nullable(sNullable, sNonnull).length(); // BAD: typechecker can not figure this out

        // Both nonnullable
        propagatesNullable(sNonnull, sNonnull).length(); // OK: result can not be null
        nullable(sNonnull, sNonnull).length(); // BAD: typechecker can not figure this out
      }
    }

    class TestBothParams {
      // both parameters are annotated:
      // means return null iff either s1 or s2 is null
      String propagatesNullable(@PropagatesNullable String s1, @PropagatesNullable String s2) {
        return s1 == null ? s1 : s2;
      }

      void testBothParamsShouldBeNonnull(@Nullable String sNullable, String sNonnull) {
        propagatesNullable(sNullable, sNullable).length(); // BAD: result can be null
        propagatesNullable(sNonnull, sNullable).length(); // BAD: result can be null
        propagatesNullable(sNullable, sNonnull).length(); // BAD: result can be null
        propagatesNullable(sNonnull, sNonnull).length(); // OK: result can be null
      }
    }

    // For convenience, we do not require to annotate return type with @Nullable,
    // make sure it is respected.
    class TestReturnValueAnnotationIsAutomaticallyInferred {

      // Ensure that we do not warn with "return not nullable" even if we did not annotate the
      // return with @Nullable

      String notAnnotatingReturnWhenThereIsPropagatesNullableIsOK(@PropagatesNullable String s) {
        return null; // OK: treat is as implicitly nullable
      }

      String notAnnotatingReturnWhenThereAreNoPropagatesNullableIsBAD(@Nullable String s) {
        return null; // BAD: return not nullable
      }

      // Ensure that the behavior remains the same for explicitly and implicitly annotated functions

      @Nullable
      String annotatedReturn(@PropagatesNullable String s) {
        return s;
      }

      String notAnnotatedReturn(@PropagatesNullable String s) {
        return s;
      }

      // 1. Both versions equally catch non-legit usages

      void annotated_dereferencingAfterPassingNullableIsBAD(@Nullable String s) {
        annotatedReturn(s).toString(); // BAD: nullable dereference
      }

      void notAnnotated_dereferencingAfterPassingNullableIsBAD(@Nullable String s) {
        notAnnotatedReturn(s).toString(); // BAD: nullable dereference
      }

      // 2. Both versions equally allow legit usages

      void annotated_dereferencingAfterPassingNonnullIsOK(String s) {
        annotatedReturn(s).toString(); // OK: inferred to be non-nullable
      }

      void notAnnotated_dereferencingAfterPassingNonnullIsOK(String s) {
        notAnnotatedReturn(s).toString(); // OK: inferred to be non-nullable
      }
    }
  }

}
