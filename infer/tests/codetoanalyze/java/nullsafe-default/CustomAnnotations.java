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

    // one parameter: means return null iff s is null
    String m(@PropagatesNullable String s) {
      return s;
    }

    void mBad() {
      m(null).length(); // bad: m returns null
    }

    void mGood() {
      m("").length();
    }

    // limitation: we currently cannot check the body, and just trust the annotation
    String cannotCheckBody(@PropagatesNullable String s) {
      return null; // nothing is reported here
    }

    void illustrateFalseNegativeAsCannotCheckBody() {
      cannotCheckBody("").length(); // this is an NPE but is not found
    }

    // second parameter: means return null iff s2 is null
    String m2(@Nullable String s1, @PropagatesNullable String s2) {
      return s2;
    }

    void m2Bad() {
      m2(null, null).length(); // bad: m2 returns null
    }

    void m2Good() {
      m2(null, "").length();
    }

    // two parameters: means return null iff either s1 or s2 is null
    String m12(@PropagatesNullable String s1, @PropagatesNullable String s2) {
      return s1 == null ? s1 : s2;
    }

    void m12Bad1() {
      m12(null, "").length(); // bad: m12 returns null
    }

    void m12Bad2() {
      m12("", null).length(); // bad: m12 returns null
    }

    void m12Good() {
      m12("", "").length();
    }
  }

}
