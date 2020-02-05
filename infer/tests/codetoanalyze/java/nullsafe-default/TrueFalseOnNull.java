/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import android.text.TextUtils;
import com.facebook.infer.annotation.FalseOnNull;
import com.facebook.infer.annotation.TrueOnNull;
import com.google.common.base.Strings;
import javax.annotation.Nullable;

/** Testing functionality related to @TrueOnNull and @FalseOnNull methods */
public class TrueFalseOnNull {

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
}
