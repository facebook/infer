/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.eradicate;

import android.text.TextUtils;

import javax.annotation.Nullable;
import com.facebook.infer.annotation.Assertions;
import com.facebook.infer.annotation.FalseOnNull;
import com.facebook.infer.annotation.PropagatesNullable;
import com.facebook.infer.annotation.TrueOnNull;
import com.google.common.base.Strings;

public class CustomAnnotations {

  static class MyTextUtils {

    @TrueOnNull
    static boolean isEmpty(@Nullable java.lang.CharSequence s) {
      return s == null || s.equals("");
    }

    @FalseOnNull
    static boolean isNotEmpty(@Nullable java.lang.CharSequence s) {
      return s != null && s.length() > 0;
    }
  }

  class TestTextUtilsIsEmpty {
    void textUtilsNotIsEmpty(@Nullable CharSequence s) {
      if (!TextUtils.isEmpty(s)) {
        s.toString(); // OK
      }
    }

    void textUtilsIsEmpty(@Nullable CharSequence s) {
      if (TextUtils.isEmpty(s)) {
        s.toString(); // BAD
      }
    }

    void myTextUtilsNotIsEmpty(@Nullable CharSequence s) {
      if (!MyTextUtils.isEmpty(s)) {
        s.toString(); // OK
      }
    }

    void myTextUtilsIsEmpty(@Nullable CharSequence s) {
      if (MyTextUtils.isEmpty(s)) {
        s.toString(); // BAD
      }
    }
    void myTextUtilsIsNotEmpty(@Nullable CharSequence s) {
      if (MyTextUtils.isNotEmpty(s)) {
        s.toString(); // OK
      }
    }

    void myTextUtilsNotIsNotEmpty(@Nullable CharSequence s) {
      if (!MyTextUtils.isNotEmpty(s)) {
        s.toString(); // BAD
      }
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

  class TestModeledTrueOnNull {

    void testIsEmptyOrNullOk(@Nullable String string) {
      if (!Strings.isNullOrEmpty(string)) {
        string.contains("Infer");
      }
    }

    void testIsEmptyOrNullBad(@Nullable String string) {
      if (Strings.isNullOrEmpty(string)) {
        string.contains("Infer");
      }
    }
  }
}
