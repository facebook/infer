/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe;

import android.text.TextUtils;
import com.facebook.infer.annotation.FalseOnNull;
import com.facebook.infer.annotation.TrueOnNull;
import com.google.common.base.Strings;
import javax.annotation.Nullable;

/** Testing functionality related to @TrueOnNull and @FalseOnNull methods */
public class TrueFalseOnNull {

  static class StaticOneParam {
    @TrueOnNull
    static boolean trueOnNull(@Nullable Object o) {
      return o == null ? true : o.toString() == "something";
    }

    @FalseOnNull
    static boolean falseOnNull(@Nullable Object o) {
      return o == null ? false : o.toString() == "something";
    }

    static boolean notAnnotated(@Nullable Object o) {
      return o == null ? false : o.toString() == "something";
    }
  }

  static class NonStaticOneParam {
    private String compareTo = "something";

    @TrueOnNull
    boolean trueOnNull(@Nullable Object o) {
      return o == null ? true : o.toString() == compareTo;
    }

    @FalseOnNull
    boolean falseOnNull(@Nullable Object o) {
      return o == null ? false : o.toString() == compareTo;
    }

    boolean notAnnotated(@Nullable Object o) {
      return o == null ? false : o.toString() == compareTo;
    }
  }

  // @TrueOnNull and @FalseOnNull should expect true/false will be returned if ANY of input objects
  // is null.
  // In other words, they should infer that all input nullable objects are non-null in the
  // corresponding branch.
  static class NonStaticSeveralParams {
    private String compareTo = "something";

    @TrueOnNull
    boolean trueOnNull(
        @Nullable Object nullable1, int primitive, Object nonnull, @Nullable Object nullable2) {
      if (nullable1 == null || nullable2 == null) {
        return true;
      }
      return nonnull == compareTo;
    }

    @FalseOnNull
    boolean falseOnNull(
        @Nullable Object nullable1, int primitive, Object nonnull, @Nullable Object nullable2) {
      if (nullable1 == null || nullable2 == null) {
        return false;
      }
      return nonnull == compareTo;
    }

    boolean notAnnotated(
        @Nullable Object nullable1, int primitive, Object nonnull, @Nullable Object nullable2) {
      if (nullable1 == null || nullable2 == null) {
        return false;
      }
      return nonnull == compareTo;
    }
  }

  class TestStaticOneParam {

    void trueOnNullPositiveBranchIsBAD(@Nullable String s) {
      if (StaticOneParam.trueOnNull(s)) {
        s.toString();
      }
    }

    void trueOnNullNegativeBranchIsOK(@Nullable String s) {
      if (!StaticOneParam.trueOnNull(s)) {
        s.toString();
      }
    }

    void falseOnNullPositiveBranchIsOK(@Nullable String s) {
      if (StaticOneParam.falseOnNull(s)) {
        s.toString();
      }
    }

    void falseOnNullNegativeBranchIsBAD(@Nullable String s) {
      if (!StaticOneParam.falseOnNull(s)) {
        s.toString();
      }
    }

    void notAnnotatedPositiveBranchIsBAD(@Nullable String s) {
      if (StaticOneParam.notAnnotated(s)) {
        s.toString();
      }
    }

    void notAnnotatedNegativeBranchIsBAD(@Nullable String s) {
      if (!StaticOneParam.notAnnotated(s)) {
        s.toString();
      }
    }
  }

  class TestNonStaticOneParam {
    private NonStaticOneParam object = new NonStaticOneParam();

    void trueOnNullPositiveBranchIsBAD(@Nullable String s) {
      if (object.trueOnNull(s)) {
        s.toString();
      }
    }

    void trueOnNullNegativeBranchIsOK(@Nullable String s) {
      if (!object.trueOnNull(s)) {
        s.toString();
      }
    }

    void falseOnNullPositiveBranchIsOK(@Nullable String s) {
      if (object.falseOnNull(s)) {
        s.toString();
      }
    }

    void falseOnNullNegativeBranchIsBAD(@Nullable String s) {
      if (!object.falseOnNull(s)) {
        s.toString();
      }
    }

    void notAnnotatedPositiveBranchIsBAD(@Nullable String s) {
      if (object.notAnnotated(s)) {
        s.toString();
      }
    }

    void notAnnotatedNegativeBranchIsBAD(@Nullable String s) {
      if (!object.notAnnotated(s)) {
        s.toString();
      }
    }
  }

  class TestNonStaticSeveralParams {
    private NonStaticSeveralParams object = new NonStaticSeveralParams();

    void trueOnNullPositiveBranchIsBAD(@Nullable String s1, @Nullable String s2) {
      if (object.trueOnNull(s1, 1, "123", s2)) {
        s1.toString();
        s2.toString();
      }
    }

    void trueOnNullNegativeBranchIsOK(@Nullable String s1, @Nullable String s2) {
      if (!object.trueOnNull(s1, 1, "123", s2)) {
        s1.toString();
        s2.toString();
      }
    }

    void falseOnNullPositiveBranchIsOK(@Nullable String s1, @Nullable String s2) {
      if (object.falseOnNull(s1, 1, "123", s2)) {
        s1.toString();
        s2.toString();
      }
    }

    void falseOnNullNegativeBranchIsBAD(@Nullable String s1, @Nullable String s2) {
      if (!object.falseOnNull(s1, 1, "123", s2)) {
        s1.toString();
        s2.toString();
      }
    }

    void notAnnotatedPositiveBranchIsBAD(@Nullable String s1, @Nullable String s2) {
      if (object.notAnnotated(s1, 1, "123", s2)) {
        s1.toString();
        s2.toString();
      }
    }

    void notAnnotatedNegativeBranchIsBAD(@Nullable String s1, @Nullable String s2) {
      if (!object.notAnnotated(s1, 1, "123", s2)) {
        s1.toString();
        s2.toString();
      }
    }
  }

  class TestModels {

    void testModelledTrueOnNull(@Nullable String s) {
      // TextUtils.isEmpty is modelled as TrueOnNull
      if (!TextUtils.isEmpty(s)) {
        s.toString(); // OK: if we are here, we know that `s` is not null
      }

      // Strings.isNullOrEmpty is modelled as TrueOnNull
      if (!Strings.isNullOrEmpty(s)) {
        s.toString(); // OK: if we are here, we know that `s` is not null
      }
    }
  }

  // nullsafe should assume Object.equals() and all overrides return false on `null` argument.
  static class TestEqualsIsFalseOnNull {
    // An example of class that overrides equals().
    static class SomeObject {
      private int mContent;

      SomeObject(int content) {
        mContent = content;
      }

      @Override
      // No nullsafe warnings are expected
      public boolean equals(@Nullable Object src) {
        if (!super.equals(src)) {
          return false;
        }
        // at this point src should be a non-nullable: super.equals() would return false otherwise.
        src.toString(); // should be OK to dereference now
        if (!(src instanceof SomeObject)) {
          return false;
        }
        SomeObject asSomeObject = (SomeObject) src;
        return mContent == asSomeObject.mContent;
      }
    }

    private void testObjectEqualsIsFalseOnNull(Object x, @Nullable Object y) {
      if (!x.equals(y)) {
        return;
      }

      // OK to dereference
      y.toString();
    }

    private void testOverrideEqualsIsFalseOnNull(SomeObject x, @Nullable Object y) {
      if (!x.equals(y)) {
        return;
      }

      // OK to dereference even that SomeObject.equals() is not annotated as @FalseOnNull
      y.toString();
    }
  }

  // this is a common enough pattern to be tested separately
  class EarlyReturn {

    void testEarlyReturn(@Nullable CharSequence s1, @Nullable CharSequence s2) {
      if (StaticOneParam.trueOnNull(s1) || StaticOneParam.notAnnotated(s2)) {
        return;
      }

      s1.toString(); // OK: if `s1` was null, we would no rech this point
      s2.toString(); // BAD: no reason for `s2` to become non-nullable
    }
  }
}
