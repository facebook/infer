/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import com.facebook.infer.annotation.Nullsafe;
import com.facebook.infer.annotation.NullsafeStrict;
import javax.annotation.Nullable;

public class NullsafeMode {
  abstract class VariousMethods {
    public String returnVal() {
      return "OK";
    }

    @Nullable
    public String returnNull() {
      return null;
    }
  }

  class NonNullsafe extends VariousMethods {
    String OK_passUncheckedToLocal(String arg) {
      return new TrustAllNullsafe().acceptVal(arg);
    }

    String OK_passUncheckedToStrictMode(String arg) {
      return new NullsafeWithStrictMode().acceptVal(arg);
    }

    String OK_passUncheckedToStrict(String arg) {
      return new StrictNullsafe().acceptVal(arg);
    }
  }

  class AnotherNonNullsafe extends VariousMethods {}

  @Nullsafe(Nullsafe.Mode.LOCAL)
  class TrustAllNullsafe extends VariousMethods {
    public String acceptVal(String arg) {
      return arg;
    }

    String OK_returnFromAnyNonNullsafe() {
      String a = new NonNullsafe().returnVal();
      String b = new AnotherNonNullsafe().returnVal();
      return a.concat(b);
    }

    String BAD_returnNullFromNonNulsafe() {
      return (new NonNullsafe()).returnNull();
    }

    String OK_passLocalToStrictMode(String arg) {
      return new NullsafeWithStrictMode().acceptVal(arg);
    }

    String OK_passLocalToStrict(String arg) {
      return new StrictNullsafe().acceptVal(arg);
    }
  }

  @Nullsafe(value = Nullsafe.Mode.LOCAL, trustOnly = @Nullsafe.TrustList({NonNullsafe.class}))
  class TrustSomeNullsafe extends VariousMethods {
    @Override
    public String returnVal() {
      return "OK";
    }

    String FP_OK_returnFromNonNullsafe() {
      return new NonNullsafe().returnVal();
    }

    String BAD_returnFromAnotherNonNullsafe() {
      return new AnotherNonNullsafe().returnVal();
    }

    @Nullable
    String OK_returnFromAnotherNonNullsafeAsNullable() {
      return new AnotherNonNullsafe().returnVal();
    }

    String BAD_returnNullFromNonNulsafe() {
      return new NonNullsafe().returnNull();
    }
  }

  @Nullsafe(value = Nullsafe.Mode.LOCAL, trustOnly = @Nullsafe.TrustList({}))
  class TrustNoneNullsafe extends VariousMethods {
    String BAD_returnFromNonNullsafe() {
      return new NonNullsafe().returnVal();
    }

    String OK_returnFromNullsafe() {
      return new TrustSomeNullsafe().returnVal();
    }
  }

  @Nullsafe(Nullsafe.Mode.STRICT)
  class NullsafeWithStrictMode extends VariousMethods {
    @Override
    public String returnVal() {
      return "OK";
    }

    public String acceptVal(String arg) {
      return arg;
    }

    String BAD_returnFromNonStrict() {
      return new TrustNoneNullsafe().returnVal();
    }

    String OK_returnFromNullsafeStrict() {
      return new StrictNullsafe().returnVal();
    }
  }

  @NullsafeStrict
  class StrictNullsafe extends VariousMethods {
    @Override
    public String returnVal() {
      return "OK";
    }

    public String acceptVal(String arg) {
      return arg;
    }

    String BAD_returnFromNonNullsafe() {
      return new NonNullsafe().returnVal();
    }

    String OK_returnFromNullsafeWithStrictMode() {
      return new NullsafeWithStrictMode().returnVal();
    }
  }
}
