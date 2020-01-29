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

  class NonNullsafe extends VariousMethods {}

  class AnotherNonNullsafe extends VariousMethods {}

  @Nullsafe(Nullsafe.Mode.LOCAL)
  class TrustAllNullsafe extends VariousMethods {
    String OK_returnFromAnyNonNullsafe() {
      String a = new NonNullsafe().returnVal();
      String b = new AnotherNonNullsafe().returnVal();
      return a.concat(b);
    }

    String BAD_returnNullFromNonNulsafe() {
      return (new NonNullsafe()).returnNull();
    }
  }

  @Nullsafe(value = Nullsafe.Mode.LOCAL, trustOnly = @Nullsafe.TrustList({NonNullsafe.class}))
  class TrustSomeNullsafe extends VariousMethods {
    String OK_returnFromNonNullsafe() {
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
    String BAD_returnFromNonStrict() {
      return new TrustNoneNullsafe().returnVal();
    }

    String OK_returnFromNullsafeStrict() {
      return new StrictNullsafe().returnVal();
    }
  }

  @NullsafeStrict
  class StrictNullsafe extends VariousMethods {
    String BAD_returnFromNonNullsafe() {
      return new NonNullsafe().returnVal();
    }

    String OK_returnFromNullsafeWithStrictMode() {
      return new NullsafeWithStrictMode().returnVal();
    }
  }
}
