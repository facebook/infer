/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import javax.annotation.Nullable;

public class NestedFieldAccess {

  class C {
    @Nullable String s;
  }

  class CC {
    @Nullable C c;
  }

  class CCC {
    @Nullable CC cc;
  }

  /**
   * Tests nullability check patterns for f1.f2.f3, when all components in the chain are nullable.
   * (it should require checking of all components in the chain)
   */
  public class TestNullableChains {
    @Nullable String s;
    C myc;

    TestNullableChains() {
      myc = new C();
    }

    void field_AccessAfterNullCheckIsOK() {
      if (s != null) {
        int n = s.length();
      }
    }

    void field_AccessWithoutNullCheckIsBad() {
      int n = s.length();
    }

    void nestedField_AccessAfterNullCheckIsOK() {
      if (myc.s != null) {
        int n = myc.s.length();
      }
    }

    void nestedField_AccessWithoutNullCheckIsBad() {
      int n = myc.s.length();
    }

    void param_AccessAfterNullCheckIsOK(C c) {
      if (c.s != null) {
        int n = c.s.length();
      }
    }

    void param_AccessWithoutNullCheckIsBad(C c) {
      int n = c.s.length();
    }

    void local_AccessAfterNullCheckIsOK() {
      C c = new C();
      if (c.s != null) {
        int n = c.s.length();
      }
    }

    void local_AccessWithoutNullCheckIsBad() {
      C c = new C();
      int n = c.s.length();
    }

    void deep_AccessWithNullCheckIsOK(CC cc) {
      if (cc.c != null && cc.c.s != null) {
        int n = cc.c.s.length();
      }
    }

    void deep_AccessWithoutNullCheckIsBad(CC cc) {
      if (cc.c != null /* && cc.c.s != null */) {
        int n = cc.c.s.length();
      }
    }

    void veryDeep_AccessWithNullCheckIsOK(CCC ccc) {
      if (ccc.cc != null && ccc.cc.c != null && ccc.cc.c.s != null) {
        int n = ccc.cc.c.s.length();
      }
    }

    void veryDeep_AccessWithoutNullCheckIsBad(CCC ccc) {
      if (ccc.cc != null && ccc.cc.c != null /* && ccc.cc.c.s != null */) {
        int n = ccc.cc.c.s.length();
      }
    }

    void veryDeep_AccessViaOrEarlyReturnIsOK(@Nullable CCC ccc) {
      if (ccc == null || ccc.cc == null || ccc.cc.c == null || ccc.cc.c.s == null) {
      } else {
        int n = ccc.cc.c.s.length();
      }
    }

    void veryDeep_IncompleteAccessViaOrEarlyReturnIsBad(@Nullable CCC ccc) {
      if (ccc == null || ccc.cc == null || ccc.cc.c == null /*|| ccc.cc.c.s == null*/) {
      } else {
        int n = ccc.cc.c.s.length();
      }
    }
  }

  /**
   * Tests nullability patterns for chains a().a().a().a().nullable(). Basically nullsafe needs to
   * realize that objects returned by a().a() and a().a().a() are different so it should not learn
   * anything about the nullability of one based on evidence about the other one.
   */
  class TestFunctionsIdempotent {
    @Nullable String s;
    String dontAssignNull = "";

    @Nullable
    String nullable(int n) {
      return s;
    }

    TestFunctionsIdempotent getSelf() {
      return this;
    }

    void chainOf0VsChainOf0IsOK() {
      if (nullable(3) != null) {
        dontAssignNull = nullable(3);
      }
    }

    void chainOf0VsChainOf0ParamsMismatchIsBad() {
      if (nullable(3) != null) {
        dontAssignNull = nullable(4);
      }
    }

    void otherObjVsItselfIsOK(TestFunctionsIdempotent otherObj) {
      if (otherObj.nullable(3) != null) {
        dontAssignNull = otherObj.nullable(3);
      }
    }

    void otherObjVsItselfIsOKParamsMismatchIsBAD(TestFunctionsIdempotent otherObj) {
      if (otherObj.nullable(3) != null) {
        dontAssignNull = otherObj.nullable(4);
      }
    }

    void selfVsOtherObjectIsBAD(TestFunctionsIdempotent otherObj) {
      if (otherObj.nullable(3) != null) {
        dontAssignNull = nullable(3);
      }
    }

    void chainOf0VsChainOf1IsBad() {
      if (nullable(3) != null) {
        dontAssignNull = getSelf().nullable(3);
      }
    }

    void chainOf1VsChainOf0IsBad() {
      if (getSelf().nullable(3) != null) {
        dontAssignNull = nullable(3);
      }
    }

    void chainOf1VsChainOf1IsOK() {
      if (getSelf().nullable(3) != null) {
        dontAssignNull = getSelf().nullable(3);
      }
    }

    void chainOf1VsChainOf1ParamMismatchIsBad() {
      if (getSelf().nullable(3) != null) {
        dontAssignNull = getSelf().nullable(4);
      }
    }

    void chainOf2VsChainOf2IsOK() {
      if (getSelf().getSelf().nullable(3) != null) {
        dontAssignNull = getSelf().getSelf().nullable(3);
      }
    }

    void chainOf1VsChainOf2IsBad() {
      if (getSelf().nullable(3) != null) {
        dontAssignNull = getSelf().getSelf().nullable(3);
      }
    }

    void chainOf2VsChainOf1IsBad() {
      if (getSelf().getSelf().nullable(3) != null) {
        dontAssignNull = getSelf().nullable(3);
      }
    }
  }
}
