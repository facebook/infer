/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.eradicate;

import com.facebook.infer.annotation.Assertions;
import com.facebook.infer.annotation.Present;
import com.google.common.base.Optional;

public class PresentTest {

  void argPresent(@Present Optional<String> present, Optional<String> absent) {}

  void testPresent(@Present Optional<String> present, Optional<String> absent) {
    argPresent(present, absent); // OK
    argPresent(present, present); // OK
    argPresent(present, absent); // OK
    argPresent(absent, absent); // Bad
  }

  class TestPresentAnnotationBasic {
    void testBasicConditional(Optional<String> o) {
      if (o.isPresent()) {
        o.get();
      }
    }

    Optional<String> absent = Optional.absent();
    @Present Optional<String> present = Optional.of("abc");

    @Present
    Optional<String> returnPresent() {
      if (absent.isPresent()) {
        return absent;
      } else return Optional.of("abc");
    }

    @Present
    Optional<String> returnPresentBad() {
      absent.get(); // Bad: get is unsafe
      return absent; // Bad: should return present
    }

    void expectPresent(@Present Optional<String> x) {}

    void bar() {
      expectPresent(present);
      String s;
      s = returnPresent().get();
      s = present.get();

      Assertions.assertCondition(absent.isPresent());
      expectPresent(absent);
    }

    void testOptionalAbsent() {
      expectPresent(Optional.absent()); // Bad
    }
  }

  class TestPresentFieldOfInnerClass {
    class D {
      @SuppressFieldNotInitialized Optional<String> s;
    }

    class D1 {
      // Different bytecode generated when the field is private
      @SuppressFieldNotInitialized private Optional<String> s;
    }

    void testD(D d) {
      if (d.s.isPresent()) {
        d.s.get();
      }
    }

    void testD1(D1 d1) {
      if (d1.s.isPresent()) {
        d1.s.get();
      }
    }

    void testD1Condition(D1 d1) {
      Assertions.assertCondition(d1.s.isPresent());
      d1.s.get();
    }
  }
}
