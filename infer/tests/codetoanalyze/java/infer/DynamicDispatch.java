/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

public class DynamicDispatch {

   static interface Interface {
    public Object foo();
  }

  static class Impl implements Interface {
    @Override public Object foo() {
      return null;
    }
  }

  static void interfaceShouldNotCauseFalseNegativeEasy() {
    Interface i = new Impl();
    // should be a warning since Impl's implementation of foo returns null
    i.foo().toString();
  }

  static void interfaceShouldNotCauseFalseNegativeHard(Interface i) {
    i.foo().toString();
  }

  static void callWithBadImplementation(Impl impl) {
    interfaceShouldNotCauseFalseNegativeHard(impl);
  }

  static class Supertype {
    Object foo() {
      return new Object();
    }

    Object bar() {
      return null;
    }
  }

  static class Subtype extends Supertype {
    @Override Object foo() {
      return null;
    }

    @Override Object bar() {
      return new Object();
    }
  }

  static void dynamicDispatchShouldNotCauseFalseNegativeEasy() {
    Supertype o = new Subtype();
    // should report a warning because we know the dynamic type of o is Subtype
    o.foo().toString();
  }

  static void dynamicDispatchShouldNotCauseFalsePositiveEasy() {
    Supertype o = new Subtype();
    // should not report a warning because we know the dynamic type of o is Subtype
    o.bar().toString();
  }

  // TODO: this test currently fails, but will pass with handling of dynamic dispatch
  static void dynamicDispatchShouldNotCauseFalseNegativeHardTODO(Supertype o) {
    // should report a warning because Subtype's implementation of foo() can return null;
    o.foo().toString();
  }

}
