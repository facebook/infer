/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

/**
 * WARNING! These methods are for testing the taint analysis only! Don't use them in models or in
 * real code.
 */
public class InferTaintSources {

  @SensitiveSourceMarker
  static class Sources {

    static Object source1() {
      return new Object();
    }

    static Object source2() {
      return new Object();
    }
  }

  static class NotSources {

    static Object notSource() {
      return new Object();
    }

    static Object sourceButNotReally() {
      return new Object();
    }
  }

  static class RegexSources {

    static Object source1() {
      return new Object();
    }

    static Object source2() {
      return new Object();
    }

    static Object notSource() {
      return new Object();
    }
  }

  @SensitiveSourceMarker2
  static class RegexAndAnnotationSources {

    static Object source1() {
      return new Object();
    }

    static Object source2() {
      return new Object();
    }

    static Object notSource() {
      return new Object();
    }
  }

  static class RegexAndAnnotationNotSources {

    static Object sourceButNotReally() {
      return new Object();
    }

    static Object notSource() {
      return new Object();
    }
  }
}
